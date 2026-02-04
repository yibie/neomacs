//! Async image loading and caching for wgpu renderer
//!
//! Provides non-blocking image loading:
//! - Fast dimension query (header only)
//! - Background decoding in thread pool
//! - GPU texture upload when ready
//! - LRU cache with memory limits

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufReader, Read, Seek, SeekFrom};
use std::path::Path;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{mpsc, Arc, Mutex};
use std::thread;

/// Maximum texture dimension (width or height)
const MAX_TEXTURE_SIZE: u32 = 4096;

/// Maximum total cache memory in bytes (64MB)
const MAX_CACHE_MEMORY: usize = 64 * 1024 * 1024;

/// Number of decoder threads
const DECODER_THREADS: usize = 4;

/// Image loading state
#[derive(Debug, Clone)]
pub enum ImageState {
    /// Queued for loading
    Pending,
    /// Currently being decoded
    Decoding,
    /// Ready with texture
    Ready,
    /// Failed to load
    Failed(String),
}

/// Cached image with GPU texture
pub struct CachedImage {
    pub texture: wgpu::Texture,
    pub view: wgpu::TextureView,
    pub bind_group: wgpu::BindGroup,
    pub width: u32,
    pub height: u32,
    /// Memory size in bytes
    pub memory_size: usize,
}

/// Decoded image data waiting for GPU upload
struct DecodedImage {
    id: u32,
    width: u32,
    height: u32,
    data: Vec<u8>, // RGBA
}

/// Image dimensions (from header)
#[derive(Debug, Clone, Copy)]
pub struct ImageDimensions {
    pub width: u32,
    pub height: u32,
}

/// Async image cache
pub struct ImageCache {
    /// Next image ID
    next_id: AtomicU32,
    /// Cached textures: id -> CachedImage
    textures: HashMap<u32, CachedImage>,
    /// Image states: id -> state
    states: HashMap<u32, ImageState>,
    /// Pending dimensions (before texture is ready)
    pending_dimensions: HashMap<u32, ImageDimensions>,
    /// Channel to receive decoded images
    decoded_rx: mpsc::Receiver<DecodedImage>,
    /// Channel to send decode requests
    decode_tx: mpsc::Sender<DecodeRequest>,
    /// Bind group layout for image textures
    bind_group_layout: wgpu::BindGroupLayout,
    /// Sampler for image textures
    sampler: wgpu::Sampler,
    /// Total cached memory
    total_memory: usize,
}

/// Request to decode an image
struct DecodeRequest {
    id: u32,
    source: ImageSource,
    max_width: u32,
    max_height: u32,
}

/// Image source
enum ImageSource {
    File(String),
    Data(Vec<u8>),
}

impl ImageCache {
    /// Create a new image cache
    pub fn new(device: &wgpu::Device) -> Self {
        // Create bind group layout for image textures
        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("Image Bind Group Layout"),
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        sample_type: wgpu::TextureSampleType::Float { filterable: true },
                        view_dimension: wgpu::TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        });

        // Create sampler
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("Image Sampler"),
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            address_mode_w: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            mipmap_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });

        // Create channels for async decoding
        let (decode_tx, decode_rx) = mpsc::channel::<DecodeRequest>();
        let (decoded_tx, decoded_rx) = mpsc::channel::<DecodedImage>();

        // Wrap receiver in Arc<Mutex> for sharing across threads
        let decode_rx = Arc::new(Mutex::new(decode_rx));

        // Spawn decoder thread pool
        for i in 0..DECODER_THREADS {
            let rx = Arc::clone(&decode_rx);
            let tx = decoded_tx.clone();
            thread::spawn(move || {
                Self::decoder_thread_pooled(i, rx, tx);
            });
        }

        Self {
            next_id: AtomicU32::new(1),
            textures: HashMap::new(),
            states: HashMap::new(),
            pending_dimensions: HashMap::new(),
            decoded_rx,
            decode_tx,
            bind_group_layout,
            sampler,
            total_memory: 0,
        }
    }

    /// Background decoder thread (pooled version)
    fn decoder_thread_pooled(
        thread_id: usize,
        rx: Arc<Mutex<mpsc::Receiver<DecodeRequest>>>,
        tx: mpsc::Sender<DecodedImage>,
    ) {
        log::debug!("Decoder thread {} started", thread_id);
        loop {
            // Lock, receive, unlock immediately to allow other threads to grab work
            let request = {
                let guard = rx.lock().unwrap();
                guard.recv()
            };

            match request {
                Ok(request) => {
                    log::debug!("Thread {} decoding image {}", thread_id, request.id);
                    let result = match request.source {
                        ImageSource::File(path) => {
                            Self::decode_file(&path, request.max_width, request.max_height)
                        }
                        ImageSource::Data(data) => {
                            Self::decode_data(&data, request.max_width, request.max_height)
                        }
                    };

                    if let Some((width, height, data)) = result {
                        let _ = tx.send(DecodedImage {
                            id: request.id,
                            width,
                            height,
                            data,
                        });
                    }
                }
                Err(_) => {
                    // Channel closed, exit thread
                    log::debug!("Decoder thread {} exiting", thread_id);
                    break;
                }
            }
        }
    }

    /// Decode image file with size constraints
    fn decode_file(path: &str, max_width: u32, max_height: u32) -> Option<(u32, u32, Vec<u8>)> {
        let img = image::open(path).ok()?;
        Self::process_image(img, max_width, max_height)
    }

    /// Decode image data with size constraints
    fn decode_data(data: &[u8], max_width: u32, max_height: u32) -> Option<(u32, u32, Vec<u8>)> {
        let img = image::load_from_memory(data).ok()?;
        Self::process_image(img, max_width, max_height)
    }

    /// Process decoded image: resize if needed, convert to RGBA
    fn process_image(
        img: image::DynamicImage,
        max_width: u32,
        max_height: u32,
    ) -> Option<(u32, u32, Vec<u8>)> {
        let (mut width, mut height) = (img.width(), img.height());

        // Apply max constraints
        let mw = if max_width > 0 { max_width } else { MAX_TEXTURE_SIZE };
        let mh = if max_height > 0 { max_height } else { MAX_TEXTURE_SIZE };

        // Scale down if needed (preserve aspect ratio)
        if width > mw || height > mh {
            let ratio = (width as f64 / height as f64).min(mw as f64 / mh as f64);
            if width > mw {
                width = mw;
                height = (mw as f64 / ratio) as u32;
            }
            if height > mh {
                height = mh;
                width = (mh as f64 * ratio) as u32;
            }
        }

        // Resize if dimensions changed
        let img = if width != img.width() || height != img.height() {
            img.resize_exact(width, height, image::imageops::FilterType::Lanczos3)
        } else {
            img
        };

        // Convert to RGBA
        let rgba = img.to_rgba8();
        Some((width, height, rgba.into_raw()))
    }

    /// Get bind group layout
    pub fn bind_group_layout(&self) -> &wgpu::BindGroupLayout {
        &self.bind_group_layout
    }

    /// Query image file dimensions (fast - reads header only)
    pub fn query_file_dimensions(path: &str) -> Option<ImageDimensions> {
        let file = File::open(path).ok()?;
        let reader = BufReader::new(file);

        // Use image crate's dimension reader (reads header only)
        let (width, height) = image::io::Reader::new(reader)
            .with_guessed_format()
            .ok()?
            .into_dimensions()
            .ok()?;

        Some(ImageDimensions { width, height })
    }

    /// Query image data dimensions (fast - reads header only)
    pub fn query_data_dimensions(data: &[u8]) -> Option<ImageDimensions> {
        let cursor = std::io::Cursor::new(data);
        let (width, height) = image::io::Reader::new(BufReader::new(cursor))
            .with_guessed_format()
            .ok()?
            .into_dimensions()
            .ok()?;

        Some(ImageDimensions { width, height })
    }

    /// Load image from file (async)
    /// Returns image ID immediately, texture loads in background
    pub fn load_file(&mut self, path: &str, max_width: u32, max_height: u32) -> u32 {
        let id = self.next_id.fetch_add(1, Ordering::SeqCst);

        // Query dimensions first (fast)
        if let Some(dims) = Self::query_file_dimensions(path) {
            // Apply max constraints to dimensions
            let (w, h) = Self::constrain_dimensions(dims.width, dims.height, max_width, max_height);
            self.pending_dimensions.insert(id, ImageDimensions { width: w, height: h });
        }

        // Queue for async decode
        self.states.insert(id, ImageState::Pending);
        let _ = self.decode_tx.send(DecodeRequest {
            id,
            source: ImageSource::File(path.to_string()),
            max_width,
            max_height,
        });

        id
    }

    /// Load image from data (async)
    pub fn load_data(&mut self, data: &[u8], max_width: u32, max_height: u32) -> u32 {
        let id = self.next_id.fetch_add(1, Ordering::SeqCst);

        // Query dimensions first (fast)
        if let Some(dims) = Self::query_data_dimensions(data) {
            let (w, h) = Self::constrain_dimensions(dims.width, dims.height, max_width, max_height);
            self.pending_dimensions.insert(id, ImageDimensions { width: w, height: h });
        }

        // Queue for async decode
        self.states.insert(id, ImageState::Pending);
        let _ = self.decode_tx.send(DecodeRequest {
            id,
            source: ImageSource::Data(data.to_vec()),
            max_width,
            max_height,
        });

        id
    }

    /// Constrain dimensions to max values while preserving aspect ratio
    fn constrain_dimensions(width: u32, height: u32, max_width: u32, max_height: u32) -> (u32, u32) {
        let mut w = width;
        let mut h = height;

        let mw = if max_width > 0 { max_width } else { MAX_TEXTURE_SIZE };
        let mh = if max_height > 0 { max_height } else { MAX_TEXTURE_SIZE };

        if w > mw {
            h = (h as f64 * mw as f64 / w as f64) as u32;
            w = mw;
        }
        if h > mh {
            w = (w as f64 * mh as f64 / h as f64) as u32;
            h = mh;
        }

        (w.max(1), h.max(1))
    }

    /// Process pending decoded images (call each frame)
    pub fn process_pending(&mut self, device: &wgpu::Device, queue: &wgpu::Queue) {
        // Drain decoded images from channel
        while let Ok(decoded) = self.decoded_rx.try_recv() {
            self.upload_texture(device, queue, decoded);
        }

        // Evict if over memory limit
        self.evict_if_needed();
    }

    /// Upload decoded image to GPU texture
    fn upload_texture(&mut self, device: &wgpu::Device, queue: &wgpu::Queue, decoded: DecodedImage) {
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("Image Texture"),
            size: wgpu::Extent3d {
                width: decoded.width,
                height: decoded.height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });

        queue.write_texture(
            wgpu::ImageCopyTexture {
                texture: &texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            &decoded.data,
            wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(decoded.width * 4),
                rows_per_image: Some(decoded.height),
            },
            wgpu::Extent3d {
                width: decoded.width,
                height: decoded.height,
                depth_or_array_layers: 1,
            },
        );

        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());

        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Image Bind Group"),
            layout: &self.bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&self.sampler),
                },
            ],
        });

        let memory_size = (decoded.width * decoded.height * 4) as usize;
        self.total_memory += memory_size;

        self.textures.insert(decoded.id, CachedImage {
            texture,
            view,
            bind_group,
            width: decoded.width,
            height: decoded.height,
            memory_size,
        });

        self.states.insert(decoded.id, ImageState::Ready);
        self.pending_dimensions.remove(&decoded.id);

        log::debug!("Uploaded image {} ({}x{}, {}KB)",
                   decoded.id, decoded.width, decoded.height, memory_size / 1024);
    }

    /// Evict old textures if over memory limit
    fn evict_if_needed(&mut self) {
        // Simple strategy: remove oldest entries until under limit
        while self.total_memory > MAX_CACHE_MEMORY && !self.textures.is_empty() {
            // Find smallest ID (oldest)
            if let Some(&id) = self.textures.keys().min() {
                if let Some(cached) = self.textures.remove(&id) {
                    self.total_memory -= cached.memory_size;
                    self.states.remove(&id);
                    log::debug!("Evicted image {} to free {}KB", id, cached.memory_size / 1024);
                }
            }
        }
    }

    /// Get cached image if ready
    pub fn get(&self, id: u32) -> Option<&CachedImage> {
        self.textures.get(&id)
    }

    /// Get image dimensions (pending or loaded)
    pub fn get_dimensions(&self, id: u32) -> Option<ImageDimensions> {
        // Check loaded textures first
        if let Some(cached) = self.textures.get(&id) {
            return Some(ImageDimensions {
                width: cached.width,
                height: cached.height,
            });
        }
        // Check pending dimensions
        self.pending_dimensions.get(&id).copied()
    }

    /// Get image state
    pub fn get_state(&self, id: u32) -> Option<&ImageState> {
        self.states.get(&id)
    }

    /// Check if image is ready
    pub fn is_ready(&self, id: u32) -> bool {
        matches!(self.states.get(&id), Some(ImageState::Ready))
    }

    /// Free an image from cache
    pub fn free(&mut self, id: u32) {
        if let Some(cached) = self.textures.remove(&id) {
            self.total_memory -= cached.memory_size;
        }
        self.states.remove(&id);
        self.pending_dimensions.remove(&id);
    }

    /// Clear entire cache
    pub fn clear(&mut self) {
        self.textures.clear();
        self.states.clear();
        self.pending_dimensions.clear();
        self.total_memory = 0;
    }
}
