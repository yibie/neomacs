//! Winit window and event handling backend.

use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::Arc;
use std::time::Duration;

use winit::application::ApplicationHandler;
use winit::dpi::{LogicalSize, PhysicalSize};
use winit::event::{ElementState, Event, KeyEvent, MouseButton, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop, EventLoopProxy};
use winit::keyboard::{Key, NamedKey};
use winit::platform::pump_events::EventLoopExtPumpEvents;
use winit::window::{Window, WindowId};

use super::events::*;

use crate::backend::DisplayBackend;
use crate::core::error::{DisplayError, DisplayResult};
use crate::core::scene::Scene;

use super::window_state::WindowState;
use super::WgpuRenderer;

/// Custom user events for the event loop.
#[derive(Debug, Clone)]
pub enum UserEvent {
    /// Request a redraw of the window.
    Redraw,
    /// A WebKit frame is ready for the given view ID.
    WebKitFrame(u32),
    /// A video frame is ready for the given video ID.
    VideoFrame(u32),
    /// The scene has been updated and needs re-rendering.
    SceneUpdated,
}

/// Callbacks for handling window events.
#[derive(Default)]
pub struct Callbacks {
    /// Called when a keyboard event occurs.
    pub on_key: Option<Box<dyn Fn(KeyEvent) + Send>>,
    /// Called when a mouse button event occurs.
    pub on_mouse_button: Option<Box<dyn Fn(MouseButton, ElementState, f64, f64) + Send>>,
    /// Called when the mouse cursor moves.
    pub on_mouse_move: Option<Box<dyn Fn(f64, f64) + Send>>,
    /// Called when the window is resized.
    pub on_resize: Option<Box<dyn Fn(u32, u32) + Send>>,
    /// Called when the window close is requested.
    pub on_close: Option<Box<dyn Fn() + Send>>,
}

/// Winit-based window and input backend.
pub struct WinitBackend {
    /// Whether the backend has been initialized.
    initialized: bool,
    /// Current window width.
    width: u32,
    /// Current window height.
    height: u32,
    /// The winit window.
    window: Option<Arc<Window>>,
    /// The wgpu renderer.
    renderer: Option<WgpuRenderer>,
    /// The wgpu surface for rendering.
    surface: Option<wgpu::Surface<'static>>,
    /// Surface configuration.
    surface_config: Option<wgpu::SurfaceConfiguration>,
    /// Event loop proxy for sending custom events.
    event_loop_proxy: Option<EventLoopProxy<UserEvent>>,
    /// The current scene to render.
    scene: Scene,
    /// Callbacks for handling events.
    callbacks: Callbacks,
    /// Current cursor position.
    cursor_position: (f64, f64),
    /// The wgpu instance for creating surfaces.
    instance: Option<wgpu::Instance>,
    /// The wgpu device for GPU operations.
    device: Option<Arc<wgpu::Device>>,
    /// The surface format for rendering.
    surface_format: wgpu::TextureFormat,
    /// The active event loop reference (only valid during event handling).
    event_loop: Option<*const ActiveEventLoop>,
    /// Window states for multi-window support.
    windows: HashMap<u32, WindowState>,
    /// Next window ID to assign.
    next_window_id: u32,
    /// Queue of input events for polling.
    event_queue: VecDeque<NeomacsInputEvent>,
    /// Current modifier key state.
    current_modifiers: u32,
    /// Current mouse position.
    mouse_position: (i32, i32),
}

impl WinitBackend {
    /// Create a new WinitBackend.
    pub fn new() -> Self {
        Self {
            initialized: false,
            width: 800,
            height: 600,
            window: None,
            renderer: None,
            surface: None,
            surface_config: None,
            event_loop_proxy: None,
            scene: Scene::new(800.0, 600.0),
            callbacks: Callbacks::default(),
            cursor_position: (0.0, 0.0),
            instance: None,
            device: None,
            surface_format: wgpu::TextureFormat::Bgra8UnormSrgb,
            event_loop: None,
            windows: HashMap::new(),
            next_window_id: 1,
            event_queue: VecDeque::new(),
            current_modifiers: 0,
            mouse_position: (0, 0),
        }
    }

    /// Create the main window and initialize the wgpu surface.
    ///
    /// This should be called from the event loop's `resumed` event.
    pub fn init_main_window(&mut self, event_loop: &ActiveEventLoop) -> DisplayResult<()> {
        let window_attributes = Window::default_attributes()
            .with_title("Neomacs")
            .with_inner_size(LogicalSize::new(self.width, self.height));

        let window = event_loop
            .create_window(window_attributes)
            .map_err(|e| DisplayError::InitFailed(format!("Failed to create window: {}", e)))?;

        let window = Arc::new(window);

        // Get the actual size
        let size = window.inner_size();
        self.width = size.width;
        self.height = size.height;

        // Create wgpu instance
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends: wgpu::Backends::all(),
            ..Default::default()
        });

        // Create surface - we need to use unsafe to create a surface from the window
        // SAFETY: The window is valid and will outlive the surface because we store
        // an Arc<Window> in self.window.
        let surface = instance
            .create_surface(window.clone())
            .map_err(|e| DisplayError::InitFailed(format!("Failed to create surface: {}", e)))?;

        // Request adapter
        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }))
        .ok_or_else(|| DisplayError::InitFailed("Failed to find a suitable GPU adapter".to_string()))?;

        // Request device and queue
        let (device, queue) = pollster::block_on(adapter.request_device(
            &wgpu::DeviceDescriptor {
                label: Some("Neomacs Device"),
                required_features: wgpu::Features::empty(),
                required_limits: wgpu::Limits::default(),
                memory_hints: Default::default(),
            },
            None,
        ))
        .map_err(|e| DisplayError::InitFailed(format!("Failed to create device: {}", e)))?;

        let device = Arc::new(device);
        let queue = Arc::new(queue);

        // Configure surface
        let caps = surface.get_capabilities(&adapter);
        let format = caps
            .formats
            .iter()
            .copied()
            .find(|f| f.is_srgb())
            .unwrap_or(caps.formats[0]);

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format,
            width: self.width,
            height: self.height,
            present_mode: wgpu::PresentMode::Fifo, // VSync
            alpha_mode: caps.alpha_modes[0],
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(&device, &config);

        // Create renderer
        let renderer = WgpuRenderer::new(None, self.width, self.height);

        // Store the wgpu infrastructure for multi-window support
        self.instance = Some(instance);
        self.device = Some(device.clone());
        self.surface_format = format;

        self.window = Some(window);
        self.surface = Some(surface);
        self.surface_config = Some(config);
        self.renderer = Some(renderer);
        self.initialized = true;

        // Update scene dimensions
        self.scene = Scene::new(self.width as f32, self.height as f32);

        log::info!(
            "WinitBackend initialized: {}x{}, format: {:?}",
            self.width,
            self.height,
            format
        );

        Ok(())
    }

    /// Request a redraw of the window.
    pub fn request_redraw(&self) {
        if let Some(window) = &self.window {
            window.request_redraw();
        }
    }

    /// Get the event loop proxy.
    pub fn event_loop_proxy(&self) -> Option<&EventLoopProxy<UserEvent>> {
        self.event_loop_proxy.as_ref()
    }

    /// Set the event loop proxy.
    pub fn set_event_loop_proxy(&mut self, proxy: EventLoopProxy<UserEvent>) {
        self.event_loop_proxy = Some(proxy);
    }

    /// Update the scene to be rendered.
    pub fn update_scene(&mut self, scene: Scene) {
        self.scene = scene;
    }

    /// Perform the actual rendering.
    pub fn do_render(&mut self) -> DisplayResult<()> {
        let surface = match &self.surface {
            Some(s) => s,
            None => return Ok(()),
        };

        let renderer = match &self.renderer {
            Some(r) => r,
            None => return Ok(()),
        };

        // Get the current texture from the surface
        let output = match surface.get_current_texture() {
            Ok(output) => output,
            Err(wgpu::SurfaceError::Lost) => {
                // Reconfigure the surface
                self.handle_resize(PhysicalSize::new(self.width, self.height));
                return Ok(());
            }
            Err(wgpu::SurfaceError::OutOfMemory) => {
                return Err(DisplayError::Render("Out of GPU memory".to_string()));
            }
            Err(e) => {
                log::warn!("Surface error: {:?}", e);
                return Ok(());
            }
        };

        let view = output
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());

        // Render the scene to the view
        renderer.render_to_view(&view, &self.scene);

        // Present the frame
        output.present();

        Ok(())
    }

    /// Handle window resize.
    pub fn handle_resize(&mut self, size: PhysicalSize<u32>) {
        if size.width == 0 || size.height == 0 {
            return;
        }

        self.width = size.width;
        self.height = size.height;

        // Reconfigure the surface
        if let (Some(surface), Some(config)) = (&self.surface, &mut self.surface_config) {
            config.width = size.width;
            config.height = size.height;

            // We need the device to reconfigure
            if let Some(renderer) = &self.renderer {
                surface.configure(renderer.device(), config);
            }
        }

        // Resize the renderer
        if let Some(renderer) = &mut self.renderer {
            renderer.resize(size.width, size.height);
        }

        // Update scene dimensions without discarding content
        self.scene.width = size.width as f32;
        self.scene.height = size.height as f32;

        // Call the resize callback
        if let Some(ref callback) = self.callbacks.on_resize {
            callback(size.width, size.height);
        }
    }

    /// Set the callbacks for handling events.
    pub fn set_callbacks(&mut self, callbacks: Callbacks) {
        self.callbacks = callbacks;
    }

    /// Get the window.
    pub fn window(&self) -> Option<&Arc<Window>> {
        self.window.as_ref()
    }

    /// Create a new window with the specified dimensions and title.
    ///
    /// Returns the window ID if successful, or None if creation failed.
    /// This requires the event loop to be active (event_loop field must be set).
    pub fn create_window(&mut self, width: u32, height: u32, title: &str) -> Option<u32> {
        let event_loop = self.event_loop.as_ref()?;
        let event_loop = unsafe { &**event_loop };

        let window_attrs = winit::window::WindowAttributes::default()
            .with_title(title)
            .with_inner_size(winit::dpi::PhysicalSize::new(width, height));

        let window = Arc::new(event_loop.create_window(window_attrs).ok()?);

        let instance = self.instance.as_ref()?;
        let device = self.device.as_ref()?;

        let surface = instance.create_surface(window.clone()).ok()?;
        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: self.surface_format,
            width,
            height,
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode: wgpu::CompositeAlphaMode::Opaque,
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };
        surface.configure(device, &config);

        let window_id = self.next_window_id;
        self.next_window_id += 1;

        let state = WindowState::new(window, surface, config, width, height);
        self.windows.insert(window_id, state);

        Some(window_id)
    }

    /// Destroy a window by its ID.
    pub fn destroy_window(&mut self, window_id: u32) {
        self.windows.remove(&window_id);
    }

    /// Get a reference to a window state by ID.
    pub fn get_window(&self, window_id: u32) -> Option<&WindowState> {
        self.windows.get(&window_id)
    }

    /// Get a mutable reference to a window state by ID.
    pub fn get_window_mut(&mut self, window_id: u32) -> Option<&mut WindowState> {
        self.windows.get_mut(&window_id)
    }

    /// Poll for input events.
    ///
    /// Note: pump_events requires taking ownership of event_loop temporarily.
    /// This is a complex integration point - for now we return queued events.
    /// The actual implementation needs careful handling of winit's event loop model.
    pub fn poll_events(&mut self) -> Vec<NeomacsInputEvent> {
        let mut events = Vec::new();

        // Process any queued events
        while let Some(ev) = self.event_queue.pop_front() {
            events.push(ev);
        }

        events
    }

    /// Translate a winit key to an X11-compatible keysym.
    fn translate_key(&self, key: &Key) -> u32 {
        match key {
            Key::Character(c) => c.chars().next().unwrap_or('\0') as u32,
            Key::Named(named) => match named {
                NamedKey::Enter => 0xFF0D,
                NamedKey::Tab => 0xFF09,
                NamedKey::Backspace => 0xFF08,
                NamedKey::Escape => 0xFF1B,
                NamedKey::Space => 0x20,
                NamedKey::ArrowUp => 0xFF52,
                NamedKey::ArrowDown => 0xFF54,
                NamedKey::ArrowLeft => 0xFF51,
                NamedKey::ArrowRight => 0xFF53,
                NamedKey::Home => 0xFF50,
                NamedKey::End => 0xFF57,
                NamedKey::PageUp => 0xFF55,
                NamedKey::PageDown => 0xFF56,
                NamedKey::Insert => 0xFF63,
                NamedKey::Delete => 0xFFFF,
                NamedKey::F1 => 0xFFBE,
                NamedKey::F2 => 0xFFBF,
                NamedKey::F3 => 0xFFC0,
                NamedKey::F4 => 0xFFC1,
                NamedKey::F5 => 0xFFC2,
                NamedKey::F6 => 0xFFC3,
                NamedKey::F7 => 0xFFC4,
                NamedKey::F8 => 0xFFC5,
                NamedKey::F9 => 0xFFC6,
                NamedKey::F10 => 0xFFC7,
                NamedKey::F11 => 0xFFC8,
                NamedKey::F12 => 0xFFC9,
                _ => 0,
            },
            _ => 0,
        }
    }
}

impl Default for WinitBackend {
    fn default() -> Self {
        Self::new()
    }
}

impl DisplayBackend for WinitBackend {
    fn init(&mut self) -> DisplayResult<()> {
        // Actual initialization happens in create_window when we have access to ActiveEventLoop
        Ok(())
    }

    fn shutdown(&mut self) {
        self.surface = None;
        self.surface_config = None;
        self.renderer = None;
        self.window = None;
        self.initialized = false;
    }

    fn render(&mut self, scene: &Scene) -> DisplayResult<()> {
        self.scene = scene.clone();
        Ok(())
    }

    fn present(&mut self) -> DisplayResult<()> {
        self.do_render()
    }

    fn name(&self) -> &'static str {
        "winit-wgpu"
    }

    fn is_initialized(&self) -> bool {
        self.initialized
    }

    fn resize(&mut self, width: u32, height: u32) {
        self.handle_resize(PhysicalSize::new(width, height));
    }

    fn set_vsync(&mut self, enabled: bool) {
        if let Some(config) = &mut self.surface_config {
            config.present_mode = if enabled {
                wgpu::PresentMode::Fifo
            } else {
                wgpu::PresentMode::Immediate
            };

            // Reconfigure surface with new present mode
            if let (Some(surface), Some(renderer)) = (&self.surface, &self.renderer) {
                surface.configure(renderer.device(), config);
            }
        }
    }
}

/// Neomacs application handler for winit.
pub struct NeomacsApp {
    /// The backend instance.
    pub backend: WinitBackend,
}

impl NeomacsApp {
    /// Create a new NeomacsApp with the given backend.
    pub fn new(backend: WinitBackend) -> Self {
        Self { backend }
    }
}

impl ApplicationHandler<UserEvent> for NeomacsApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        // Create the window if it doesn't exist
        if self.backend.window.is_none() {
            if let Err(e) = self.backend.init_main_window(event_loop) {
                log::error!("Failed to create window: {}", e);
                event_loop.exit();
                return;
            }
        }
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::CloseRequested => {
                if let Some(ref callback) = self.backend.callbacks.on_close {
                    callback();
                }
                event_loop.exit();
            }

            WindowEvent::Resized(size) => {
                self.backend.handle_resize(size);
            }

            WindowEvent::RedrawRequested => {
                if let Err(e) = self.backend.do_render() {
                    log::error!("Render error: {}", e);
                }
            }

            WindowEvent::KeyboardInput {
                event,
                is_synthetic: false,
                ..
            } => {
                if let Some(ref callback) = self.backend.callbacks.on_key {
                    callback(event);
                }
            }

            WindowEvent::MouseInput { state, button, .. } => {
                if let Some(ref callback) = self.backend.callbacks.on_mouse_button {
                    let (x, y) = self.backend.cursor_position;
                    callback(button, state, x, y);
                }
            }

            WindowEvent::CursorMoved { position, .. } => {
                self.backend.cursor_position = (position.x, position.y);
                if let Some(ref callback) = self.backend.callbacks.on_mouse_move {
                    callback(position.x, position.y);
                }
            }

            _ => {}
        }
    }

    fn user_event(&mut self, _event_loop: &ActiveEventLoop, event: UserEvent) {
        match event {
            UserEvent::Redraw => {
                self.backend.request_redraw();
            }

            UserEvent::SceneUpdated => {
                self.backend.request_redraw();
            }

            UserEvent::WebKitFrame(view_id) => {
                log::debug!("WebKit frame ready for view {}", view_id);
                self.backend.request_redraw();
            }

            UserEvent::VideoFrame(video_id) => {
                log::debug!("Video frame ready for video {}", video_id);
                self.backend.request_redraw();
            }
        }
    }
}

/// Create and run the event loop with the given backend.
pub fn run_event_loop(mut backend: WinitBackend) -> DisplayResult<()> {
    let event_loop = EventLoop::<UserEvent>::with_user_event()
        .build()
        .map_err(|e| DisplayError::InitFailed(format!("Failed to create event loop: {}", e)))?;

    event_loop.set_control_flow(ControlFlow::Wait);

    // Store the event loop proxy in the backend
    backend.set_event_loop_proxy(event_loop.create_proxy());

    let mut app = NeomacsApp::new(backend);

    event_loop
        .run_app(&mut app)
        .map_err(|e| DisplayError::Backend(format!("Event loop error: {}", e)))?;

    Ok(())
}
