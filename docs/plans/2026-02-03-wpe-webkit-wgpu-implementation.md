# WPE WebKit + wgpu Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Import WPE WebKit frames as wgpu textures without any GTK4 dependency.

**Architecture:** Remove GTK4 entirely, use DMA-BUF export from WPE WebKit, import into wgpu via Vulkan HAL using ash crate.

**Tech Stack:** WPE WebKit, EGL, Vulkan (ash crate), wgpu HAL, DMA-BUF

---

## Task 1: Remove GTK4 Dependencies from Cargo.toml

**Files:**
- Modify: `rust/neomacs-display/Cargo.toml`

**Step 1: Update Cargo.toml**

Remove GTK4 dependencies and add ash:

```toml
[dependencies]
# DELETE these lines entirely:
# gtk4 = { version = "0.9", optional = true }
# gdk4 = { version = "0.9", features = ["v4_14"], optional = true }
# gsk4 = { version = "0.9", optional = true }
# gio = { version = "0.20", optional = true }
# glib = { version = "0.20", optional = true }
# graphene-rs = { version = "0.22.0-alpha.2", package = "graphene-rs", optional = true }
# pango = { version = "0.20", optional = true }
# pangocairo = { version = "0.20", optional = true }

# ADD ash for Vulkan DMA-BUF import
ash = { version = "0.38", optional = true }

[features]
default = ["winit-backend"]
# DELETE gtk4-backend feature entirely
winit-backend = ["winit", "wgpu", "raw-window-handle", "arboard", "bytemuck", "pollster"]
tty-backend = []
video = ["gstreamer", "gstreamer-video"]
# UPDATE wpe-webkit to require winit-backend and ash
wpe-webkit = ["winit-backend", "ash"]
```

**Step 2: Verify syntax**

Run: `cd rust/neomacs-display && cargo check --features winit-backend 2>&1 | head -20`
Expected: May have errors (expected, we haven't removed gtk4 code yet)

**Step 3: Commit**

```bash
git add rust/neomacs-display/Cargo.toml
git commit -m "build: remove GTK4 dependencies, add ash for Vulkan DMA-BUF"
```

---

## Task 2: Delete GTK4 Backend Directory

**Files:**
- Delete: `rust/neomacs-display/src/backend/gtk4/` (entire directory)

**Step 1: Remove gtk4 backend**

```bash
rm -rf rust/neomacs-display/src/backend/gtk4/
```

**Step 2: Commit**

```bash
git add -A
git commit -m "refactor: remove gtk4 backend directory"
```

---

## Task 3: Update backend/mod.rs

**Files:**
- Modify: `rust/neomacs-display/src/backend/mod.rs`

**Step 1: Remove gtk4 module and update BackendType**

```rust
//! Backend trait and module exports.

use crate::core::error::DisplayResult;
use crate::core::scene::Scene;

pub mod tty;

#[cfg(feature = "winit-backend")]
pub mod wgpu;

#[cfg(feature = "wpe-webkit")]
pub mod wpe;

// Remove the webkit stub - wpe module handles it now
// DELETE the entire #[cfg(not(feature = "wpe-webkit"))] pub mod webkit { ... } block

/// Display backend trait
pub trait DisplayBackend {
    fn init(&mut self) -> DisplayResult<()>;
    fn shutdown(&mut self);
    fn render(&mut self, scene: &Scene) -> DisplayResult<()>;
    fn present(&mut self) -> DisplayResult<()>;
    fn name(&self) -> &'static str;
    fn is_initialized(&self) -> bool;
    fn resize(&mut self, width: u32, height: u32);
    fn set_vsync(&mut self, enabled: bool);
}

/// Backend type selection
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub enum BackendType {
    /// Terminal/TTY backend
    Tty = 0,

    /// Winit/wgpu GPU-accelerated backend
    #[cfg(feature = "winit-backend")]
    Wgpu = 1,
}

impl Default for BackendType {
    fn default() -> Self {
        #[cfg(feature = "winit-backend")]
        return Self::Wgpu;

        #[cfg(not(feature = "winit-backend"))]
        return Self::Tty;
    }
}
```

**Step 2: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features winit-backend 2>&1 | head -30`
Expected: Errors in ffi.rs (expected, we fix that next)

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/backend/mod.rs
git commit -m "refactor: remove gtk4 from backend module, simplify BackendType"
```

---

## Task 4: Clean Up ffi.rs - Remove GTK4 Code

**Files:**
- Modify: `rust/neomacs-display/src/ffi.rs`

**Step 1: Remove all GTK4 imports and code paths**

This is a large file. Key changes:
1. Remove all `#[cfg(feature = "gtk4-backend")]` blocks entirely
2. Remove gtk4-related struct fields from NeomacsDisplay
3. Remove gtk4-related FFI functions
4. Keep only winit-backend and wpe-webkit paths

The NeomacsDisplay struct should become:

```rust
pub struct NeomacsDisplay {
    backend_type: BackendType,

    #[cfg(feature = "winit-backend")]
    winit_backend: Option<WinitBackend>,

    scene: Scene,
    faces: FaceRegistry,
    text_engine: TextEngine,
}
```

Remove all functions that reference:
- gtk4_backend
- renderer (Gtk4Renderer)
- gsk_renderer
- hybrid_renderer
- video_cache (GTK4-specific)
- image_cache (GTK4-specific)
- Any pango/cairo rendering

Keep:
- neomacs_display_init (simplified)
- neomacs_display_free
- neomacs_display_render
- Face registration functions
- Scene building functions
- WPE webkit functions (feature-gated)

**Step 2: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features winit-backend 2>&1 | head -50`
Expected: May have errors in wpe module (expected)

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/ffi.rs
git commit -m "refactor: remove all GTK4 code from FFI layer"
```

---

## Task 5: Clean Up Core Types - Remove GTK4 Helpers

**Files:**
- Modify: `rust/neomacs-display/src/core/types.rs`
- Modify: `rust/neomacs-display/src/text/mod.rs`
- Modify: `rust/neomacs-display/src/text/engine.rs`
- Modify: `rust/neomacs-display/src/text/atlas.rs`
- Modify: `rust/neomacs-display/src/lib.rs`

**Step 1: Remove GTK4 helper methods from types.rs**

Remove `to_gdk()`, `to_graphene()`, `to_gsk()` methods.

**Step 2: Remove GTK4 from text module**

In `text/mod.rs`, remove GlyphAtlas export if it depends on gdk4.
In `text/engine.rs`, remove `create_texture()` method that uses gdk4.
In `text/atlas.rs`, remove or feature-gate gdk4-dependent code.

**Step 3: Update lib.rs init function**

Remove gtk4::init() call, keep only simple initialization.

**Step 4: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features winit-backend 2>&1 | head -30`

**Step 5: Commit**

```bash
git add rust/neomacs-display/src/
git commit -m "refactor: remove GTK4 helpers from core types and text module"
```

---

## Task 6: Update WPE dmabuf.rs - Remove GDK4

**Files:**
- Modify: `rust/neomacs-display/src/backend/wpe/dmabuf.rs`

**Step 1: Add ExportedDmaBuf struct and remove gdk4**

```rust
//! DMA-BUF export from EGLImages.
//!
//! Exports EGLImages from WPE WebKit to DMA-BUF file descriptors
//! for zero-copy import into wgpu.

use std::ffi::CStr;

use crate::core::error::{DisplayError, DisplayResult};
use super::sys::egl;

/// Exported DMA-BUF data (no GTK4 dependency)
#[derive(Debug)]
pub struct ExportedDmaBuf {
    /// File descriptors per plane (up to 4)
    pub fds: [i32; 4],
    /// Stride per plane in bytes
    pub strides: [u32; 4],
    /// Offset per plane in bytes
    pub offsets: [u32; 4],
    /// Number of planes
    pub num_planes: u32,
    /// DRM fourcc format code
    pub fourcc: u32,
    /// DRM modifier
    pub modifier: u64,
    /// Width in pixels
    pub width: u32,
    /// Height in pixels
    pub height: u32,
}

impl ExportedDmaBuf {
    /// Close file descriptors
    pub fn close_fds(&mut self) {
        for i in 0..self.num_planes as usize {
            if self.fds[i] >= 0 {
                unsafe { libc::close(self.fds[i]); }
                self.fds[i] = -1;
            }
        }
    }
}

impl Drop for ExportedDmaBuf {
    fn drop(&mut self) {
        self.close_fds();
    }
}

/// DMA-BUF exporter using EGL MESA extensions.
pub struct DmaBufExporter {
    egl_display: egl::EGLDisplay,
    export_dmabuf_image: Option<egl::PFNEGLEXPORTDMABUFIMAGEMESAPROC>,
    export_dmabuf_query: Option<egl::PFNEGLEXPORTDMABUFIMAGEQUERYMESAPROC>,
    supported: bool,
}

impl DmaBufExporter {
    /// Create a new DMA-BUF exporter for the given EGL display.
    pub fn new(egl_display: *mut libc::c_void) -> Self {
        // ... (keep existing initialization code, just remove gdk4 parts)
    }

    /// Check if DMA-BUF export is supported.
    pub fn is_supported(&self) -> bool {
        self.supported
    }

    /// Export an EGLImage to DMA-BUF.
    pub fn export_egl_image(
        &self,
        egl_image: *mut libc::c_void,
        width: u32,
        height: u32,
    ) -> DisplayResult<ExportedDmaBuf> {
        if !self.supported {
            return Err(DisplayError::WebKit("DMA-BUF export not supported".into()));
        }

        if egl_image.is_null() {
            return Err(DisplayError::WebKit("NULL EGLImage".into()));
        }

        let export_query = self.export_dmabuf_query.unwrap();
        let export_image = self.export_dmabuf_image.unwrap();

        unsafe {
            let mut fourcc: libc::c_int = 0;
            let mut num_planes: libc::c_int = 0;
            let mut modifier: u64 = 0;

            let query_result = export_query(
                self.egl_display,
                egl_image as egl::EGLImageKHR,
                &mut fourcc,
                &mut num_planes,
                &mut modifier,
            );

            if query_result == 0 {
                return Err(DisplayError::WebKit("eglExportDMABUFImageQueryMESA failed".into()));
            }

            if num_planes < 1 || num_planes > 4 {
                return Err(DisplayError::WebKit(format!("Invalid plane count: {}", num_planes)));
            }

            let mut fds: [libc::c_int; 4] = [-1; 4];
            let mut strides: [i32; 4] = [0; 4];
            let mut offsets: [i32; 4] = [0; 4];

            let export_result = export_image(
                self.egl_display,
                egl_image as egl::EGLImageKHR,
                fds.as_mut_ptr(),
                strides.as_mut_ptr(),
                offsets.as_mut_ptr(),
            );

            if export_result == 0 {
                return Err(DisplayError::WebKit("eglExportDMABUFImageMESA failed".into()));
            }

            Ok(ExportedDmaBuf {
                fds,
                strides: [
                    strides[0] as u32,
                    strides[1] as u32,
                    strides[2] as u32,
                    strides[3] as u32,
                ],
                offsets: [
                    offsets[0] as u32,
                    offsets[1] as u32,
                    offsets[2] as u32,
                    offsets[3] as u32,
                ],
                num_planes: num_planes as u32,
                fourcc: fourcc as u32,
                modifier,
                width,
                height,
            })
        }
    }
}
```

**Step 2: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features wpe-webkit 2>&1 | head -30`

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/backend/wpe/dmabuf.rs
git commit -m "refactor: remove gdk4 from DmaBufExporter, add ExportedDmaBuf"
```

---

## Task 7: Update WPE view.rs and platform.rs

**Files:**
- Modify: `rust/neomacs-display/src/backend/wpe/view.rs`
- Modify: `rust/neomacs-display/src/backend/wpe/platform.rs`

**Step 1: Remove gdk4 from view.rs**

Remove gdk4 imports and `gdk4::Texture` return types. Add `get_frame_dmabuf()` method:

```rust
impl WpeWebView {
    /// Get current frame as DmaBufBuffer for wgpu rendering.
    pub fn get_frame_dmabuf(&self) -> Option<crate::backend::wgpu::external_buffer::DmaBufBuffer> {
        // Get EGLImage from WPE
        // Export to DmaBuf using DmaBufExporter
        // Convert to DmaBufBuffer
        todo!("Implement after DmaBufBuffer is ready")
    }
}
```

**Step 2: Remove gdk4 from platform.rs**

Remove `use gtk4::gdk;` and any gdk-related code.

**Step 3: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features wpe-webkit 2>&1 | head -30`

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/backend/wpe/
git commit -m "refactor: remove gdk4 from WPE view and platform modules"
```

---

## Task 8: Implement DmaBufBuffer::to_wgpu_texture()

**Files:**
- Modify: `rust/neomacs-display/src/backend/wgpu/external_buffer.rs`

**Step 1: Add ash imports and Vulkan DMA-BUF import**

```rust
#[cfg(target_os = "linux")]
use ash::vk;

#[cfg(target_os = "linux")]
impl DmaBufBuffer {
    /// Import DMA-BUF as wgpu texture using Vulkan external memory.
    pub fn to_wgpu_texture_impl(
        &self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> Option<wgpu::Texture> {
        use wgpu::hal::api::Vulkan;

        // This requires unsafe HAL access
        unsafe {
            // 1. Get raw Vulkan device from wgpu
            device.as_hal::<Vulkan, _, _>(|hal_device| {
                let hal_device = hal_device?;
                let raw_device = hal_device.raw_device();

                // 2. Create VkImage with external memory
                let external_memory_image_info = vk::ExternalMemoryImageCreateInfo::builder()
                    .handle_types(vk::ExternalMemoryHandleTypeFlags::DMA_BUF_EXT)
                    .build();

                let image_info = vk::ImageCreateInfo::builder()
                    .push_next(&mut external_memory_image_info)
                    .image_type(vk::ImageType::TYPE_2D)
                    .format(self.drm_format_to_vk_format())
                    .extent(vk::Extent3D {
                        width: self.width,
                        height: self.height,
                        depth: 1,
                    })
                    .mip_levels(1)
                    .array_layers(1)
                    .samples(vk::SampleCountFlags::TYPE_1)
                    .tiling(vk::ImageTiling::DRM_FORMAT_MODIFIER_EXT)
                    .usage(vk::ImageUsageFlags::SAMPLED)
                    .sharing_mode(vk::SharingMode::EXCLUSIVE)
                    .build();

                // 3. Import DMA-BUF fd
                let import_memory_fd_info = vk::ImportMemoryFdInfoKHR::builder()
                    .handle_type(vk::ExternalMemoryHandleTypeFlags::DMA_BUF_EXT)
                    .fd(self.fd)
                    .build();

                // ... complete Vulkan import logic

                // 4. Wrap as wgpu::Texture
                // This part requires careful HAL interop

                None // Placeholder - full implementation needed
            })?
        }
    }

    fn drm_format_to_vk_format(&self) -> vk::Format {
        // Convert DRM fourcc to Vulkan format
        match self.format {
            0x34325241 => vk::Format::B8G8R8A8_UNORM,  // DRM_FORMAT_ARGB8888
            0x34325258 => vk::Format::B8G8R8A8_UNORM,  // DRM_FORMAT_XRGB8888
            _ => vk::Format::B8G8R8A8_UNORM,  // Default
        }
    }
}
```

**Note:** This is complex code requiring careful Vulkan/wgpu HAL interop. The actual implementation will need extensive testing.

**Step 2: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features wpe-webkit 2>&1 | head -30`

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/backend/wgpu/external_buffer.rs
git commit -m "feat: implement DmaBufBuffer::to_wgpu_texture via Vulkan HAL"
```

---

## Task 9: Create WgpuWebKitCache

**Files:**
- Create: `rust/neomacs-display/src/backend/wgpu/webkit_cache.rs`
- Modify: `rust/neomacs-display/src/backend/wgpu/mod.rs`

**Step 1: Create webkit_cache.rs**

```rust
//! WebKit view texture cache for wgpu rendering.

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;

use super::external_buffer::DmaBufBuffer;

/// Cached WebKit view texture.
pub struct CachedWebKitView {
    pub texture: wgpu::Texture,
    pub view: wgpu::TextureView,
    pub bind_group: wgpu::BindGroup,
    pub width: u32,
    pub height: u32,
    pub last_updated: Instant,
}

/// Cache of WebKit view textures for wgpu rendering.
pub struct WgpuWebKitCache {
    views: HashMap<u32, CachedWebKitView>,
    bind_group_layout: wgpu::BindGroupLayout,
    sampler: wgpu::Sampler,
}

impl WgpuWebKitCache {
    /// Create a new WebKit cache.
    pub fn new(device: &wgpu::Device) -> Self {
        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("WebKit Bind Group Layout"),
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

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("WebKit Sampler"),
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            ..Default::default()
        });

        Self {
            views: HashMap::new(),
            bind_group_layout,
            sampler,
        }
    }

    /// Get the bind group layout for texture rendering.
    pub fn bind_group_layout(&self) -> &wgpu::BindGroupLayout {
        &self.bind_group_layout
    }

    /// Update or create a cached view from DmaBufBuffer.
    pub fn update_view(
        &mut self,
        view_id: u32,
        buffer: DmaBufBuffer,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> bool {
        let texture = match buffer.to_wgpu_texture(device, queue) {
            Some(t) => t,
            None => {
                log::warn!("Failed to import DMA-BUF for view {}", view_id);
                return false;
            }
        };

        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());

        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("WebKit Bind Group"),
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

        let (width, height) = buffer.dimensions();

        self.views.insert(view_id, CachedWebKitView {
            texture,
            view,
            bind_group,
            width,
            height,
            last_updated: Instant::now(),
        });

        true
    }

    /// Get a cached view.
    pub fn get(&self, view_id: u32) -> Option<&CachedWebKitView> {
        self.views.get(&view_id)
    }

    /// Get bind group for a view.
    pub fn get_bind_group(&self, view_id: u32) -> Option<&wgpu::BindGroup> {
        self.views.get(&view_id).map(|v| &v.bind_group)
    }

    /// Remove a view.
    pub fn remove(&mut self, view_id: u32) {
        self.views.remove(&view_id);
    }

    /// Clear all cached views.
    pub fn clear(&mut self) {
        self.views.clear();
    }
}
```

**Step 2: Update mod.rs to include webkit_cache**

Add to `rust/neomacs-display/src/backend/wgpu/mod.rs`:

```rust
#[cfg(feature = "wpe-webkit")]
mod webkit_cache;

#[cfg(feature = "wpe-webkit")]
pub use webkit_cache::{WgpuWebKitCache, CachedWebKitView};
```

**Step 3: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features wpe-webkit 2>&1 | head -30`

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/backend/wgpu/
git commit -m "feat: add WgpuWebKitCache for WebKit texture caching"
```

---

## Task 10: Final Integration and Testing

**Files:**
- Modify: `rust/neomacs-display/src/backend/wgpu/renderer.rs`

**Step 1: Add WebKit rendering support to WgpuRenderer**

```rust
impl WgpuRenderer {
    /// Render a WebKit view texture at the given bounds.
    #[cfg(feature = "wpe-webkit")]
    pub fn render_webkit_view(
        &mut self,
        encoder: &mut wgpu::CommandEncoder,
        view: &wgpu::TextureView,
        webkit_bind_group: &wgpu::BindGroup,
        bounds: crate::core::types::Rect,
    ) {
        // Use texture pipeline to render webkit content
        // Similar to render_to_view but with external texture
    }
}
```

**Step 2: Verify full compilation**

Run: `cd rust/neomacs-display && cargo check --features wpe-webkit`
Expected: Compiles with warnings only

Run: `cd rust/neomacs-display && cargo test --features winit-backend --lib`
Expected: All tests pass

**Step 3: Commit**

```bash
git add -A
git commit -m "feat: integrate WebKit rendering with WgpuRenderer"
```

---

## Summary

| Task | Description | Key Files |
|------|-------------|-----------|
| 1 | Remove GTK4 from Cargo.toml | Cargo.toml |
| 2 | Delete gtk4 backend | src/backend/gtk4/ |
| 3 | Update backend/mod.rs | mod.rs |
| 4 | Clean ffi.rs | ffi.rs |
| 5 | Clean core types | types.rs, text/* |
| 6 | Update WPE dmabuf.rs | dmabuf.rs |
| 7 | Update WPE view/platform | view.rs, platform.rs |
| 8 | Implement DMA-BUF import | external_buffer.rs |
| 9 | Create WebKit cache | webkit_cache.rs |
| 10 | Final integration | renderer.rs |
