# Pure Winit Emacs Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace GTK windowing with winit so Emacs renders entirely via wgpu.

**Architecture:** Emacs keeps its event loop, calls into Rust for window management and rendering. Rust uses winit's `pump_events()` for non-blocking event polling. Events flow from winit → Rust → FFI callback → Emacs event queue.

**Tech Stack:** winit 0.30, wgpu 23, Emacs C FFI, Rust

---

## Phase 1: Window Creation

### Task 1: Add Window FFI Types to Header

**Files:**
- Modify: `rust/neomacs-display/include/neomacs_display.h`

**Step 1: Add window FFI declarations**

Add after line ~460 (after existing FFI declarations):

```c
/* Window management - winit backend */
uint32_t neomacs_display_create_window(void *display_handle, int width, int height, const char *title);
void neomacs_display_destroy_window(void *display_handle, uint32_t window_id);
void neomacs_display_show_window(void *display_handle, uint32_t window_id, bool visible);
void neomacs_display_set_window_title(void *display_handle, uint32_t window_id, const char *title);
void neomacs_display_set_window_size(void *display_handle, uint32_t window_id, int width, int height);

/* Event polling - winit backend */
int neomacs_display_poll_events(void *display_handle);
```

**Step 2: Commit**

```bash
git add rust/neomacs-display/include/neomacs_display.h
git commit -m "feat: add window management FFI declarations"
```

---

### Task 2: Add WindowState Struct in Rust

**Files:**
- Create: `rust/neomacs-display/src/backend/wgpu/window_state.rs`
- Modify: `rust/neomacs-display/src/backend/wgpu/mod.rs`

**Step 1: Create window_state.rs**

```rust
//! Window state management for winit windows.

use std::sync::Arc;
use winit::window::Window;

use crate::core::scene::Scene;

/// State for a single winit window.
pub struct WindowState {
    pub window: Arc<Window>,
    pub surface: wgpu::Surface<'static>,
    pub config: wgpu::SurfaceConfiguration,
    pub scene: Scene,
    pub width: u32,
    pub height: u32,
}

impl WindowState {
    pub fn new(
        window: Arc<Window>,
        surface: wgpu::Surface<'static>,
        config: wgpu::SurfaceConfiguration,
        width: u32,
        height: u32,
    ) -> Self {
        Self {
            window,
            surface,
            config,
            scene: Scene::new(width, height),
            width,
            height,
        }
    }

    pub fn resize(&mut self, device: &wgpu::Device, width: u32, height: u32) {
        if width > 0 && height > 0 {
            self.width = width;
            self.height = height;
            self.config.width = width;
            self.config.height = height;
            self.surface.configure(device, &self.config);
            self.scene.resize(width, height);
        }
    }
}
```

**Step 2: Update mod.rs**

Add to `rust/neomacs-display/src/backend/wgpu/mod.rs`:

```rust
mod window_state;
pub use window_state::WindowState;
```

**Step 3: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features winit-backend`
Expected: Compiles successfully

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/backend/wgpu/window_state.rs
git add rust/neomacs-display/src/backend/wgpu/mod.rs
git commit -m "feat: add WindowState struct for winit windows"
```

---

### Task 3: Add Window Management to WinitBackend

**Files:**
- Modify: `rust/neomacs-display/src/backend/wgpu/backend.rs`

**Step 1: Add window storage and methods**

Add imports at top:

```rust
use std::collections::HashMap;
use super::window_state::WindowState;
```

Add to WinitBackend struct:

```rust
pub struct WinitBackend {
    // ... existing fields
    windows: HashMap<u32, WindowState>,
    next_window_id: u32,
}
```

Update `new()` to initialize:

```rust
windows: HashMap::new(),
next_window_id: 1,
```

Add methods:

```rust
impl WinitBackend {
    pub fn create_window(&mut self, width: u32, height: u32, title: &str) -> Option<u32> {
        let event_loop = self.event_loop.as_ref()?;

        let window_attrs = winit::window::WindowAttributes::default()
            .with_title(title)
            .with_inner_size(winit::dpi::PhysicalSize::new(width, height));

        let window = Arc::new(event_loop.create_window(window_attrs).ok()?);

        let surface = self.instance.create_surface(window.clone()).ok()?;
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
        surface.configure(&self.device, &config);

        let window_id = self.next_window_id;
        self.next_window_id += 1;

        let state = WindowState::new(window, surface, config, width, height);
        self.windows.insert(window_id, state);

        Some(window_id)
    }

    pub fn destroy_window(&mut self, window_id: u32) {
        self.windows.remove(&window_id);
    }

    pub fn get_window(&self, window_id: u32) -> Option<&WindowState> {
        self.windows.get(&window_id)
    }

    pub fn get_window_mut(&mut self, window_id: u32) -> Option<&mut WindowState> {
        self.windows.get_mut(&window_id)
    }
}
```

**Step 2: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features winit-backend`
Expected: Compiles (may have warnings about unused fields)

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/backend/wgpu/backend.rs
git commit -m "feat: add window management methods to WinitBackend"
```

---

### Task 4: Add Window FFI Functions

**Files:**
- Modify: `rust/neomacs-display/src/ffi.rs`

**Step 1: Add window FFI exports**

Add near other FFI functions:

```rust
#[no_mangle]
pub extern "C" fn neomacs_display_create_window(
    handle: *mut NeomacsDisplay,
    width: i32,
    height: i32,
    title: *const c_char,
) -> u32 {
    let display = unsafe { &mut *handle };

    #[cfg(feature = "winit-backend")]
    if let Some(ref mut backend) = display.winit_backend {
        let title_str = if title.is_null() {
            "Emacs"
        } else {
            unsafe { std::ffi::CStr::from_ptr(title).to_str().unwrap_or("Emacs") }
        };
        return backend.create_window(width as u32, height as u32, title_str).unwrap_or(0);
    }

    0
}

#[no_mangle]
pub extern "C" fn neomacs_display_destroy_window(handle: *mut NeomacsDisplay, window_id: u32) {
    let display = unsafe { &mut *handle };

    #[cfg(feature = "winit-backend")]
    if let Some(ref mut backend) = display.winit_backend {
        backend.destroy_window(window_id);
    }
}

#[no_mangle]
pub extern "C" fn neomacs_display_show_window(
    handle: *mut NeomacsDisplay,
    window_id: u32,
    visible: bool,
) {
    let display = unsafe { &mut *handle };

    #[cfg(feature = "winit-backend")]
    if let Some(ref backend) = display.winit_backend {
        if let Some(state) = backend.get_window(window_id) {
            state.window.set_visible(visible);
        }
    }
}

#[no_mangle]
pub extern "C" fn neomacs_display_set_window_title(
    handle: *mut NeomacsDisplay,
    window_id: u32,
    title: *const c_char,
) {
    let display = unsafe { &mut *handle };

    #[cfg(feature = "winit-backend")]
    if let Some(ref backend) = display.winit_backend {
        if let Some(state) = backend.get_window(window_id) {
            let title_str = if title.is_null() {
                "Emacs"
            } else {
                unsafe { std::ffi::CStr::from_ptr(title).to_str().unwrap_or("Emacs") }
            };
            state.window.set_title(title_str);
        }
    }
}

#[no_mangle]
pub extern "C" fn neomacs_display_set_window_size(
    handle: *mut NeomacsDisplay,
    window_id: u32,
    width: i32,
    height: i32,
) {
    let display = unsafe { &mut *handle };

    #[cfg(feature = "winit-backend")]
    if let Some(ref mut backend) = display.winit_backend {
        if let Some(state) = backend.get_window_mut(window_id) {
            let _ = state.window.request_inner_size(
                winit::dpi::PhysicalSize::new(width as u32, height as u32)
            );
        }
    }
}
```

**Step 2: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features winit-backend`
Expected: Compiles successfully

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/ffi.rs
git commit -m "feat: add window management FFI functions"
```

---

### Task 5: Update C Code to Use Winit Windows

**Files:**
- Modify: `src/neomacsfns.c`
- Modify: `src/neomacsterm.h`

**Step 1: Add window_id to frame output struct**

In `src/neomacsterm.h`, add to `struct neomacs_output`:

```c
struct neomacs_output {
  // ... existing fields
  uint32_t window_id;  /* Winit window identifier */
};
```

**Step 2: Replace GTK window creation in neomacsfns.c**

Find the `neomacs_create_frame_window` function (around line 1130) and replace the GTK window creation with:

```c
static void
neomacs_create_frame_window (struct frame *f)
{
  struct neomacs_display_info *dpyinfo = FRAME_NEOMACS_DISPLAY_INFO (f);

  /* Create winit window via Rust backend */
  uint32_t window_id = neomacs_display_create_window(
      dpyinfo->display_handle,
      FRAME_PIXEL_WIDTH (f),
      FRAME_PIXEL_HEIGHT (f),
      "Emacs"
  );

  if (window_id == 0)
    {
      error ("Failed to create winit window");
      return;
    }

  FRAME_NEOMACS_OUTPUT (f)->window_id = window_id;

  /* Show the window */
  neomacs_display_show_window (dpyinfo->display_handle, window_id, true);
}
```

**Step 3: Compile Emacs to verify**

Run: `make -j8 2>&1 | tail -20`
Expected: Compiles (may have warnings)

**Step 4: Commit**

```bash
git add src/neomacsfns.c src/neomacsterm.h
git commit -m "feat: use winit windows instead of GTK"
```

---

## Phase 2: Event Polling

### Task 6: Add Event Types

**Files:**
- Create: `rust/neomacs-display/src/backend/wgpu/events.rs`
- Modify: `rust/neomacs-display/src/backend/wgpu/mod.rs`

**Step 1: Create events.rs**

```rust
//! Event types for winit → Emacs communication.

/// Input event kinds matching Emacs event types.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventKind {
    KeyPress = 1,
    KeyRelease = 2,
    MousePress = 3,
    MouseRelease = 4,
    MouseMove = 5,
    Scroll = 6,
    Resize = 7,
    CloseRequest = 8,
    FocusIn = 9,
    FocusOut = 10,
}

/// Modifier flags matching Emacs.
pub const NEOMACS_SHIFT_MASK: u32 = 1 << 0;
pub const NEOMACS_CTRL_MASK: u32 = 1 << 1;
pub const NEOMACS_META_MASK: u32 = 1 << 2;
pub const NEOMACS_SUPER_MASK: u32 = 1 << 3;

/// Input event structure passed to C.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct NeomacsInputEvent {
    pub kind: u32,
    pub window_id: u32,
    pub timestamp: u64,
    pub x: i32,
    pub y: i32,
    pub keycode: u32,
    pub keysym: u32,
    pub modifiers: u32,
    pub button: u32,
    pub scroll_delta_x: f32,
    pub scroll_delta_y: f32,
    pub width: u32,
    pub height: u32,
}

impl Default for NeomacsInputEvent {
    fn default() -> Self {
        Self {
            kind: 0,
            window_id: 0,
            timestamp: 0,
            x: 0,
            y: 0,
            keycode: 0,
            keysym: 0,
            modifiers: 0,
            button: 0,
            scroll_delta_x: 0.0,
            scroll_delta_y: 0.0,
            width: 0,
            height: 0,
        }
    }
}
```

**Step 2: Update mod.rs**

Add to `rust/neomacs-display/src/backend/wgpu/mod.rs`:

```rust
mod events;
pub use events::{EventKind, NeomacsInputEvent, NEOMACS_SHIFT_MASK, NEOMACS_CTRL_MASK, NEOMACS_META_MASK, NEOMACS_SUPER_MASK};
```

**Step 3: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features winit-backend`

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/backend/wgpu/events.rs
git add rust/neomacs-display/src/backend/wgpu/mod.rs
git commit -m "feat: add event types for winit→Emacs communication"
```

---

### Task 7: Implement Event Polling

**Files:**
- Modify: `rust/neomacs-display/src/backend/wgpu/backend.rs`

**Step 1: Add event queue and polling**

Add imports:

```rust
use std::collections::VecDeque;
use std::time::Duration;
use winit::platform::pump_events::EventLoopExtPumpEvents;
use winit::event::{Event, WindowEvent, ElementState, MouseButton};
use winit::keyboard::{Key, NamedKey};
use super::events::*;
```

Add to WinitBackend struct:

```rust
event_queue: VecDeque<NeomacsInputEvent>,
current_modifiers: u32,
mouse_position: (i32, i32),
```

Initialize in `new()`:

```rust
event_queue: VecDeque::new(),
current_modifiers: 0,
mouse_position: (0, 0),
```

Add method:

```rust
impl WinitBackend {
    pub fn poll_events(&mut self) -> Vec<NeomacsInputEvent> {
        let event_loop = match self.event_loop.take() {
            Some(el) => el,
            None => return vec![],
        };

        let mut events = Vec::new();

        event_loop.pump_events(Some(Duration::ZERO), |event, _target| {
            match event {
                Event::WindowEvent { window_id: _, event } => {
                    if let Some(ev) = self.translate_window_event(event) {
                        events.push(ev);
                    }
                }
                _ => {}
            }
        });

        self.event_loop = Some(event_loop);
        events
    }

    fn translate_window_event(&mut self, event: WindowEvent) -> Option<NeomacsInputEvent> {
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis() as u64;

        match event {
            WindowEvent::KeyboardInput { event, .. } => {
                let keysym = self.translate_key(&event.logical_key);
                Some(NeomacsInputEvent {
                    kind: if event.state == ElementState::Pressed {
                        EventKind::KeyPress as u32
                    } else {
                        EventKind::KeyRelease as u32
                    },
                    timestamp,
                    keysym,
                    modifiers: self.current_modifiers,
                    ..Default::default()
                })
            }
            WindowEvent::ModifiersChanged(mods) => {
                self.current_modifiers = 0;
                let state = mods.state();
                if state.shift_key() { self.current_modifiers |= NEOMACS_SHIFT_MASK; }
                if state.control_key() { self.current_modifiers |= NEOMACS_CTRL_MASK; }
                if state.alt_key() { self.current_modifiers |= NEOMACS_META_MASK; }
                if state.super_key() { self.current_modifiers |= NEOMACS_SUPER_MASK; }
                None
            }
            WindowEvent::CursorMoved { position, .. } => {
                self.mouse_position = (position.x as i32, position.y as i32);
                Some(NeomacsInputEvent {
                    kind: EventKind::MouseMove as u32,
                    timestamp,
                    x: position.x as i32,
                    y: position.y as i32,
                    modifiers: self.current_modifiers,
                    ..Default::default()
                })
            }
            WindowEvent::MouseInput { button, state, .. } => {
                let btn = match button {
                    MouseButton::Left => 1,
                    MouseButton::Middle => 2,
                    MouseButton::Right => 3,
                    _ => 0,
                };
                Some(NeomacsInputEvent {
                    kind: if state == ElementState::Pressed {
                        EventKind::MousePress as u32
                    } else {
                        EventKind::MouseRelease as u32
                    },
                    timestamp,
                    x: self.mouse_position.0,
                    y: self.mouse_position.1,
                    button: btn,
                    modifiers: self.current_modifiers,
                    ..Default::default()
                })
            }
            WindowEvent::MouseWheel { delta, .. } => {
                let (dx, dy) = match delta {
                    winit::event::MouseScrollDelta::LineDelta(x, y) => (x, y),
                    winit::event::MouseScrollDelta::PixelDelta(pos) => {
                        (pos.x as f32 / 10.0, pos.y as f32 / 10.0)
                    }
                };
                Some(NeomacsInputEvent {
                    kind: EventKind::Scroll as u32,
                    timestamp,
                    x: self.mouse_position.0,
                    y: self.mouse_position.1,
                    scroll_delta_x: dx,
                    scroll_delta_y: dy,
                    modifiers: self.current_modifiers,
                    ..Default::default()
                })
            }
            WindowEvent::Resized(size) => {
                Some(NeomacsInputEvent {
                    kind: EventKind::Resize as u32,
                    timestamp,
                    width: size.width,
                    height: size.height,
                    ..Default::default()
                })
            }
            WindowEvent::CloseRequested => {
                Some(NeomacsInputEvent {
                    kind: EventKind::CloseRequest as u32,
                    timestamp,
                    ..Default::default()
                })
            }
            WindowEvent::Focused(focused) => {
                Some(NeomacsInputEvent {
                    kind: if focused {
                        EventKind::FocusIn as u32
                    } else {
                        EventKind::FocusOut as u32
                    },
                    timestamp,
                    ..Default::default()
                })
            }
            _ => None,
        }
    }

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
```

**Step 2: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features winit-backend`

**Step 3: Commit**

```bash
git add rust/neomacs-display/src/backend/wgpu/backend.rs
git commit -m "feat: implement event polling with winit pump_events"
```

---

### Task 8: Add Event Polling FFI

**Files:**
- Modify: `rust/neomacs-display/src/ffi.rs`

**Step 1: Add event callback type and poll function**

Add type alias near top:

```rust
type EventCallback = extern "C" fn(*const crate::backend::wgpu::NeomacsInputEvent);
static mut EVENT_CALLBACK: Option<EventCallback> = None;
```

Add FFI function:

```rust
#[no_mangle]
pub extern "C" fn neomacs_display_set_event_callback(callback: EventCallback) {
    unsafe {
        EVENT_CALLBACK = Some(callback);
    }
}

#[no_mangle]
pub extern "C" fn neomacs_display_poll_events(handle: *mut NeomacsDisplay) -> i32 {
    let display = unsafe { &mut *handle };
    let mut count = 0;

    #[cfg(feature = "winit-backend")]
    if let Some(ref mut backend) = display.winit_backend {
        let events = backend.poll_events();
        count = events.len() as i32;

        unsafe {
            if let Some(callback) = EVENT_CALLBACK {
                for event in &events {
                    callback(event as *const _);
                }
            }
        }
    }

    count
}
```

**Step 2: Update header file**

Add to `rust/neomacs-display/include/neomacs_display.h`:

```c
/* Event callback type */
typedef struct {
    uint32_t kind;
    uint32_t window_id;
    uint64_t timestamp;
    int32_t x;
    int32_t y;
    uint32_t keycode;
    uint32_t keysym;
    uint32_t modifiers;
    uint32_t button;
    float scroll_delta_x;
    float scroll_delta_y;
    uint32_t width;
    uint32_t height;
} NeomacsInputEvent;

typedef void (*neomacs_event_callback_t)(const NeomacsInputEvent *event);
void neomacs_display_set_event_callback(neomacs_event_callback_t callback);
```

**Step 3: Verify compilation**

Run: `cd rust/neomacs-display && cargo check --features winit-backend`

**Step 4: Commit**

```bash
git add rust/neomacs-display/src/ffi.rs
git add rust/neomacs-display/include/neomacs_display.h
git commit -m "feat: add event polling FFI with callback"
```

---

### Task 9: Integrate Event Polling in Emacs

**Files:**
- Modify: `src/neomacsterm.c`

**Step 1: Add event callback and update read_socket**

Add callback function:

```c
/* Event callback from Rust/winit */
static void
neomacs_event_callback (const NeomacsInputEvent *event)
{
  union buffered_input_event inev;
  struct frame *f = NULL;  /* TODO: look up from window_id */

  /* Find frame by window_id */
  FOR_EACH_FRAME (tail, frame)
    {
      struct frame *tf = XFRAME (frame);
      if (FRAME_NEOMACS_P (tf)
          && FRAME_NEOMACS_OUTPUT (tf)->window_id == event->window_id)
        {
          f = tf;
          break;
        }
    }

  if (!f)
    f = SELECTED_FRAME ();

  EVENT_INIT (inev.ie);
  inev.ie.timestamp = event->timestamp;

  switch (event->kind)
    {
    case 1: /* KeyPress */
      inev.ie.kind = ASCII_KEYSTROKE_EVENT;
      inev.ie.code = event->keysym;
      inev.ie.modifiers = 0;
      if (event->modifiers & 1) inev.ie.modifiers |= shift_modifier;
      if (event->modifiers & 2) inev.ie.modifiers |= ctrl_modifier;
      if (event->modifiers & 4) inev.ie.modifiers |= meta_modifier;
      if (event->modifiers & 8) inev.ie.modifiers |= super_modifier;
      XSETFRAME (inev.ie.frame_or_window, f);
      neomacs_evq_enqueue (&inev);
      break;

    case 3: /* MousePress */
    case 4: /* MouseRelease */
      inev.ie.kind = (event->kind == 3) ? MOUSE_CLICK_EVENT : MOUSE_CLICK_EVENT;
      inev.ie.code = event->button;
      inev.ie.modifiers = (event->kind == 3) ? down_modifier : up_modifier;
      XSETINT (inev.ie.x, event->x);
      XSETINT (inev.ie.y, event->y);
      XSETFRAME (inev.ie.frame_or_window, f);
      neomacs_evq_enqueue (&inev);
      break;

    case 7: /* Resize */
      {
        struct neomacs_display_info *dpyinfo = FRAME_NEOMACS_DISPLAY_INFO (f);
        neomacs_display_resize (dpyinfo->display_handle, event->width, event->height);
      }
      break;

    case 8: /* CloseRequest */
      inev.ie.kind = DELETE_WINDOW_EVENT;
      XSETFRAME (inev.ie.frame_or_window, f);
      neomacs_evq_enqueue (&inev);
      break;
    }
}

/* Updated read_socket */
static int
neomacs_read_socket (struct terminal *terminal, struct input_event *hold_quit)
{
  struct neomacs_display_info *dpyinfo = terminal->display_info.neomacs;

  /* Poll winit events */
  neomacs_display_poll_events (dpyinfo->display_handle);

  /* Flush queued events to Emacs */
  return neomacs_evq_flush (hold_quit);
}
```

Add initialization in terminal setup:

```c
/* In neomacs_term_init or similar */
neomacs_display_set_event_callback (neomacs_event_callback);
```

**Step 2: Compile and test**

Run: `make -j8`

**Step 3: Commit**

```bash
git add src/neomacsterm.c
git commit -m "feat: integrate winit event polling with Emacs event loop"
```

---

## Phase 3: Rendering to Window

### Task 10: Update Rendering to Target Window

**Files:**
- Modify: `rust/neomacs-display/src/backend/wgpu/backend.rs`
- Modify: `rust/neomacs-display/src/ffi.rs`

**Step 1: Add window-targeted rendering methods**

In backend.rs, add:

```rust
impl WinitBackend {
    pub fn begin_frame_for_window(&mut self, window_id: u32) {
        if let Some(state) = self.windows.get_mut(&window_id) {
            state.scene.clear();
        }
    }

    pub fn end_frame_for_window(&mut self, window_id: u32) {
        let state = match self.windows.get_mut(&window_id) {
            Some(s) => s,
            None => return,
        };

        let output = match state.surface.get_current_texture() {
            Ok(t) => t,
            Err(_) => return,
        };

        let view = output.texture.create_view(&wgpu::TextureViewDescriptor::default());

        self.renderer.render_to_view(
            &self.device,
            &self.queue,
            &view,
            &state.scene,
        );

        output.present();
        state.window.request_redraw();
    }

    pub fn get_scene_mut(&mut self, window_id: u32) -> Option<&mut Scene> {
        self.windows.get_mut(&window_id).map(|s| &mut s.scene)
    }
}
```

**Step 2: Update FFI to use window_id**

Modify existing `neomacs_display_begin_frame` and `neomacs_display_end_frame`:

```rust
#[no_mangle]
pub extern "C" fn neomacs_display_begin_frame_window(
    handle: *mut NeomacsDisplay,
    window_id: u32,
) {
    let display = unsafe { &mut *handle };

    #[cfg(feature = "winit-backend")]
    if let Some(ref mut backend) = display.winit_backend {
        backend.begin_frame_for_window(window_id);
    }
}

#[no_mangle]
pub extern "C" fn neomacs_display_end_frame_window(
    handle: *mut NeomacsDisplay,
    window_id: u32,
) {
    let display = unsafe { &mut *handle };

    #[cfg(feature = "winit-backend")]
    if let Some(ref mut backend) = display.winit_backend {
        backend.end_frame_for_window(window_id);
    }
}
```

**Step 3: Verify and commit**

```bash
cd rust/neomacs-display && cargo check --features winit-backend
git add rust/neomacs-display/src/backend/wgpu/backend.rs rust/neomacs-display/src/ffi.rs
git commit -m "feat: add window-targeted rendering"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Window FFI header | neomacs_display.h |
| 2 | WindowState struct | window_state.rs |
| 3 | Window management | backend.rs |
| 4 | Window FFI functions | ffi.rs |
| 5 | C code window creation | neomacsfns.c |
| 6 | Event types | events.rs |
| 7 | Event polling impl | backend.rs |
| 8 | Event polling FFI | ffi.rs |
| 9 | Emacs event integration | neomacsterm.c |
| 10 | Window rendering | backend.rs, ffi.rs |

After these 10 tasks, you should have a basic working winit window with keyboard input and rendering.
