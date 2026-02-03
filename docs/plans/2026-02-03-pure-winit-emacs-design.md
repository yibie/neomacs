# Pure Winit Emacs Integration Design

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace GTK windowing with winit, making Emacs render entirely via wgpu.

**Architecture:** Emacs keeps its event loop, calls into Rust for window management and rendering. Rust uses winit's `pump_events()` for non-blocking event polling.

**Tech Stack:** winit 0.30+, wgpu, Emacs C FFI

---

## 1. Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                      Emacs (C)                          │
│  ┌─────────────┐    ┌──────────────┐    ┌───────────┐  │
│  │ Event Loop  │───▶│ read_socket  │───▶│ Redisplay │  │
│  └─────────────┘    └──────────────┘    └───────────┘  │
│         │                  ▲                   │        │
│         ▼                  │                   ▼        │
│  ┌─────────────────────────┴───────────────────────┐   │
│  │              FFI (neomacs_display_*)             │   │
│  └─────────────────────────┬───────────────────────┘   │
└─────────────────────────────┼───────────────────────────┘
                              │
┌─────────────────────────────┼───────────────────────────┐
│                      Rust                               │
│  ┌──────────────┐    ┌─────┴─────┐    ┌─────────────┐  │
│  │ WinitBackend │◀──▶│pump_events│───▶│ Event Queue │  │
│  └──────────────┘    └───────────┘    └─────────────┘  │
│         │                                               │
│         ▼                                               │
│  ┌──────────────┐    ┌───────────┐                     │
│  │ wgpu Renderer│───▶│  Window   │                     │
│  └──────────────┘    └───────────┘                     │
└─────────────────────────────────────────────────────────┘
```

**Key insight:** winit's `pump_events(Duration::ZERO)` returns immediately, making it compatible with Emacs's event loop.

---

## 2. Window Creation and Lifecycle

**New FFI functions:**

```c
// Create a winit window, returns window_id
uint32_t neomacs_display_create_window(
    void* display_handle,
    int width,
    int height,
    const char* title
);

// Destroy window
void neomacs_display_destroy_window(void* display_handle, uint32_t window_id);

// Show/hide window
void neomacs_display_show_window(void* display_handle, uint32_t window_id, bool visible);

// Set window title
void neomacs_display_set_title(void* display_handle, uint32_t window_id, const char* title);

// Resize window
void neomacs_display_set_window_size(void* display_handle, uint32_t window_id, int w, int h);
```

**Rust implementation:**

```rust
impl WinitBackend {
    pub fn create_window(&mut self, width: u32, height: u32, title: &str) -> u32 {
        let window = self.event_loop.create_window(
            WindowAttributes::default()
                .with_title(title)
                .with_inner_size(PhysicalSize::new(width, height))
        ).unwrap();

        let window_id = self.next_window_id;
        self.windows.insert(window_id, WindowState::new(window));
        self.next_window_id += 1;
        window_id
    }
}
```

---

## 3. Event Loop Integration

**New FFI function:**

```c
// Poll for window events, returns number of events queued
int neomacs_display_poll_events(void* display_handle);
```

**Rust implementation:**

```rust
impl WinitBackend {
    pub fn poll_events(&mut self, event_callback: impl FnMut(NeomacsEvent)) {
        use winit::platform::pump_events::EventLoopExtPumpEvents;

        self.event_loop.pump_events(Some(Duration::ZERO), |event, target| {
            match event {
                Event::WindowEvent { window_id, event } => {
                    match event {
                        WindowEvent::KeyboardInput { event, .. } => { /* translate */ }
                        WindowEvent::MouseInput { button, state, .. } => { /* translate */ }
                        WindowEvent::CursorMoved { position, .. } => { /* track */ }
                        WindowEvent::Resized(size) => { /* queue resize */ }
                        WindowEvent::CloseRequested => { /* queue close */ }
                        _ => {}
                    }
                }
                Event::AboutToWait => {
                    target.set_control_flow(ControlFlow::Poll);
                }
                _ => {}
            }
        });
    }
}
```

**C side integration (neomacsterm.c):**

```c
static int
neomacs_read_socket(struct terminal *terminal, struct input_event *hold_quit)
{
    struct neomacs_display_info *dpyinfo = terminal->display_info.neomacs;
    neomacs_display_poll_events(dpyinfo->display_handle);
    return neomacs_evq_flush(hold_quit);
}
```

---

## 4. Keyboard and Mouse Event Translation

**Event structure (Rust → C):**

```rust
#[repr(C)]
pub struct NeomacsInputEvent {
    pub kind: u32,           // KEY_PRESS, MOUSE_CLICK, MOUSE_MOVE, etc.
    pub window_id: u32,
    pub timestamp: u64,
    pub x: i32,
    pub y: i32,
    pub keycode: u32,
    pub modifiers: u32,      // Ctrl, Alt, Shift, Super
    pub button: u32,
    pub keysym: u32,         // X11-style keysym
}
```

**Modifier translation:**

```rust
fn translate_modifiers(mods: &ModifiersState) -> u32 {
    let mut result = 0;
    if mods.shift_key() { result |= NEOMACS_SHIFT_MASK; }
    if mods.control_key() { result |= NEOMACS_CTRL_MASK; }
    if mods.alt_key() { result |= NEOMACS_META_MASK; }
    if mods.super_key() { result |= NEOMACS_SUPER_MASK; }
    result
}
```

**Key translation to X11 keysyms:**

```rust
fn translate_key(key: &Key) -> u32 {
    match key {
        Key::Character(c) => c.chars().next().unwrap() as u32,
        Key::Named(named) => match named {
            NamedKey::Enter => 0xFF0D,
            NamedKey::Tab => 0xFF09,
            NamedKey::Backspace => 0xFF08,
            NamedKey::Escape => 0xFF1B,
            NamedKey::ArrowUp => 0xFF52,
            NamedKey::ArrowDown => 0xFF54,
            NamedKey::ArrowLeft => 0xFF51,
            NamedKey::ArrowRight => 0xFF53,
            // ... etc
        }
    }
}
```

---

## 5. Rendering Integration

**Render flow:**

```
Emacs redisplay_internal()
    → neomacs_display_begin_frame(window_id)
    → neomacs_display_draw_glyph_string() [many calls]
    → neomacs_display_end_frame(window_id)
        → Rust: renderer.render(&scene)
        → wgpu draws to window surface
        → surface.present()
```

**Window state:**

```rust
struct WindowState {
    window: Window,
    surface: wgpu::Surface,
    config: wgpu::SurfaceConfiguration,
    scene: Scene,
}

impl WindowState {
    fn resize(&mut self, device: &wgpu::Device, width: u32, height: u32) {
        self.config.width = width;
        self.config.height = height;
        self.surface.configure(device, &self.config);
    }
}
```

---

## 6. C Code Changes

**Files to modify:**

| File | Changes |
|------|---------|
| `src/neomacsfns.c` | Remove GTK window creation, use `neomacs_display_create_window()` |
| `src/neomacsterm.c` | Add `neomacs_display_poll_events()` to `read_socket` |
| `src/neomacsterm.h` | Add `window_id` field to frame structure |
| `configure.ac` | Remove GTK dependency requirement for neomacs |

**Before (GTK):**
```c
window = gtk_window_new();
gtk_window_set_title(GTK_WINDOW(window), "Emacs");
drawing_area = neomacs_display_create_widget();
// ... 100+ lines of GTK setup
```

**After (winit):**
```c
f->window_id = neomacs_display_create_window(
    dpyinfo->display_handle,
    FRAME_PIXEL_WIDTH(f),
    FRAME_PIXEL_HEIGHT(f),
    "Emacs"
);
neomacs_display_show_window(dpyinfo->display_handle, f->window_id, true);
```

---

## 7. Implementation Phases

| Phase | Goal | Test |
|-------|------|------|
| 1. Window Creation | Empty winit window appears | `./src/emacs` shows window |
| 2. Basic Rendering | Emacs content visible | See text in window |
| 3. Keyboard Input | Can type | Characters appear |
| 4. Mouse Input | Click and scroll work | Select text, scroll buffer |
| 5. Window Management | Resize/close work | Full window lifecycle |
| 6. Cleanup | Remove GTK | Build without GTK |

Each phase is independently testable.

---

## 8. Dependencies

**Rust crates:**
- `winit = "0.30"` (already have)
- `wgpu = "23"` (already have)

**Removed:**
- GTK4 (libgtk-4-dev)
- All gdk4/gsk4/pango dependencies

**Build changes:**
- `configure.ac`: Remove `--with-gtk` requirement for neomacs
- Can still build traditional Emacs with GTK, just not neomacs backend
