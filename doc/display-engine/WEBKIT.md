# Neomacs WebKit Integration

Neomacs provides a GPU-accelerated WebKit browser engine integration using WPE WebKit with the WPE Platform API. This allows embedding web content directly in Emacs buffers or as floating overlays.

## Overview

The WebKit integration uses:
- **WPE WebKit**: Headless WebKit port designed for embedded systems
- **WPE Platform API**: Modern API for GPU buffer handling and input events
- **DMA-BUF**: Zero-copy texture sharing for efficient rendering

## Quick Start

```elisp
;; Initialize the WebKit subsystem (required once)
(neomacs-webkit-init)

;; Create a webkit view (returns view ID)
(setq my-webkit (neomacs-webkit-create 800 600))

;; Load a URL
(neomacs-webkit-load-uri my-webkit "https://example.com")

;; Display as floating overlay
(neomacs-webkit-floating my-webkit 50 50 800 600)

;; Or insert inline at point
(insert (propertize " " 'display (neomacs-insert-webkit "https://example.com" 400 300 t)))
```

## Lisp API Reference

### Lifecycle Management

#### `(neomacs-webkit-init)`
Initialize the WebKit subsystem. Must be called before creating any webkit views.
Returns `t` on success, `nil` on failure.

#### `(neomacs-webkit-create WIDTH HEIGHT)`
Create a new WebKit view with the specified dimensions.
Returns the view ID (integer) on success, `nil` on failure.

#### `(neomacs-webkit-destroy VIEW-ID)`
Destroy a webkit view and release its resources.
Returns `t` on success.

### Navigation

#### `(neomacs-webkit-load-uri VIEW-ID URI)`
Load a URL in the webkit view.
```elisp
(neomacs-webkit-load-uri my-webkit "https://www.gnu.org")
```

#### `(neomacs-webkit-go-back VIEW-ID)`
Navigate back in the view's history.

#### `(neomacs-webkit-go-forward VIEW-ID)`
Navigate forward in the view's history.

#### `(neomacs-webkit-reload VIEW-ID)`
Reload the current page.

### Display

#### `(neomacs-webkit-floating VIEW-ID X Y WIDTH HEIGHT)`
Display the webkit view as a floating overlay at the specified position.
The overlay renders on top of all Emacs content.
```elisp
(neomacs-webkit-floating my-webkit 100 100 640 480)
```

#### `(neomacs-webkit-floating-clear VIEW-ID)`
Remove the floating webkit overlay.

#### `(neomacs-insert-webkit URI WIDTH HEIGHT &optional LOAD-P)`
Create a webkit view and return a display spec suitable for `propertize`.
If LOAD-P is non-nil, immediately loads the URI.
```elisp
;; Insert inline webkit at point
(insert (propertize " " 'display (neomacs-insert-webkit "https://example.com" 400 300 t)))
```

### Page Information

#### `(neomacs-webkit-get-title VIEW-ID)`
Returns the page title as a string, or `nil` if not available.

#### `(neomacs-webkit-get-url VIEW-ID)`
Returns the current URL as a string.

#### `(neomacs-webkit-get-progress VIEW-ID)`
Returns the loading progress as a float from 0.0 to 1.0.

#### `(neomacs-webkit-loading-p VIEW-ID)`
Returns non-nil if the view is currently loading.

### JavaScript Execution

#### `(neomacs-webkit-execute-js VIEW-ID SCRIPT)`
Execute JavaScript code in the webkit view.
```elisp
(neomacs-webkit-execute-js my-webkit "document.body.style.backgroundColor = 'lightblue'")
```

### Input Events

For floating webkit views, mouse events are automatically forwarded when the mouse is over the webkit area. For manual control or inline views, use these functions:

#### `(neomacs-webkit-send-key VIEW-ID KEY-CODE HARDWARE-KEY-CODE PRESSED &optional MODIFIERS)`
Send a keyboard event to the webkit view.
- KEY-CODE: XKB keysym
- HARDWARE-KEY-CODE: Physical scancode
- PRESSED: Non-nil for key down, nil for key up
- MODIFIERS: Bitmask (ctrl=1, shift=2, alt=4, meta=8)

#### `(neomacs-webkit-send-pointer VIEW-ID EVENT-TYPE X Y BUTTON STATE &optional MODIFIERS)`
Send a pointer/mouse event.
- EVENT-TYPE: 1 for motion, 2 for button
- X, Y: Coordinates relative to the view
- BUTTON: Mouse button (1=left, 2=middle, 3=right)
- STATE: Button state (1=pressed, 0=released)

#### `(neomacs-webkit-send-scroll VIEW-ID X Y DELTA-X DELTA-Y)`
Send a scroll event.
- X, Y: Position within the view
- DELTA-X: Horizontal scroll amount (positive = right)
- DELTA-Y: Vertical scroll amount (positive = down)

#### `(neomacs-webkit-click VIEW-ID X Y BUTTON)`
Convenience function to send a click at the specified position.
```elisp
(neomacs-webkit-click my-webkit 200 150 1)  ; Left click at (200,150)
```

### Callbacks

#### `(neomacs-webkit-set-load-callback FUNCTION)`
Set a callback for page load events.
```elisp
(neomacs-webkit-set-load-callback
 (lambda (view-id event uri)
   (message "WebKit %d: %s - %s" view-id event uri)))
```
EVENT is one of: `started`, `redirected`, `committed`, `finished`, `failed`.

#### `(neomacs-webkit-set-new-window-function FUNCTION)`
Set a callback for new window/tab requests (target="_blank", window.open()).
```elisp
(neomacs-webkit-set-new-window-function
 (lambda (view-id url frame-name)
   (message "New window request: %s" url)
   ;; Return t to indicate we handled it
   t))
```

## Event Handling

### Automatic Forwarding

When the mouse is over a floating webkit view, events are automatically forwarded:
- **Clicks**: Forwarded via `neomacs-webkit-click`
- **Motion**: Forwarded via `neomacs-webkit-send-pointer`
- **Scroll**: Forwarded via `neomacs-webkit-send-scroll`

This enables interactive web pages without additional Lisp code.

### Keyboard Input

Keyboard events are NOT automatically forwarded (Emacs needs them for editing).
To send keyboard input to a webkit view, use `neomacs-webkit-send-key` explicitly.

## Example: Simple Web Browser

```elisp
(defvar my-browser-id nil)

(defun my-browser-open (url)
  "Open URL in embedded browser."
  (interactive "sURL: ")
  (unless my-browser-id
    (neomacs-webkit-init)
    (setq my-browser-id (neomacs-webkit-create 1024 768)))
  (neomacs-webkit-load-uri my-browser-id url)
  (neomacs-webkit-floating my-browser-id 50 50 1024 768))

(defun my-browser-close ()
  "Close the browser overlay."
  (interactive)
  (when my-browser-id
    (neomacs-webkit-floating-clear my-browser-id)))

(defun my-browser-info ()
  "Show browser info."
  (interactive)
  (when my-browser-id
    (message "Title: %s\nURL: %s\nProgress: %.0f%%"
             (neomacs-webkit-get-title my-browser-id)
             (neomacs-webkit-get-url my-browser-id)
             (* 100 (neomacs-webkit-get-progress my-browser-id)))))
```

## Technical Notes

### GPU Rendering Pipeline

1. WebKit renders to WPE buffer (DMA-BUF)
2. Buffer is imported as GdkTexture via `wpe_buffer_import_to_pixels()`
3. Texture is rendered by GTK's snapshot/render node system
4. Zero-copy path available when DMA-BUF export is supported

### Memory Management

- Each webkit view consumes GPU memory for its texture
- Destroy unused views with `neomacs-webkit-destroy`
- Floating webkits are cleared when you call `neomacs-webkit-floating-clear`

### Limitations

- HTTPS requires `glib-networking` package
- Some WebKit features (WebGL, video acceleration) may require additional libraries
- Keyboard input must be explicitly forwarded
