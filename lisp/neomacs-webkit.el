;;; neomacs-webkit.el --- WebKit browser support for Neomacs -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; Author: Neomacs Contributors
;; Keywords: web, browser, webkit

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides WebKit browser embedding support for Neomacs.
;; It uses WPE WebKit for headless rendering with GPU acceleration.
;;
;; Basic usage:
;;   (neomacs-webkit-browse "https://example.com")
;;
;; API functions:
;;   `neomacs-webkit-browse' - Open URL in a new browser view
;;   `neomacs-webkit-back' - Go back in history
;;   `neomacs-webkit-forward' - Go forward in history
;;   `neomacs-webkit-reload' - Reload current page
;;   `neomacs-webkit-eval-js' - Execute JavaScript

;;; Code:

(defgroup neomacs-webkit nil
  "WebKit browser embedding for Neomacs."
  :group 'web
  :prefix "neomacs-webkit-")

(defcustom neomacs-webkit-default-width 800
  "Default width for WebKit views."
  :type 'integer
  :group 'neomacs-webkit)

(defcustom neomacs-webkit-default-height 600
  "Default height for WebKit views."
  :type 'integer
  :group 'neomacs-webkit)

(defvar neomacs-webkit--views (make-hash-table :test 'eq)
  "Hash table mapping view IDs to their metadata.")

(defvar neomacs-webkit--initialized nil
  "Whether the WebKit subsystem has been initialized.")

(defcustom neomacs-webkit-new-window-action 'new-buffer
  "Action to take when WebKit requests a new window.
Possible values:
  `new-buffer' - Open URL in a new webkit buffer (default)
  `same-buffer' - Load URL in the current webkit view
  `external' - Open URL in external browser
  `ignore' - Ignore the request
  `ask' - Ask user what to do"
  :type '(choice (const :tag "Open in new buffer" new-buffer)
                 (const :tag "Load in same view" same-buffer)
                 (const :tag "Open in external browser" external)
                 (const :tag "Ignore" ignore)
                 (const :tag "Ask user" ask))
  :group 'neomacs-webkit)

(defun neomacs-webkit--handle-new-window (view-id url frame-name)
  "Handle a new window request from WebKit VIEW-ID.
URL is the requested URL, FRAME-NAME is the target attribute (e.g., \"_blank\")."
  (message "WebKit new window request: view=%d url=%s frame=%s"
           view-id url frame-name)
  (pcase neomacs-webkit-new-window-action
    ('new-buffer
     ;; Open URL in a new webkit buffer
     (let ((new-view-id (neomacs-webkit-browse url)))
       (when new-view-id
         (message "Opened %s in new webkit view %d" url new-view-id)))
     t)
    ('same-buffer
     ;; Load URL in the same view
     (neomacs-webkit-load-uri view-id url)
     (message "Loading %s in view %d" url view-id)
     t)
    ('external
     ;; Open in external browser
     (browse-url url)
     (message "Opened %s in external browser" url)
     t)
    ('ignore
     ;; Ignore the request
     (message "Ignored new window request for %s" url)
     nil)
    ('ask
     ;; Ask user what to do
     (let ((choice (completing-read
                    (format "New window request for %s: " url)
                    '("Open in new buffer" "Load in same view"
                      "Open in external browser" "Ignore")
                    nil t)))
       (pcase choice
         ("Open in new buffer"
          (neomacs-webkit-browse url)
          t)
         ("Load in same view"
          (neomacs-webkit-load-uri view-id url)
          t)
         ("Open in external browser"
          (browse-url url)
          t)
         (_
          nil))))
    (_
     ;; Default: open in new buffer
     (neomacs-webkit-browse url)
     t)))

(defun neomacs-webkit--ensure-initialized ()
  "Ensure the WebKit subsystem is initialized."
  (unless neomacs-webkit--initialized
    (when (neomacs-webkit-init)
      ;; Set up the new window callback
      (neomacs-webkit-set-new-window-function #'neomacs-webkit--handle-new-window)
      (setq neomacs-webkit--initialized t))))

(defun neomacs-webkit-browse (url &optional width height)
  "Open URL in a new WebKit browser view.
Optional WIDTH and HEIGHT specify view dimensions.
Returns the view ID on success, nil on failure."
  (interactive "sURL: ")
  (neomacs-webkit--ensure-initialized)
  (let* ((w (or width neomacs-webkit-default-width))
         (h (or height neomacs-webkit-default-height))
         (view-id (neomacs-webkit-create w h)))
    (when view-id
      (neomacs-webkit-load-uri view-id url)
      (puthash view-id `(:url ,url :width ,w :height ,h) neomacs-webkit--views)
      (message "WebKit view %d: %s" view-id url))
    view-id))

(defun neomacs-webkit-back (view-id)
  "Go back in history for VIEW-ID."
  (interactive "nView ID: ")
  (neomacs-webkit-go-back view-id))

(defun neomacs-webkit-forward (view-id)
  "Go forward in history for VIEW-ID."
  (interactive "nView ID: ")
  (neomacs-webkit-go-forward view-id))

(defun neomacs-webkit-eval-js (view-id script)
  "Execute JavaScript SCRIPT in VIEW-ID."
  (interactive "nView ID: \nsJavaScript: ")
  (neomacs-webkit-execute-js view-id script))

(defun neomacs-webkit-close (view-id)
  "Close WebKit view VIEW-ID."
  (interactive "nView ID: ")
  (when (neomacs-webkit-destroy view-id)
    (remhash view-id neomacs-webkit--views)
    (message "WebKit view %d closed" view-id)))

(defun neomacs-webkit-close-all ()
  "Close all WebKit views."
  (interactive)
  (maphash (lambda (id _info)
             (neomacs-webkit-destroy id))
           neomacs-webkit--views)
  (clrhash neomacs-webkit--views)
  (message "All WebKit views closed"))

(defun neomacs-webkit-show-floating (view-id x y &optional width height)
  "Show VIEW-ID as floating overlay at position (X, Y).
Optional WIDTH and HEIGHT override stored dimensions."
  (interactive "nView ID: \nnX position: \nnY position: ")
  (let* ((info (gethash view-id neomacs-webkit--views))
         (w (or width (plist-get info :width) neomacs-webkit-default-width))
         (h (or height (plist-get info :height) neomacs-webkit-default-height)))
    (neomacs-webkit-floating view-id x y w h)))

(defun neomacs-webkit-hide-floating (view-id)
  "Hide floating overlay for VIEW-ID."
  (interactive "nView ID: ")
  (neomacs-webkit-floating-clear view-id))

;;; Browser buffer mode

(defvar-local neomacs-webkit-buffer-view-id nil
  "The WebKit view ID associated with this buffer.")

(defvar neomacs-webkit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'neomacs-webkit-mode-reload)
    (define-key map "r" #'neomacs-webkit-mode-reload)
    (define-key map "B" #'neomacs-webkit-mode-back)
    (define-key map "F" #'neomacs-webkit-mode-forward)
    (define-key map "q" #'neomacs-webkit-mode-quit)
    (define-key map "o" #'neomacs-webkit-mode-open)
    (define-key map "i" #'neomacs-webkit-mode-toggle-input)  ; Enter input mode
    (define-key map [mouse-1] #'neomacs-webkit-mode-mouse-click)
    (define-key map [wheel-up] #'neomacs-webkit-mode-scroll-up)
    (define-key map [wheel-down] #'neomacs-webkit-mode-scroll-down)
    map)
  "Keymap for `neomacs-webkit-mode'.
Press 'i' to enter input mode for typing in web forms.")

(defvar neomacs-webkit-mode-line-format
  '(:eval (neomacs-webkit--mode-line-string))
  "Mode line format for WebKit buffers.")

(defun neomacs-webkit--mode-line-string ()
  "Generate mode line string for WebKit buffer."
  (if neomacs-webkit-buffer-view-id
      (let ((title (neomacs-webkit-get-title neomacs-webkit-buffer-view-id))
            (progress (neomacs-webkit-get-progress neomacs-webkit-buffer-view-id))
            (loading (neomacs-webkit-loading-p neomacs-webkit-buffer-view-id)))
        (format " [%s%s]"
                (or title "WebKit")
                (if loading
                    (format " %.0f%%" (* 100 (or progress 0)))
                  "")))
    " [WebKit]"))

(define-derived-mode neomacs-webkit-mode special-mode "WebKit"
  "Major mode for browsing the web with WebKit.
\\{neomacs-webkit-mode-map}"
  (setq buffer-read-only t)
  (setq mode-line-format
        (list "%e" mode-line-front-space
              neomacs-webkit-mode-line-format
              " " mode-line-buffer-identification
              mode-line-end-spaces)))

(defun neomacs-webkit-mode-reload ()
  "Reload the current page."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-reload neomacs-webkit-buffer-view-id)
    (message "Reloading...")))

(defun neomacs-webkit-mode-back ()
  "Go back in history."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-go-back neomacs-webkit-buffer-view-id)))

(defun neomacs-webkit-mode-forward ()
  "Go forward in history."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-go-forward neomacs-webkit-buffer-view-id)))

(defun neomacs-webkit-mode-quit ()
  "Close the WebKit browser buffer."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-floating-clear neomacs-webkit-buffer-view-id)
    (neomacs-webkit-close neomacs-webkit-buffer-view-id)
    (setq neomacs-webkit-buffer-view-id nil))
  (kill-buffer))

(defun neomacs-webkit-mode-open (url)
  "Open URL in the current WebKit view."
  (interactive "sURL: ")
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-load-uri neomacs-webkit-buffer-view-id url)
    (message "Loading: %s" url)))

(defvar-local neomacs-webkit--floating-position nil
  "Position and size of floating WebKit view (x y width height).")

(defun neomacs-webkit-mode-mouse-click (event)
  "Handle mouse click EVENT in WebKit view."
  (interactive "e")
  (when (and neomacs-webkit-buffer-view-id neomacs-webkit--floating-position)
    (let* ((pos (posn-x-y (event-start event)))
           (click-x (car pos))
           (click-y (cdr pos))
           (float-x (nth 0 neomacs-webkit--floating-position))
           (float-y (nth 1 neomacs-webkit--floating-position))
           ;; Translate to WebKit view coordinates
           (view-x (- click-x float-x))
           (view-y (- click-y float-y)))
      (neomacs-webkit-click neomacs-webkit-buffer-view-id view-x view-y 1))))

(defun neomacs-webkit-mode-scroll-up (event)
  "Handle scroll up EVENT in WebKit view."
  (interactive "e")
  (when (and neomacs-webkit-buffer-view-id neomacs-webkit--floating-position)
    (let* ((pos (posn-x-y (event-start event)))
           (float-x (nth 0 neomacs-webkit--floating-position))
           (float-y (nth 1 neomacs-webkit--floating-position))
           (view-x (- (car pos) float-x))
           (view-y (- (cdr pos) float-y)))
      (neomacs-webkit-send-scroll neomacs-webkit-buffer-view-id view-x view-y 0 -50))))

(defun neomacs-webkit-mode-scroll-down (event)
  "Handle scroll down EVENT in WebKit view."
  (interactive "e")
  (when (and neomacs-webkit-buffer-view-id neomacs-webkit--floating-position)
    (let* ((pos (posn-x-y (event-start event)))
           (float-x (nth 0 neomacs-webkit--floating-position))
           (float-y (nth 1 neomacs-webkit--floating-position))
           (view-x (- (car pos) float-x))
           (view-y (- (cdr pos) float-y)))
      (neomacs-webkit-send-scroll neomacs-webkit-buffer-view-id view-x view-y 0 50))))

;; Keyboard input support for text entry in web forms

(defvar neomacs-webkit--input-mode nil
  "Non-nil when input mode is active (for typing in web forms).")

(defun neomacs-webkit-mode-toggle-input ()
  "Toggle input mode for typing in web forms.
When enabled, most keys are forwarded to the WebKit view."
  (interactive)
  (setq neomacs-webkit--input-mode (not neomacs-webkit--input-mode))
  (if neomacs-webkit--input-mode
      (progn
        (message "WebKit input mode ON - type to enter text, C-c C-c to exit")
        (neomacs-webkit--enable-input-mode))
    (message "WebKit input mode OFF")
    (neomacs-webkit--disable-input-mode)))

(defun neomacs-webkit--enable-input-mode ()
  "Enable input mode keybindings."
  ;; Use a minor mode map that captures most keys
  (setq-local neomacs-webkit--saved-keymap (current-local-map))
  (use-local-map neomacs-webkit-input-mode-map))

(defun neomacs-webkit--disable-input-mode ()
  "Disable input mode keybindings."
  (when neomacs-webkit--saved-keymap
    (use-local-map neomacs-webkit--saved-keymap)
    (setq-local neomacs-webkit--saved-keymap nil)))

(defvar neomacs-webkit--saved-keymap nil
  "Saved keymap before entering input mode.")

(defun neomacs-webkit--char-to-keysym (char)
  "Convert CHAR to XKB keysym."
  (cond
   ;; Letters
   ((and (>= char ?a) (<= char ?z)) char)
   ((and (>= char ?A) (<= char ?Z)) char)
   ;; Numbers
   ((and (>= char ?0) (<= char ?9)) char)
   ;; Space
   ((= char ?\s) #x0020)
   ;; Common punctuation
   ((= char ?.) #x002e)
   ((= char ?,) #x002c)
   ((= char ?!) #x0021)
   ((= char ??) #x003f)
   ((= char ?:) #x003a)
   ((= char ?\;) #x003b)
   ((= char ?') #x0027)
   ((= char ?\") #x0022)
   ((= char ?-) #x002d)
   ((= char ?_) #x005f)
   ((= char ?=) #x003d)
   ((= char ?+) #x002b)
   ((= char ?/) #x002f)
   ((= char ?\\) #x005c)
   ((= char ?@) #x0040)
   ((= char ?#) #x0023)
   ((= char ?$) #x0024)
   ((= char ?%) #x0025)
   ((= char ?^) #x005e)
   ((= char ?&) #x0026)
   ((= char ?*) #x002a)
   ((= char ?\() #x0028)
   ((= char ?\)) #x0029)
   ((= char ?\[) #x005b)
   ((= char ?\]) #x005d)
   ((= char ?{) #x007b)
   ((= char ?}) #x007d)
   ((= char ?<) #x003c)
   ((= char ?>) #x003e)
   ((= char ?|) #x007c)
   ((= char ?`) #x0060)
   ((= char ?~) #x007e)
   ;; Default: use char code as keysym
   (t char)))

(defun neomacs-webkit-input-self-insert ()
  "Forward current key to WebKit as text input."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (let* ((char last-command-event)
           (keysym (neomacs-webkit--char-to-keysym char))
           (modifiers 0))
      ;; Send key down then key up
      (neomacs-webkit-send-key neomacs-webkit-buffer-view-id keysym 0 t modifiers)
      (neomacs-webkit-send-key neomacs-webkit-buffer-view-id keysym 0 nil modifiers))))

(defun neomacs-webkit-input-return ()
  "Send Return/Enter to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff0d 36 t 0)  ; XK_Return
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff0d 36 nil 0)))

(defun neomacs-webkit-input-backspace ()
  "Send Backspace to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff08 22 t 0)  ; XK_BackSpace
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff08 22 nil 0)))

(defun neomacs-webkit-input-tab ()
  "Send Tab to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff09 23 t 0)  ; XK_Tab
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff09 23 nil 0)))

(defun neomacs-webkit-input-escape ()
  "Send Escape to WebKit and exit input mode."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff1b 9 t 0)  ; XK_Escape
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff1b 9 nil 0))
  (neomacs-webkit-mode-toggle-input))

(defun neomacs-webkit-input-arrow-up ()
  "Send Up arrow to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff52 111 t 0)
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff52 111 nil 0)))

(defun neomacs-webkit-input-arrow-down ()
  "Send Down arrow to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff54 116 t 0)
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff54 116 nil 0)))

(defun neomacs-webkit-input-arrow-left ()
  "Send Left arrow to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff51 113 t 0)
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff51 113 nil 0)))

(defun neomacs-webkit-input-arrow-right ()
  "Send Right arrow to WebKit."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff53 114 t 0)
    (neomacs-webkit-send-key neomacs-webkit-buffer-view-id #xff53 114 nil 0)))

(defvar neomacs-webkit-input-mode-map
  (let ((map (make-keymap)))
    ;; Self-insert for printable characters
    (dotimes (i 128)
      (when (and (>= i 32) (/= i 127))  ; printable ASCII
        (define-key map (vector i) #'neomacs-webkit-input-self-insert)))
    ;; Special keys
    (define-key map (kbd "RET") #'neomacs-webkit-input-return)
    (define-key map (kbd "DEL") #'neomacs-webkit-input-backspace)
    (define-key map (kbd "<backspace>") #'neomacs-webkit-input-backspace)
    (define-key map (kbd "TAB") #'neomacs-webkit-input-tab)
    (define-key map (kbd "<up>") #'neomacs-webkit-input-arrow-up)
    (define-key map (kbd "<down>") #'neomacs-webkit-input-arrow-down)
    (define-key map (kbd "<left>") #'neomacs-webkit-input-arrow-left)
    (define-key map (kbd "<right>") #'neomacs-webkit-input-arrow-right)
    ;; Exit input mode
    (define-key map (kbd "ESC") #'neomacs-webkit-input-escape)
    (define-key map (kbd "C-c C-c") #'neomacs-webkit-mode-toggle-input)
    ;; Keep mouse bindings
    (define-key map [mouse-1] #'neomacs-webkit-mode-mouse-click)
    (define-key map [wheel-up] #'neomacs-webkit-mode-scroll-up)
    (define-key map [wheel-down] #'neomacs-webkit-mode-scroll-down)
    map)
  "Keymap for WebKit input mode (typing in web forms).")

(defun neomacs-webkit-mode-show-fullscreen ()
  "Show WebKit view as fullscreen floating overlay."
  (interactive)
  (when neomacs-webkit-buffer-view-id
    (let* ((frame (selected-frame))
           (width (frame-pixel-width frame))
           (height (frame-pixel-height frame)))
      (setq neomacs-webkit--floating-position (list 0 0 width height))
      (neomacs-webkit-floating neomacs-webkit-buffer-view-id 0 0 width height))))

;;;###autoload
(defun neomacs-webkit-open-url (url)
  "Open URL in a new WebKit browser buffer."
  (interactive "sURL: ")
  (neomacs-webkit--ensure-initialized)
  (let* ((buffer (generate-new-buffer "*WebKit*"))
         (view-id (neomacs-webkit-browse url)))
    (when view-id
      (with-current-buffer buffer
        (neomacs-webkit-mode)
        (setq neomacs-webkit-buffer-view-id view-id)
        (rename-buffer (format "*WebKit: %s*" url) t))
      (pop-to-buffer buffer)
      (message "WebKit browser opened: %s" url))
    buffer))

(provide 'neomacs-webkit)

;;; neomacs-webkit.el ends here
