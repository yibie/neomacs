;;; neomacs-webkit-test.el --- Test inline WebKit rendering in Neomacs -*- lexical-binding: t -*-

;; This test verifies that WebKit views are rendered inline in buffers
;; using WPE WebKit with GPU acceleration.

;;; Commentary:
;; Run with: ./test/manual/run-webkit-test.sh
;; Or manually: DISPLAY=:0 ./src/emacs -Q -l test/manual/neomacs-webkit-test.el
;;
;; Neomacs renders WebKit views inline in buffers (not as floating overlays).
;; This follows the standard Emacs xwidget pattern:
;;
;;   (insert (propertize " " 'display (neomacs-insert-webkit url width height t)))
;;
;; The webkit view becomes part of the buffer content, scrolls naturally,
;; and respects Emacs window management.

;;; Code:

;; Load neomacs-webkit for auto-resize support
(require 'neomacs-webkit nil t)

(defvar neomacs-webkit-test-url "https://www.google.com/"
  "URL to load for testing.")

(defvar neomacs-webkit-test-width 0
  "Width of test WebKit view (0 = auto-fit to window).")

(defvar neomacs-webkit-test-height 0
  "Height of test WebKit view (0 = auto from aspect ratio).")

(defun neomacs-webkit-test-run ()
  "Run the inline WebKit rendering test."
  (interactive)
  (switch-to-buffer (get-buffer-create "*WebKit Test*"))
  (erase-buffer)

  (insert "=== Neomacs Inline WebKit Test ===\n\n")

  (condition-case err
      (progn
        (insert "Initializing WebKit subsystem...\n")
        (neomacs-webkit-init)
        (insert "WebKit initialized.\n\n")

        ;; Calculate dimensions (0 = auto-fit to window)
        (let* ((margin 16)
               (aspect-ratio (/ 16.0 9.0))
               (width (if (and neomacs-webkit-test-width (> neomacs-webkit-test-width 0))
                          neomacs-webkit-test-width
                        (- (window-body-width nil t) margin)))
               (height (if (and neomacs-webkit-test-height (> neomacs-webkit-test-height 0))
                           neomacs-webkit-test-height
                         (round (/ width aspect-ratio)))))
          (insert (format "Loading %s inline (%dx%d)...\n\n"
                          neomacs-webkit-test-url width height))

          ;; Create inline webkit view
          (let ((spec (neomacs-insert-webkit neomacs-webkit-test-url
                                             width
                                             height
                                             t)))
            (if spec
                (progn
                  ;; Insert the webkit view inline and track position
                  (let ((pos (point))
                        (view-id (plist-get (cdr spec) :id)))
                    (insert (propertize " " 'display spec))
                    ;; Register for auto-resize tracking
                    (neomacs-webkit--register-view pos view-id)
                    ;; Enable auto-resize hook
                    (neomacs-webkit-enable-auto-resize))
                  (insert "\n\n")
                  (insert (format "WebKit spec: %S\n\n" spec))
                  (insert "SUCCESS! Inline WebKit rendering works.\n")
                  (insert "Auto-resize enabled - try resizing the window!\n")
                  (insert "\nControls (use view ID from spec above):\n")
                  (insert "  (neomacs-webkit-load-uri ID \"url\") - load new URL\n")
                  (insert "  (neomacs-webkit-go-back ID) - go back\n")
                  (insert "  (neomacs-webkit-go-forward ID) - go forward\n")
                  (insert "  (neomacs-webkit-resize ID w h) - resize view\n")
                  (insert "  (neomacs-webkit-reload ID) - reload\n"))
              (insert "FAILED: neomacs-insert-webkit returned nil\n")))))
    (error
     (insert (format "ERROR: %S\n" err))))

  (goto-char (point-min))
  (redisplay t)

  ;; Auto-exit after delay when run non-interactively
  (when noninteractive
    (run-at-time 15 nil (lambda () (kill-emacs 0)))))

;; Auto-run when loaded
(add-hook 'emacs-startup-hook #'neomacs-webkit-test-run)

(provide 'neomacs-webkit-test)
;;; neomacs-webkit-test.el ends here
