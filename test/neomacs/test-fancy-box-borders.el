;;; test-fancy-box-borders.el --- Test all 10 fancy :box border styles -*- lexical-binding: t -*-

;; Load this file in neomacs to display a showcase buffer with all
;; fancy rounded-box border styles.
;;
;; Usage:
;;   M-x eval-buffer  or  (load "test/neomacs/test-fancy-box-borders.el")
;;
;; :box plist keys:
;;   :line-width N       — border thickness (pixels)
;;   :color COLOR        — primary border color
;;   :corner-radius N    — rounded corner radius (pixels)
;;   :border-style N     — fancy style (0-10, see below)
;;   :border-speed N     — animation speed (100 = 1.0x, 200 = 2.0x)
;;   :color2 COLOR       — secondary color (for gradient/neon)
;;
;; Styles:
;;   0 = Solid (default)      5 = Neon Double-Stroke
;;   1 = Rainbow               6 = Dashed
;;   2 = Animated Rainbow      7 = Comet Trail
;;   3 = Gradient (2-color)    8 = Iridescent
;;   4 = Pulsing Glow          9 = Fire / Plasma
;;                             10 = Heartbeat

;; ── Define a face for each style ────────────────────────────────────

(defface fancy-box-solid
  '((t :foreground "white"
       :box (:line-width 3 :color "dodger blue" :corner-radius 10
             :border-style 0 :border-speed 100)))
  "Style 0: Solid rounded box.")

(defface fancy-box-rainbow
  '((t :foreground "white"
       :box (:line-width 3 :color "white" :corner-radius 12
             :border-style 1 :border-speed 100)))
  "Style 1: Rainbow.")

(defface fancy-box-animated-rainbow
  '((t :foreground "white"
       :box (:line-width 3 :color "white" :corner-radius 12
             :border-style 2 :border-speed 100)))
  "Style 2: Animated Rainbow.")

(defface fancy-box-gradient
  '((t :foreground "white"
       :box (:line-width 3 :color "deep pink" :corner-radius 10
             :border-style 3 :border-speed 100 :color2 "cyan")))
  "Style 3: Gradient (pink → cyan).")

(defface fancy-box-glow
  '((t :foreground "white"
       :box (:line-width 2 :color "lime green" :corner-radius 10
             :border-style 4 :border-speed 100)))
  "Style 4: Pulsing Glow.")

(defface fancy-box-neon
  '((t :foreground "white"
       :box (:line-width 3 :color "magenta" :corner-radius 10
             :border-style 5 :border-speed 100 :color2 "cyan")))
  "Style 5: Neon Double-Stroke.")

(defface fancy-box-dashed
  '((t :foreground "white"
       :box (:line-width 3 :color "gold" :corner-radius 10
             :border-style 6 :border-speed 100)))
  "Style 6: Dashed.")

(defface fancy-box-comet
  '((t :foreground "white"
       :box (:line-width 3 :color "orange red" :corner-radius 12
             :border-style 7 :border-speed 100)))
  "Style 7: Comet Trail.")

(defface fancy-box-iridescent
  '((t :foreground "white"
       :box (:line-width 3 :color "steel blue" :corner-radius 10
             :border-style 8 :border-speed 100)))
  "Style 8: Iridescent.")

(defface fancy-box-fire
  '((t :foreground "white"
       :box (:line-width 3 :color "dark red" :corner-radius 10
             :border-style 9 :border-speed 100)))
  "Style 9: Fire / Plasma.")

(defface fancy-box-heartbeat
  '((t :foreground "white"
       :box (:line-width 2 :color "red" :corner-radius 10
             :border-style 10 :border-speed 100)))
  "Style 10: Heartbeat.")

;; ── Create showcase buffer ──────────────────────────────────────────

(defun fancy-box-demo ()
  "Create a buffer showing all fancy box border styles."
  (interactive)
  (let ((buf (get-buffer-create "*Fancy Box Borders*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Neomacs Fancy Rounded Box Borders\n\n"
                            'face '(:height 1.5 :weight bold)))

        ;; Style 0: Solid
        (insert "  ")
        (insert (propertize " 0: Solid " 'face 'fancy-box-solid))
        (insert "   ")

        ;; Style 1: Rainbow
        (insert (propertize " 1: Rainbow " 'face 'fancy-box-rainbow))
        (insert "   ")

        ;; Style 2: Animated Rainbow
        (insert (propertize " 2: Animated Rainbow " 'face 'fancy-box-animated-rainbow))
        (insert "\n\n\n  ")

        ;; Style 3: Gradient
        (insert (propertize " 3: Gradient (pink->cyan) " 'face 'fancy-box-gradient))
        (insert "   ")

        ;; Style 4: Pulsing Glow
        (insert (propertize " 4: Pulsing Glow " 'face 'fancy-box-glow))
        (insert "   ")

        ;; Style 5: Neon
        (insert (propertize " 5: Neon Double-Stroke " 'face 'fancy-box-neon))
        (insert "\n\n\n  ")

        ;; Style 6: Dashed
        (insert (propertize " 6: Dashed " 'face 'fancy-box-dashed))
        (insert "   ")

        ;; Style 7: Comet
        (insert (propertize " 7: Comet Trail " 'face 'fancy-box-comet))
        (insert "   ")

        ;; Style 8: Iridescent
        (insert (propertize " 8: Iridescent " 'face 'fancy-box-iridescent))
        (insert "\n\n\n  ")

        ;; Style 9: Fire
        (insert (propertize " 9: Fire / Plasma " 'face 'fancy-box-fire))
        (insert "   ")

        ;; Style 10: Heartbeat
        (insert (propertize " 10: Heartbeat " 'face 'fancy-box-heartbeat))
        (insert "\n\n\n")

        ;; Speed variations
        (insert (propertize "Speed Variations (Animated Rainbow):\n\n"
                            'face '(:height 1.2 :weight bold)))
        (insert "  ")
        (insert (propertize " 0.5x speed "
                            'face '(:foreground "white"
                                    :box (:line-width 3 :color "white" :corner-radius 10
                                          :border-style 2 :border-speed 50))))
        (insert "   ")
        (insert (propertize " 1.0x speed "
                            'face '(:foreground "white"
                                    :box (:line-width 3 :color "white" :corner-radius 10
                                          :border-style 2 :border-speed 100))))
        (insert "   ")
        (insert (propertize " 2.0x speed "
                            'face '(:foreground "white"
                                    :box (:line-width 3 :color "white" :corner-radius 10
                                          :border-style 2 :border-speed 200))))
        (insert "   ")
        (insert (propertize " 4.0x speed "
                            'face '(:foreground "white"
                                    :box (:line-width 3 :color "white" :corner-radius 10
                                          :border-style 2 :border-speed 400))))
        (insert "\n\n\n")

        ;; Different border widths
        (insert (propertize "Border Width Variations (Heartbeat):\n\n"
                            'face '(:height 1.2 :weight bold)))
        (insert "  ")
        (insert (propertize " 1px "
                            'face '(:foreground "white"
                                    :box (:line-width 1 :color "red" :corner-radius 8
                                          :border-style 10 :border-speed 100))))
        (insert "   ")
        (insert (propertize " 2px "
                            'face '(:foreground "white"
                                    :box (:line-width 2 :color "red" :corner-radius 10
                                          :border-style 10 :border-speed 100))))
        (insert "   ")
        (insert (propertize " 4px "
                            'face '(:foreground "white"
                                    :box (:line-width 4 :color "red" :corner-radius 12
                                          :border-style 10 :border-speed 100))))
        (insert "\n\n\n")

        ;; Color combinations
        (insert (propertize "Gradient Color Combos:\n\n"
                            'face '(:height 1.2 :weight bold)))
        (insert "  ")
        (insert (propertize " Sunset "
                            'face '(:foreground "white"
                                    :box (:line-width 3 :color "red" :corner-radius 10
                                          :border-style 3 :border-speed 100 :color2 "gold"))))
        (insert "   ")
        (insert (propertize " Ocean "
                            'face '(:foreground "white"
                                    :box (:line-width 3 :color "navy" :corner-radius 10
                                          :border-style 3 :border-speed 100 :color2 "aquamarine"))))
        (insert "   ")
        (insert (propertize " Forest "
                            'face '(:foreground "white"
                                    :box (:line-width 3 :color "dark green" :corner-radius 10
                                          :border-style 3 :border-speed 100 :color2 "lime green"))))
        (insert "   ")
        (insert (propertize " Galaxy "
                            'face '(:foreground "white"
                                    :box (:line-width 3 :color "purple" :corner-radius 10
                                          :border-style 3 :border-speed 100 :color2 "hot pink"))))
        (insert "\n"))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (switch-to-buffer buf)))

;; Run the demo
(fancy-box-demo)

(provide 'test-fancy-box-borders)
;;; test-fancy-box-borders.el ends here
