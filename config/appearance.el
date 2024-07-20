;; Font.
(setq doom-font (font-spec :family "Iosevka" :size 18)
      doom-variable-pitch-font (font-spec :family "Iosevka" :size 18)
      doom-big-font (font-spec :family "Iosevka" :size 26))

;; Disable line numbers.
(setq display-line-numbers-type nil)

;; Minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(load-theme 'whiteboard t)

(set-face-background 'default "#ffffff")

(set-face-attribute 'default nil :family "Iosevka")

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))
