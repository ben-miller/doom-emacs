;; Font.
;; (setq doom-font (font-spec :family "Iosevka" :size 18)
      ;; doom-variable-pitch-font (font-spec :family "Iosevka" :size 18)
      ;; doom-big-font (font-spec :family "Iosevka" :size 24))

(setq doom-font (font-spec :family "JetBrains Mono" :size 16 :weight 'Medium)
      doom-big-font (font-spec :family "JetBrains Mono" :size 20 :weight 'Medium)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 16))

(set-face-attribute 'default nil :family "Iosevka")

;; Minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 1)

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

;; (setq doom-theme 'doom-feather-light)
;; (setq doom-theme 'doom-flatwhite)
;; (setq doom-theme 'doom-nord-light)
