(add-hook 'doom-after-init-hook
          (lambda () (doom/quickload-session t)))

;; Don't prompt when exiting.
(setq confirm-kill-emacs nil)

;; Visual line mode
(global-visual-line-mode)

;; Blink cursor mode.
(blink-cursor-mode 1)

;; Disable highlight line mode.
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; Disable flyspell.
(remove-hook 'text-mode-hook #'flyspell-mode)
(remove-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Suppress warnings.
(setq warning-suppress-types
      '((obsolete)
        (bytecomp)
        (bytecomp . buffer-local-value)))
(setq enable-local-variables :all)

(after! emacs
  (find-file "~/.doom.d/config/settings/keybindings.el")
  (neotree))

(setq frame-title-format
      '((:eval (if (projectile-project-p)
                   (projectile-project-name)
                 ""))))
