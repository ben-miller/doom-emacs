(load (expand-file-name "config/settings/keybindings.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/org-mode.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/proj-persp.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/neotree.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/centaur-tabs.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/magit.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/org-trello.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/leetcode.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/evil.el" (file-name-directory load-file-name)))

;; -*- no-byte-compile: t; -*-

(add-hook 'doom-after-init-hook
          (lambda () (doom/quickload-session t)))

(defadvice! reload-with-tangle ()
  "Tangle README.org before reloading Doom Emacs."
  :before #'doom/reload
  (org-babel-tangle-file (expand-file-name "README.org" doom-private-dir)))

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
  (find-file "~/.doom.d/README.org")
  (neotree))

(setq frame-title-format
      '((:eval (if (projectile-project-p)
                   (projectile-project-name)
                 ""))))

(load (expand-file-name "config/settings/editor.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/settings/clipboard.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/settings/appearance.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/utils.el" (file-name-directory load-file-name)))
