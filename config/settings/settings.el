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

(add-hook 'emacs-startup-hook
          (lambda ()
            (find-file "~/.doom.d/tasks.org")
            (org-mode-restart)
            (neotree)
            (find-file "~/.doom.d/tasks.org")))

(setq frame-persp-debug
      '((:eval (if (projectile-project-p)
                   (concat
                    " Persp:" (persp-name (persp-curr))
                    " Proj:" (projectile-project-name)
                    " Buf-groups:" (prin1-to-string (centaur-tabs-buffer-groups))
                    " Get-groups:" (prin1-to-string (centaur-tabs-get-groups))
                    " Cent-proj:" (centaur-tabs-project-name)
                    " Proj-curr:" (prin1-to-string (cdr (project-current)))
                    " Tabset:" (prin1-to-string centaur-tabs-current-tabset)
                 )))))

(setq frame-title-format
      '((:eval (if (projectile-project-p)
                   (concat
                    "Perspective: " (persp-name (persp-curr))
                    " <=> Project: " (projectile-project-name)
                 )))))

(setq consult-fd-args
  (append
    (list (if (executable-find "fdfind" 'remote) "fdfind" "fd")
          "--color=never" "--full-path" "--absolute-path" "--hidden" "--no-ignore" "--exclude" ".git")))

(defun try-eval (expr)
  "Evaluate EXPR and insert the result. If EXPR results in an error, insert the error message instead."
  (condition-case err
      (eval expr)
    (error (concat "<Error: " (error-message-string err) ">"))))

(defun insert-line (line) (insert (concat line "\n")))

(defun persp-projectile-status ()
  "Jump to singleton buffer with debug info."
  (interactive)
  (let ((buffer-name "Perspective / Projectile Status"))
    (with-current-buffer (get-buffer-create buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (emacs-lisp-mode)
      (flycheck-mode -1)
      ;; Expressions to be evaluated
      (let ((expressions '((projectile-project-root (persp-name (persp-curr)))
                           (projectile-project-root)
                           (projectile-project-name)
                           projectile-known-projects
                           (projectile-open-projects)
                           (persp-name (persp-curr))
                           (persp-names)
                           (centaur-tabs-buffer-groups)
                           (centaur-tabs-get-groups)
                           )))
        ;; Insert expressions and their results
        (dolist (expr expressions)
          (let ((result (try-eval expr)))
            (insert (format "%s :: %s\n    => %s\n\n" expr (type-of result)
                            (if (listp result)
                                (mapconcat #'prin1-to-string result "\n       ")
                              (prin1-to-string result)))))))
      ;; Display the buffer
      (switch-to-buffer buffer-name))))

;; Enable Vertico-Posframe
(use-package! vertico-posframe
  :after vertico
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-center
        vertico-posframe-width 120
        vertico-posframe-height 30
        vertico-posframe-border-width 1))

(use-package! paredit
  :hook (emacs-lisp-mode . enable-paredit-mode)
  :config
  (defun enable-paredit-mode ()
    "Enable paredit mode."
    (paredit-mode 1)))
