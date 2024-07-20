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

(setq frame-title-format
      '((:eval (if (projectile-project-p)
                   (projectile-project-name)
                 ""))))

(setq consult-fd-args
  (append
    (list (if (executable-find "fdfind" 'remote) "fdfind" "fd")
          "--color=never" "--full-path" "--absolute-path" "--hidden" "--no-ignore" "--exclude" ".git")))

(defun log (text)
  (interactive "sMessage: ")
  (let ((buffer-name "debug-info"))
    (with-current-buffer (get-buffer-create buffer-name)
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (insert (concat text "\n"))
      (setq buffer-read-only t)
      )
    )
  )

(defun try-eval (expr)
  "Evaluate EXPR and insert the result. If EXPR results in an error, insert the error message instead."
  (condition-case err
      (funcall expr)
    (error (concat "<Error: " (error-message-string err) ">"))))

(defun insert-line (line) (insert (concat line "\n")))

(defun persp-projectile-status ()
    "Jump to singleton buffer w/ debug info."
  (interactive)
  (let ((buffer-name "Perspective / Projectile Status"))
    (let ((buffer (get-buffer buffer-name)))
      (switch-to-buffer (get-buffer-create buffer-name))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert-line "Perspective / projectile status.")
      (insert-line "\n\n")
      (insert-line (concat "Persp name: " (persp-name (persp-curr))))
      (insert-line (concat "Projectile project from persp name: " (projectile-project-root (persp-name (persp-curr)))))
      (insert-line (concat "Projectile root w/o persp name: " (projectile-project-root)))
      (insert-line (concat "Projectile project name: " (projectile-project-name)))
      (insert-line (concat "Projectile open projects: " (prin1-to-string (projectile-open-projects))))
      (insert-line (concat "Perspectives: " (prin1-to-string (persp-names))))
      )))
