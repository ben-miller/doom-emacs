(add-hook 'doom-after-init-hook
          (lambda () (doom/quickload-session t)))

;; Disable save-place mode
(setq save-place-mode nil)

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
            (org-reload)
            (org-modern-mode)
            (neotree)
            ))

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

(after! consult
  (setq consult-fd-args
        (append
         (list (if (executable-find "fdfind" 'remote) "fdfind" "fd")
               "--color=never" "--full-path" "--absolute-path" "--hidden" "--no-ignore" "--exclude" ".git"))))

(defun try-eval (expr)
  "Evaluate EXPR and insert the result. If EXPR results in an error, insert the error message instead."
  (condition-case err
      (eval expr)
    (error (concat "<Error: " (error-message-string err) ">"))))

(defun insert-line (line) (insert (concat line "\n")))

(use-package! smartparens
  :config
  (smartparens-global-mode t)
  (map! :map smartparens-mode-map
        "M-s-0" #'sp-wrap-round
        "M-s-9" #'sp-unwrap-sexp
        "s-," #'sp-forward-slurp-sexp
        "s-." #'sp-forward-barf-sexp
        "M-s-." #'sp-sexp-backward-slurp
        "M-s-," #'sp-backward-barf-sexp
        "M-<up>" #'sp-backward-sexp
        "M-<down>" #'sp-forward-sexp
        "M-<right>" #'sp-forward-sexp
        "M-<left>" #'sp-backward-sexp
        "M-S-<up>" #'sp-up-sexp
        "M-S-<down>" #'sp-down-sexp
        "M-S-<left>" #'sp-previous-sexp
        "M-S-<right>" #'sp-next-sexp
        "M-s-x" #'sp-delete-sexp))

(defun scratch-open-in-proj () (interactive)
  (let* ((project-name (persp-name (persp-curr)))
         (buffer-name (concat "*scratch* (" project-name ")"))
         (existing-buffer (get-buffer buffer-name)))
    (if existing-buffer
        (switch-to-buffer existing-buffer)
      (switch-to-buffer (generate-new-buffer buffer-name)))))

(setq auto-save-default nil)

(defun disable-flycheck-kotlin ()
  (when (derived-mode-p 'kotlin-mode)
    (flycheck-mode -1)))

(add-hook 'kotlin-mode-hook 'disable-flycheck-kotlin)

;; Make cursor flash indefinitely
(setq blink-cursor-blinks 0)
