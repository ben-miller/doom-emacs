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

;; Disable company spell-checking backends.
(after! company
  (setq company-backends (delete 'company-ispell company-backends))
  (setq company-backends (delete 'company-dabbrev company-backends)))

;; Suppress warnings.
(setq warning-suppress-types
      '((obsolete)
        (bytecomp)
        (bytecomp . buffer-local-value)))
(setq enable-local-variables :all)

(add-hook 'emacs-startup-hook
          (lambda ()
            (org-reload)
            (org-modern-mode)
            (neotree)
            ))

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

;; Dired settings
(after! dired
  (setq dired-listing-switches "-a1")
  (setq dired-omit-files "^\\...+$")
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package! ultra-scroll-mac
  :if (eq window-system 'mac)
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mac-mode 1))

(use-package! popwin
  :config
  (popwin-mode 1)
  (push '("*Messages*" :height 0.6 :position bottom :dedicated t) popwin:special-display-config)
  (push '("*compilation*<.emacs.d>" :height 0.4 :position bottom :dedicated t) popwin:special-display-config)
  (push '("*Warnings*" :height 0.4 :position bottom :dedicated t) popwin:special-display-config)
  (push '("*Help*" :height 0.5 :position right) popwin:special-display-config))

(defun project-root-from-persp ()
  "Get the project root directory based on the current perspective name using `persp-proj-map`."
  (interactive)
  (let* ((persp-name (persp-name (persp-curr))) ;; Get current perspective name
         (project-root (cdr (assoc persp-name persp-proj-map)))) ;; Look up in map
    (if project-root
        (expand-file-name project-root)
      (message "No project root found for perspective: %s" persp-name)
      nil)))

(defun magit-status-from-persp ()
  "Open Magit in the project root directory derived from the current perspective."
  (interactive)
  (let ((project-root (project-root-from-persp)))
    (if project-root
        (magit-status project-root)
      (message "Could not determine project root from perspective."))))

(use-package! claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu)  ; Or choose your own keybinding
  :config
  (claude-code-ide-emacs-tools-setup))  ; Enables MCP tools for Emacs integration
