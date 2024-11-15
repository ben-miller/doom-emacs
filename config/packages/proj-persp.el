(setq persp-proj-map '(
                       (".doom.d" . "~/.doom.d/")
                       ("life" . "~/life/")
                       ("org" . "~/org/")
                       ("infra" . "~/src/infra/")
                       ("config" . "~/src/infra/config")
                       (".emacs.d" . "~/.emacs.d/")
                       ("imprecv" . "~/src/projects/imprecv/")
                       ("relational-links" . "~/src/projects/relational-links/")
                       ))

(after! projectile
  (setq projectile-known-projects '(
                                    "~/.doom.d"
                                    "~/life"
                                    "~/org"
                                    "~/src/infra"
                                    "~/src/infra/config"
                                    "~/src/projects/imprecv"
                                    "~/src/projects/relational-links"
                                    )
        projectile-completion-system 'default
        projectile-auto-discover nil
        projectile-cache-file (concat doom-cache-dir "projectile.cache")
        projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-require-project-root t
        projectile-switch-project-action (lambda ())
        projectile-track-known-projects-automatically nil)
  (add-hook 'persp-switch-hook
            (lambda ()
              (neotree-dir-from-persp)
            ))
  (add-hook 'projectile-after-switch-project-hook (lambda ())))

(use-package! perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(defun find-doom-file (path)
  (message (concat "finding config file for: " path))
  (persp-switch ".doom.d")
  (find-file path))

(defun find-infra-file (path)
  (message (concat "finding config file for: " path))
  (persp-switch "infra")
  (find-file path))

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

;; (setq frame-title-format
;;       '((:eval (if (projectile-project-p)
;;                    (concat
;;                     "Perspective: " (persp-name (persp-curr))
;;                     " <=> Project: " (projectile-project-name)
;;                  )))))

;; Disable frame title (otherwise shows as barely visible white color)
(setq frame-title-format nil)

(defun rename-initial-tab-to-main ()
  "Rename the initial tab to main and link it to the main perspective."
  (when (string= (alist-get 'name (tab-bar--current-tab)) "*scratch*")
    (tab-bar-rename-tab "main")
    (message "Renamed initial tab to main.")))

(add-hook 'tab-bar-mode-hook #'rename-initial-tab-to-main)

;; Enable 'tab-bar-mode'
(tab-bar-mode 1)

(defun persp-status ()
  "Print the current perspective name and a list of all perspectives."
  (interactive)
  (let ((current-persp (persp-name (persp-curr)))
        (all-persps (persp-names)))
    (message "Current Perspective: %s" current-persp)
    (message "All Perspectives: %s" (mapconcat 'identity all-persps ", "))))


;; Hook for when a perspective is created
(add-hook 'persp-created-hook
          (lambda (&rest _args)
            (let ((persp-name (persp-name (persp-curr))))
              (message "persp-created-hook triggered: %s" persp-name)
              ;; Create a new tab with the name of the perspective
              (tab-bar-new-tab)
              (tab-bar-rename-tab persp-name)
              ;; Optionally show perspective status
              (persp-status))))

;; Hook for before a perspective is killed
(add-hook 'persp-killed-hook
          (lambda (&rest _args)
            (let ((persp-name (persp-name persp)))
              (message "persp-killed-hook triggered: %s" persp-name))
            (persp-status)))

;; Hook for when a perspective is activated
(add-hook 'persp-activated-hook
          (lambda (&rest _args)
            (message "persp-activated-hook triggered")
            (let ((persp-name (persp-name (persp-curr))))
              (message "persp-activated-hook triggered: %s" persp-name))
            (persp-status)))

;; Hook for when a perspective is switched
(add-hook 'persp-switch-hook
          (lambda ()
            (message "persp-switch-hook triggered")
            (persp-status)))

;; Advice for when a tab is selected
(advice-add 'tab-bar-select-tab :after
            (lambda (&rest _args)
              (let ((tab-name (alist-get 'name (tab-bar--current-tab))))
                (persp-switch tab-name)
                (message "Switched to next tab: %s" tab-name))
              (persp-status)))

;; Advice for switching to the next tab
(advice-add 'tab-bar-switch-to-next-tab :after
            (lambda (&rest _args)
              (let ((tab-name (alist-get 'name (tab-bar--current-tab))))
                (persp-switch tab-name)
                (message "Switched to next tab: %s" tab-name))
              (persp-status)))

;; Advice for switching to the previous tab
(advice-add 'tab-bar-switch-to-prev-tab :after
            (lambda (&rest _args)
              (let ((tab-name (alist-get 'name (tab-bar--current-tab))))
                (persp-switch tab-name)
                (message "Switched to previous tab: %s" tab-name))
              (persp-status)))
