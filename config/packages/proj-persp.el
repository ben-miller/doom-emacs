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
              (setq projectile-project-name (persp-name (persp-curr)))
              (tab-bar-select-tab-by-name (persp-name (persp-curr)))
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

(setq frame-persp-debug
      '((:eval (if (projectile-project-p)
                   (concat
                    "Current perspective:        " (persp-name (persp-curr))
                    "\nProjectile project name:    " (projectile-project-name)
                    "\nCentaur tabs buffers:       " (prin1-to-string centaur-tabs--buffers)
                    "\nCentaur tabs groups:        " (prin1-to-string (centaur-tabs-get-groups))
                    "\nCentaur tabs tabset:        " (prin1-to-string centaur-tabs-current-tabset)
                 )))))

(defun print-frame-persp-debug ()
  "Evaluate and print the contents of `frame-persp-debug` as a message."
  (interactive)
  (let ((debug-output (mapconcat
                       (lambda (segment)
                         (if (and (listp segment) (eq (car segment) :eval))
                             (eval (cadr segment)) ;; Evaluate `(:eval ...)` segments
                           (format "%s" segment))) ;; Convert other parts to strings
                       frame-persp-debug
                       ""))) ;; Concatenate results
    (message "%s\n\n\n" debug-output)))

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
  (print-frame-persp-debug))

;; Hook for when a perspective is created
(add-hook 'persp-created-hook
          (lambda (&rest _args)
            (let ((persp-name (persp-name (persp-curr))))
              (message "persp-created-hook triggered: %s" persp-name)
              ;; Create a new tab with the name of the perspective
              (tab-bar-new-tab)
              (tab-bar-rename-tab persp-name))))

;; Hook for when a perspective is activated
(add-hook 'persp-activated-hook
          (lambda (&rest _args)
            (let ((persp-name (persp-name (persp-curr))))
              (message "persp-activated-hook triggered: %s" persp-name))))

;; Advice for when a tab is selected
(advice-add 'tab-bar-select-tab :after
            (lambda (&rest _args)
              (let ((tab-name (alist-get 'name (tab-bar--current-tab))))
                (message "Tab bar select tab: %s" tab-name)
                (persp-switch tab-name))
              (persp-status)))
