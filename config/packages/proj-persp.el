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

(defun on-perspective-switched ()
  "Function to run when the perspective is switched."
  (message "Perspective changed to %s" (persp-name (persp-curr))
  (neotree-toggle)
  (neotree-dir-from-persp)))

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
              (on-perspective-switched)
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

(perspective-tabs-mode +1)

(advice-add 'perspective-tabs-select-tab :after (lambda (tab-index)
                                                  (on-perspective-switched)))
