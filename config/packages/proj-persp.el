(setq persp-proj-map '(
                       (".doom.d" . "~/.doom.d/")
                       ("life" . "~/life/")
                       ("infra" . "~/src/infra/")
                       ("config" . "~/src/infra/config")
                       (".emacs.d" . "~/.emacs.d/")
                       ("imprecv" . "~/src/projects/imprecv/")
                       ("life-api" . "~/src/projects/life-api/")
                       ("obsidian-components" . "~/src/projects/obsidian-components/")
                       ("life" . "~/life/")
                       ("py-utils" . "~/src/projects/py-utils/"))
      )

(after! projectile
  (setq projectile-known-projects '(
                                    "~/.doom.d"
                                    "~/life"
                                    "~/src/infra"
                                    "~/src/infra/config"
                                    "~/src/projects/imprecv"
                                    "~/src/projects/life-api"
                                    "~/src/projects/obsidian-components"
                                    "~/life"
                                    "~/src/projects/py-utils"
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
