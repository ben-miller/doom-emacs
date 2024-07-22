(after! projectile
  (setq projectile-known-projects '(
                                    "~/.doom.d"
                                    "~/org"
                                    "~/life"
                                    "~/src/infra"
                                    "~/src/projects/comptus-takehome"
                                    )
        persp-proj-map '(
                               (".doom.d" . "~/.doom.d")
                               ("org" . "~/org")
                               ("life" . "~/life")
                               ("infra" . "~/src/infra")
                               ("comptus-takehome" . "~/src/projects/comptus-takehome")
                               )
        projectile-completion-system 'default
        projectile-auto-discover nil
        projectile-cache-file (concat doom-cache-dir "projectile.cache")
        projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-require-project-root t
        projectile-switch-project-action (lambda ())
        projectile-track-known-projects-automatically nil)
  (add-hook 'persp-switch-hook (lambda ()
                                 (message (concat "persp name: " (prin1-to-string (persp-name (persp-curr)))))
                                 (neotree-dir (cdr (assoc (persp-name (persp-curr)) persp-proj-map)))
                                 (setq org-capture-templates (my/org-capture-templates)
                                       org-agenda-files (my/org-agenda-files))))
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
