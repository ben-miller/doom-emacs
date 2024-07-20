(after! projectile
  (setq projectile-known-projects '(
                                    "~/.doom.d/"
                                    "~/org"
                                    "~/life"
                                    "~/src/infra/"
                                    "~/src/projects/comptus-takehome"
                                    )
        projectile-completion-system 'default
        projectile-auto-discover nil
        projectile-cache-file (concat doom-cache-dir "projectile.cache")
        projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-require-project-root t
        projectile-switch-project-action (lambda () (find-file (expand-file-name "tasks.org" (projectile-project-root))))
        projectile-track-known-projects-automatically nil)
  (projectile-discover-projects-in-search-path)
  (add-hook 'projectile-after-switch-project-hook (lambda ()
                                                    (setq org-capture-templates (my/org-capture-templates)
                                                          org-agenda-files (my/org-agenda-files))
                                                    (message "Project org file: %s" (my/org-project-agenda-file)))))

(use-package! perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))
