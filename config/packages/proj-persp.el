(setq persp-proj-map '(
                       (".doom.d" . "~/.doom.d/")
                       ("org" . "~/org/")
                       ("life" . "~/life/")
                       ("infra" . "~/src/infra/")
                       (".emacs.d" . "~/.emacs.d/")
                       ("comptus-takehome" . "~/src/projects/comptus-takehome/"))
      )

(after! projectile
  (setq projectile-known-projects '(
                                    "~/.doom.d"
                                    "~/org"
                                    "~/life"
                                    "~/src/infra"
                                    "~/.emacs.d"
                                    "~/src/projects/comptus-takehome"
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
