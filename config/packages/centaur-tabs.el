;; -*- lexical-binding: t; -*-

(defun centaur-tabs-buffer-groups ()
  "Group buffers by their Projectile project."
  (if (projectile-project-p)
      (list (projectile-project-name))
    (list "Misc")))

(defun try-match-pattern (pattern)
  (lambda (input-string)
    (if (string-match pattern input-string)
        (match-string 1 input-string)
      nil)))

(defun match-proj-scratch (input-string)
  (funcall (try-match-pattern "\\*scratch\\* (\\(.*\\))") input-string))

(defun match-proj-magit (input-string)
  (funcall (try-match-pattern "magit[^:]*: \\(.*\\)") input-string))

(defun my/centaur-tabs-buffer-groups ()
  (list
   (cond
    ;; Match project-specific scratch buffers
    ((when-let* ((buf (buffer-name))
                 (persp (match-proj-scratch buf))
                 (proj (cdr (assoc persp persp-proj-map))))
                 (concat "Project: " (expand-file-name proj))))
    ((when-let* ((buf (buffer-name))
                 (persp (match-proj-magit buf))
                 (proj (cdr (assoc persp persp-proj-map))))
                 (concat "Git: " (expand-file-name proj))))
    ((string-match-p "^\\*Org Agenda\\*" (buffer-name))
     "Org Agenda"
     )
    ((string-match-p "^\\*Org TODO\\*" (buffer-name))
     "Org TODO"
     )
    ((when-let ((project-dir (cdr (project-current))))
       (concat "Project: " project-dir)))
    ((when-let ((project-name (centaur-tabs-project-name)))
       project-name))
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '( magit-process-mode
                             magit-status-mode
                             magit-diff-mode
                             magit-log-mode
                             magit-file-mode
                             magit-blob-mode
                             magit-blame-mode)))
     "Emacs")
    ((derived-mode-p 'shell-mode) "Shell")
    ((derived-mode-p 'eshell-mode) "EShell")
    ((derived-mode-p 'emacs-lisp-mode) "Elisp")
    ((derived-mode-p 'dired-mode) "Dired")
    (t
     (centaur-tabs-get-group-name (current-buffer))))))

;; Apply the custom grouping function
(setq centaur-tabs-buffer-groups-function #'my/centaur-tabs-buffer-groups)

(setq centaur-tabs-cycle-scope 'tabs)

(centaur-tabs-mode)
