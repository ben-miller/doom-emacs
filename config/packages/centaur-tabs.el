(defun centaur-tabs-buffer-groups ()
  "Group buffers by their Projectile project."
  (if (projectile-project-p)
      (list (projectile-project-name))
    (list "Misc")))

(message "loading centaur tabs...")

(defun my/centaur-tabs-buffer-groups ()
  (list
   (cond
    ;; Match project-specific scratch buffers
    ((when-let ((buffer-name (buffer-name))
                 (project-name (centaur-tabs-project-name))
                 (match (string-match "\\*scratch\\* (\\(.*\\))" buffer-name))
                 (scratch-project-name (match-string 1 buffer-name)))
       (message scratch-project-name)
       (when (string-equal project-name scratch-project-name)
         project-name)))
    ((when-let ((project-name (centaur-tabs-project-name)))
       project-name))
    ((or (memq major-mode '(org-mode org-agenda-mode diary-mode))
         (string-match-p "^\\*Org Agenda\\*" (buffer-name)))
     "OrgMode")
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
