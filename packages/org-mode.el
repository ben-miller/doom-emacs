(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   ;; Add other languages here if needed
   ))

(defun my/org-project-agenda-file ()
  "Get project's tasks.org file, if it exists."
  (expand-file-name "tasks.org" (or (projectile-project-root) "~/org/")))

(defun my/org-agenda-files ()
  (interactive)
  (list "~/org/tasks.org" (my/org-project-agenda-file)))

(defun my/org-capture-templates ()
  "Define org capture templates. Global capture, as well templates specific to current project."
  `(("g" "Global INBOX item" entry
     (file+headline "~/org/tasks.org" "Inbox")
     "** INBOX %?\n")

    ("i" "INBOX item" entry
     (file+headline ,(my/org-project-agenda-file) "Inbox")
     "** INBOX %?\n")

    ("d" "DOING item" entry
     (file+headline ,(my/org-project-agenda-file) "Inbox")
     "** DOING %?\n")

    ("n" "NEXT item" entry
     (file+headline ,(my/org-project-agenda-file) "Inbox")
     "** NEXT %?\n")

    ("s" "SELECTED item" entry
     (file+headline ,(my/org-project-agenda-file) "Inbox")
     "** SELECTED %?\n")
    ))

(after! org
  (setq
        org-capture-templates (my/org-capture-templates)
        org-agenda-files (my/org-agenda-files))
  (map! :map org-mode-map
        "M-o" (lambda ()
                (interactive)
                (org-meta-return)
                (evil-insert 1))))

(after! org-agenda
  (map! :map org-agenda-mode-map "<escape>" #'org-agenda-exit))
