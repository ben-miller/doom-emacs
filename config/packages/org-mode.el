(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

(defun my/org-project-agenda-file ()
  "Get project's tasks.org file, if it exists"
  (expand-file-name "tasks.org" "~/life/org/"))

(defun my/org-agenda-files ()
  (interactive)
  (list (my/org-project-agenda-file)))

(defun my/org-capture-templates ()
  "Define org capture templates"
  `(
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
   org-agenda-files (my/org-agenda-files)
   org-todo-keywords '((sequence "INBOX" "IDEA" "SELECTED" "NEXT" "DOING" "POSTPONED" "BUG" "|" "DONE")))
  (map! :map org-mode-map
        "M-o" (lambda ()
                (interactive)
                (org-meta-return)
                (evil-insert 1)))
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5
                  org-level-6
                  org-level-7
                  org-level-8
                  org-document-title))
    (set-face-attribute face nil :weight 'Light))
  )

(after! org-agenda
  (map! :map org-agenda-mode-map "<escape>" #'org-agenda-Quit))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t)

;; Ellipsis styling
(setq org-ellipsis "…")
(set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

;; Enable org-modern
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-todo-faces '(
                                ("INBOX" :background "#1E90FF" :foreground "white")
                                ("DOING" :background "#FF8C00" :foreground "white")
                                ("NEXT" :background "#32CD32" :foreground "white")
                                ("BUG" :background "#EE4B2B" :foreground "white")
                                ("SELECTED" :background "#9B30FF" :foreground "white")
                                ))
  (global-org-modern-mode))
