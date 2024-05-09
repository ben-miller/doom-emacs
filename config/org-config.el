(setq org-directory "~/org/")

(after! org
  (setq org-todo-keyword-faces
        '(("INBOX" . "#1E90FF")
          ("DOING" . "#FF8C00")
          ("NEXT" . "#32CD32")
          ("BUG" . "#EE4B2B")
          ("IDEA" . "#9B30FF")
          )))

(after! org
  (add-to-list 'org-capture-templates
               '("i" "Inbox item" entry
                 (file+headline "~/org/inbox.org" "Inbox")
                 "** INBOX %?\n"))
  (add-to-list 'org-capture-templates
               '("d" "Inbox item" entry
                 (file+headline "~/org/inbox.org" "Inbox")
                 "** DOING %?\n"))
  (add-to-list 'org-capture-templates
               '("n" "Inbox item" entry
                 (file+headline "~/org/inbox.org" "Inbox")
                 "** NEXT %?\n"))
  )

(after! org-agenda
  (map! :map org-agenda-mode-map
        "<escape>" #'org-agenda-exit))
