(after! evil
  (map! :n "s-/" nil)

  (setq evil-move-beyond-eol t)
  (setq evil-cross-lines t)
  (setq evil-ex-search-case 'smart)

  (map! :n "C-e" 'evil-end-of-line)
  (map! :v "C-e" 'evil-end-of-line)
  (map! :n "C-a" 'evil-beginning-of-line)
  (map! :v "C-a" 'evil-beginning-of-line)

  (define-key evil-insert-state-map (kbd "s-<right>") 'centaur-tabs-forward)
  (define-key evil-insert-state-map (kbd "s-<left>") 'centaur-tabs-backward)

  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  )
