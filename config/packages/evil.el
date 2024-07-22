
(after! evil
  (map! :n "s-/" nil)
  (setq evil-move-beyond-eol t)
  (setq evil-ex-search-case 'smart))

(define-key evil-insert-state-map (kbd "s-<right>") 'centaur-tabs-forward)
(define-key evil-insert-state-map (kbd "s-<left>") 'centaur-tabs-backward)
