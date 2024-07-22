(setq evil-ex-search-case 'smart)

(after! evil
  (map! :n "s-/" nil)
)

(define-key evil-insert-state-map (kbd "s-<right>") 'centaur-tabs-forward)
(define-key evil-insert-state-map (kbd "s-<left>") 'centaur-tabs-backward)
