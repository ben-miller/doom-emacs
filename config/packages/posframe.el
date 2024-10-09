(use-package! vertico-posframe
  :after vertico
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-center
        vertico-posframe-width 120
        vertico-posframe-height 30
        vertico-count 28
        vertico-posframe-border-width 1))

(after! vertico
  (define-key vertico-map (kbd "C-M-j") #'vertico-next)
  (define-key vertico-map (kbd "C-M-k") #'vertico-previous))
