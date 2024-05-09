;; -*- no-byte-compile: t; -*-

;; Load config files in settings subdir.
(load! "config/inbox.el")
(load! "config/org-config.el")

;;
;; Emacs / doom emacs settings.
;;

;; Theme.
(setq doom-theme 'doom-one-light)

;; Disable line numbers.
(setq display-line-numbers-type nil)

;; Disable the system clipboard.
(setq select-enable-clipboard nil)
(setq select-enable-primary nil)

;;
;; Settings for random packages.
;;

;;
;; Enable projectile caching.
;;
(setq projectile-enable-caching t)

;; Ivy
(after! ivy
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))

;; LeetCode
(setq leetcode-prefer-language "java")

;; Expand-region
(use-package! expand-region
  :bind ("M-k" . er/expand-region)
  :bind ("M-j" . er/contract-region)
  )

;; Magit
(after! magit
  (map! :map magit-mode-map
        "<escape>" #'magit-mode-bury-buffer))

;;
;; Key bindings.
;;

;; SPC keybindings.
(map! :leader
      ;; File/directory navigation.
      :desc "Find files in homedir" "f j" (lambda () (interactive) (counsel-find-file "~"))
      :desc "Find files in homedir" "f k" #'counsel-fzf
      ;; :desc "Neotree change root to homedir" "f K" (lambda () (interactive) (neotree-dir "~"))
      ;; :desc "Open currrent file in NeoTree pane" "f h" #'neotree-find
      ;; :desc "NeoTree toggle pane" "f n" #'neotree-toggle
      :desc "Dired" "SPC" #'dired

      ;; Magit.
      :desc "Magit commit -m 'Update'" "g k" #'magit-commit-update

      ;; Org mode (SPC d).
      :desc "Org refile" "d f" #'org-refile
      :desc "Show DOING items" "d d" (lambda () (interactive) (org-todo-list "DOING"))
      :desc "Show NEXT items" "d n" (lambda () (interactive) (org-todo-list "NEXT"))
      :desc "Capture note to inbox as INBOX" "d k" (lambda () (interactive) (org-capture nil "i"))
      :desc "Capture note to inbox as INBOX" "d I" (lambda () (interactive) (org-capture nil "i"))
      :desc "Capture note to inbox as DOING" "d D" (lambda () (interactive) (org-capture nil "d"))
      :desc "Capture note to inbox as NEXT" "d N" (lambda () (interactive) (org-capture nil "n"))
      :desc "Open inbox.org" "d i" (lambda () (interactive) (find-file "~/org/inbox.org"))
      :desc "Open emacs.org" "d e" (lambda () (interactive) (find-file "~/org/emacs.org"))

      ;; Java
      :desc "Gradle test" "j j" #'gradle-test
      :desc "Gradle build" "j k" #'gradle-build

      ;; Frequently edited files (SPC k).
      :desc "Edit config" "k k" (lambda () (interactive) (find-file "~/.doom.d/config.el"))
      :desc "Edit config" "k i" (lambda () (interactive) (find-file "~/.doom.d/config/inbox.el"))
      :desc "Edit config" "k p" (lambda () (interactive) (find-file "~/.doom.d/packages.el"))
      )

;; Non-SPC key bindings.
(map! :map global-map
      ;; Editor navigation.
      :desc "Move tab right" "s-S-<right>" #'tab-bar-move-tab
      :desc "Move tab left" "s-S-<left>" #'tab-bar-move-tab-backward
      :desc "Change to right tab" "s-<right>" #'tab-bar-switch-to-next-tab
      :desc "Change to left tab" "s-<left>" #'tab-bar-switch-to-prev-tab
      :desc "New tab (scratch)" "s-t" #'open-scratch-in-new-tab
      :desc "Close tab" "s-w" #'tab-bar-close-tab
      :desc "Focus pane left" "s-h" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-left))))
      :desc "Focus pane right" "s-l" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-right))))
      :desc "Focus pane up" "s-k" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-up))))
      :desc "Focus pane down" "s-j" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-down))))
      :desc "Split pane vertically" "s-d" #'split-and-balance-windows-vertically
      :desc "Split pane horizontally" "s-D" #'split-and-balance-windows-horizontally
      :desc "Close tab" "s-w" #'close-window-or-tab
      :desc "Previous buffer" "s-[" #'previous-buffer
      :desc "Next buffer" "s-]" #'next-buffer
      :desc "Toggle pane maximization" "s-K" #'toggle-maximize-window

      ;; Swiper.
      :desc "Swiper" "C-/" #'swiper

      ;; Projectile
      :desc "Find file" "s-o" #'+ivy/projectile-find-file

      ;; System clipboard.
      :desc "Paste from system clipboard" "s-v" #'paste-from-system-clipboard
      :desc "Copy to system clipboard" "s-c" #'copy-region-to-system-clipboard

      ;; Old habits die hard.
      :desc "Edit config" "s-," (lambda () (interactive) (find-file "~/.doom.d/config.el"))
      )
