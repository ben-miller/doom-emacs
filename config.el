(map! :leader
      ;; Inbox
      :desc "Babel execute source block" "c RET" #'org-babel-execute-src-block

      ;; File/directory navigation.
      :desc "Find files in homedir" "f j" (lambda () (interactive) (affe-find "~"))

      ;; Magit.
      :desc "Magit commit -m 'Update'" "g k" #'magit-commit-update

      ;; Org mode (SPC d).
      :desc "Org refile" "d f" #'org-refile
      :desc "Show DOING items" "d d" (lambda () (interactive) (org-todo-list "DOING"))
      :desc "Show NEXT items" "d n" (lambda () (interactive) (org-todo-list "NEXT"))
      :desc "Show SELECTED items" "d s" (lambda () (interactive) (org-todo-list "SELECTED"))

      :desc "Capture to inbox as DOING" "d D" (lambda () (interactive) (org-capture nil "d"))
      :desc "Capture to inbox as INBOX" "d k" (lambda () (interactive) (org-capture nil "i"))
      :desc "Capture to inbox as NEXT" "d N" (lambda () (interactive) (org-capture nil "n"))
      :desc "Capture to global inbox" "n k" (lambda () (interactive) (org-capture nil "g"))

      :desc "Open current tasks.org" "d K" (lambda () (interactive) (find-file (my/org-project-agenda-file)))
      :desc "Open global tasks.org" "n K" (lambda () (interactive) (find-file "~/org/tasks.org"))

      ;; Projectile-perspective.
      :desc "Switch project" "p p" #'projectile-persp-switch-project

      ;; Java
      :desc "Gradle test" "j j" #'gradle-test
      :desc "Gradle build" "j k" #'gradle-build

      ;; Shell interaction.
      :desc "Kill process" "m k" #'kill-process

      ;; Frequently edited files (SPC k).
      :desc "Edit config.el (README.org)" "k k" (lambda () (interactive) (find-file "~/.doom.d/README.org"))
      :desc "Edit packages.el" "k p" (lambda () (interactive) (find-file "~/.doom.d/packages.el"))
      :desc "Edit init.el" "k i" (lambda () (interactive) (find-file "~/.doom.d/init.el"))
      :desc "Edit hammerspoon config" "k h" (lambda () (interactive) (find-file "~/src/infra/hs-profiles/init.lua"))
      :desc "Edit wezterm config" "k w" (lambda () (interactive) (find-file "~/src/infra/config/wezterm/.wezterm.lua"))
      :desc "Edit tmux config" "k t" (lambda () (interactive) (find-file "~/src/infra/config/tmux/.tmux.conf"))
      :desc "Edit fish config" "k f" (lambda () (interactive) (find-file "~/src/infra/config/fish/.config/fish/config.fish"))
      :desc "Edit nvim config" "k v" (lambda () (interactive) (find-file "~/.config/nvim/lua/options.lua"))
      :desc "Edit nvim config" "k V" (lambda () (interactive) (find-file "~/.config/nvim/lua/plugins.lua"))
      )

(map! :map global-map
      ;; Editor navigation.
      :desc "Move tab right" "s-S-<right>" #'centaur-tabs-move-current-tab-to-right
      :desc "Move tab left" "s-S-<left>" #'centaur-tabs-move-current-tab-to-left
      :desc "Change to tab right" "s-<right>" #'centaur-tabs-forward
      :desc "Change to tab left" "s-<left>" #'centaur-tabs-backward
      :desc "Change to tab group right" "M-s-<right>" #'centaur-tabs-forward-group
      :desc "Change to tab group left" "M-s-<left>" #'centaur-tabs-backward-group
      :desc "New tab (scratch)" "s-t" #'centaur-tabs--create-new-tab
      :desc "Close tab" "s-w" #'kill-current-buffer
      :desc "Focus pane left" "s-h" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-left))))
      :desc "Focus pane right" "s-l" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-right))))
      :desc "Focus pane up" "s-k" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-up))))
      :desc "Focus pane down" "s-j" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-down))))
      :desc "Split pane vertically" "s-d" #'split-and-balance-windows-vertically
      :desc "Split pane horizontally" "s-D" #'split-and-balance-windows-horizontally
      :desc "Previous buffer" "s-[" #'previous-buffer
      :desc "Next buffer" "s-]" #'next-buffer
      :desc "Toggle pane maximization" "s-K" #'toggle-maximize-window
      :desc "Jump to definition" "s-b" #'+lookup/definition

      ;; Swiper.
      :desc "Swiper" "C-/" #'swiper

      ;; Files.
      :desc "Find file" "s-o" #'affe-find
      :desc "Grep in project" "s-i" #'affe-grep
      :desc "Create file" "s-n" #'projectile-create-new-file

      ;; Neotree.
      :desc "Toggle neotree" "s-g" #'neotree-project-dir

      ;; System clipboard.
      :desc "Paste from system clipboard" "s-v" #'paste-from-system-clipboard
      :desc "Copy to system clipboard" "s-c" #'copy-region-to-system-clipboard

      ;; Next/previous error.
      :desc "Next error" "M-]" #'next-error
      :desc "Previous error" "M-[" #'previous-error

      ;; Drag stuff.
      :desc "Drag stuff up" "M-k" #'drag-stuff-up
      :desc "Drag stuff down" "M-j" #'drag-stuff-down

      ;; Expand-region
      :desc "Expand region" "M-s-j" #'er/expand-region
      :desc "Contract region" "M-s-k" #'er/contract-region

      ;; Comments.
      :desc "Comment LOC" "s-/" (lambda () (interactive)
                                  (evilnc-comment-or-uncomment-lines 1)
                                  (forward-line 1))
      )

(after! evil
  (map! :n "s-/" nil)
)

(load (expand-file-name "config/packages/org-mode.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/proj-persp.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/neotree.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/centaur-tabs.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/magit.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/org-trello.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/leetcode.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/packages/evil.el" (file-name-directory load-file-name)))

;; -*- no-byte-compile: t; -*-

(add-hook 'doom-after-init-hook
          (lambda () (doom/quickload-session t)))

(defadvice! reload-with-tangle ()
  "Tangle README.org before reloading Doom Emacs."
  :before #'doom/reload
  (org-babel-tangle-file (expand-file-name "README.org" doom-private-dir)))

;; Don't prompt when exiting.
(setq confirm-kill-emacs nil)

;; Visual line mode
(global-visual-line-mode)

;; Blink cursor mode.
(blink-cursor-mode 1)

;; Disable highlight line mode.
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; Disable flyspell.
(remove-hook 'text-mode-hook #'flyspell-mode)
(remove-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Suppress warnings.
(setq warning-suppress-types
      '((obsolete)
        (bytecomp)
        (bytecomp . buffer-local-value)))
(setq enable-local-variables :all)

(after! emacs
  (find-file "~/.doom.d/README.org")
  (neotree))

(setq frame-title-format
      '((:eval (if (projectile-project-p)
                   (projectile-project-name)
                 ""))))

(load (expand-file-name "config/settings/editor.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/settings/clipboard.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/settings/appearance.el" (file-name-directory load-file-name)))

(load (expand-file-name "config/utils.el" (file-name-directory load-file-name)))
