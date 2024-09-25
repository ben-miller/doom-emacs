;; Make both Option keys Meta
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)

(map! :leader
      ;; Inbox
      :desc "Babel execute source block"
      "c RET" #'org-babel-execute-src-block

      ;; File/directory navigation.
      :desc "Find files in homedir"
      "f j" (lambda () (interactive) (affe-find "~"))

      ;; Magit.
      :desc "Magit commit -m 'Update'"
      "g k" #'magit-commit-update

      ;; Org mode (SPC d).
      :desc "Org refile"
      "d f" #'org-refile
      ;; :desc "Show DOING items"
      ;; "d d" (lambda () (interactive) (org-todo-list "DOING"))
      :desc "Show DOING items"
      "d d" (lambda () (interactive) (find-file "~/org/projects/dev.org"))
      :desc "Show NEXT items"
      "d n" (lambda () (interactive) (org-todo-list "NEXT"))
      :desc "Show SELECTED items"
      "d s" (lambda () (interactive) (org-todo-list "SELECTED"))

      :desc "Capture to inbox as DOING"
      "d D" (lambda () (interactive) (org-capture nil "d"))
      :desc "Capture to inbox as INBOX"
      "d k" (lambda () (interactive) (org-capture nil "i"))
      :desc "Capture to inbox as INBOX"
      "d I" (lambda () (interactive) (org-capture nil "i"))
      :desc "Capture to inbox as NEXT"
      "d N" (lambda () (interactive) (org-capture nil "n"))
      :desc "Capture to global inbox"
      "n k" (lambda () (interactive) (org-capture nil "g"))

      :desc "Open tasks.org"
      "d K" (lambda () (interactive) (find-file (my/org-project-agenda-file)))
      :desc "Open global tasks.org"
      "n K" (lambda () (interactive) (find-file "~/org/tasks.org"))

      ;; Projectile-perspective.
      :desc "Switch project"
      "p p" #'projectile-persp-switch-project

      ;; Shell interaction.
      :desc "Kill process"
      "m k" #'kill-process

      ;; Frequently edited files (SPC k).
      :desc "Edit settings.el"
      "k s" (lambda () (interactive) (find-doom-file "~/.doom.d/config/settings/settings.el"))
      :desc "Edit leader keybindings.el"
      "k k" (lambda () (interactive) (find-doom-file "~/.doom.d/config/settings/keybindings-leader.el"))
      :desc "Edit hold-down keybindings.el"
      "k K" (lambda () (interactive) (find-doom-file "~/.doom.d/config/settings/keybindings-hold-down.el"))
      :desc "Edit appearance.el"
      "k a" (lambda () (interactive) (find-doom-file "~/.doom.d/config/settings/appearance.el"))
      :desc "Edit appearance.el"
      "k o" (lambda () (interactive) (find-doom-file "~/.doom.d/config/packages/org-mode.el"))
      :desc "Edit proj-persp.el"
      "k j" (lambda () (interactive) (find-doom-file "~/.doom.d/config/packages/proj-persp.el"))
      :desc "Edit JetBrains keymaps"
      "k J" (lambda () (interactive) (find-doom-file "~/src/infra/config/jetbrains/macOS copy.xml"))
      :desc "Edit packages.el"
      "k p" (lambda () (interactive) (find-doom-file "~/.doom.d/packages.el"))
      :desc "Edit init.el"
      "k i" (lambda () (interactive) (find-doom-file "~/.doom.d/init.el"))
      :desc "Edit hammerspoon config"
      "k h" (lambda () (interactive) (find-infra-file "~/src/infra/hs-profiles/init.lua"))
      :desc "Edit wezterm config"
      "k w" (lambda () (interactive) (find-infra-file "~/src/infra/config/wezterm/.wezterm.lua"))
      :desc "Edit tmux config"
      "k t" (lambda () (interactive) (find-infra-file "~/src/infra/config/tmux/.tmux.conf"))
      :desc "Edit fish config"
      "k f" (lambda () (interactive) (find-infra-file "~/src/infra/config/fish/.config/fish/config.fish"))
      :desc "Edit nvim options"
      "k v" (lambda () (interactive) (find-infra-file "~/.config/nvim/lua/options.lua"))
      :desc "Edit evil config"
      "k e" (lambda () (interactive) (find-infra-file "~/.doom.d/config/packages/evil.el"))
      :desc "Edit ideavim config"
      "k V" (lambda () (interactive) (find-infra-file "~/.ideavimrc"))
      :desc "View debug info buffer"
      "k d" #'persp-projectile-status

      ;; Uncategorized.
      :desc "Neotree find this file"
      "f h" #'+neotree/find-this-file
      :desc "Open ielm"
      "c m" #'ielm
      :desc "Reload ielm"
      "c M" #'ielm-reload
      :desc "Open scratch in current project"
      "c s" #'scratch-open-in-proj
)
