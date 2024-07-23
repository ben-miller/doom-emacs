(map! :map global-map
      ;; Editor navigation.
      :desc "Move tab right"
      "s-S-<right>" #'centaur-tabs-move-current-tab-to-right
      :desc "Move tab left"
      "s-S-<left>" #'centaur-tabs-move-current-tab-to-left
      :desc "Change to tab right"
      "s-<right>" #'centaur-tabs-forward
      :desc "Change to tab left"
      "s-<left>" #'centaur-tabs-backward
      :desc "Change to tab group right"
      "M-s-<right>" #'centaur-tabs-forward-group
      :desc "Change to tab group left"
      "M-s-<left>" #'centaur-tabs-backward-group
      :desc "New tab (scratch)"
      "s-t" #'centaur-tabs--create-new-empty-buffer
      :desc "Close tab"
      "s-w" #'kill-current-buffer
      :desc "Focus pane left"
      "s-h" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-left))))
      :desc "Focus pane right"
      "s-l" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-right))))
      :desc "Focus pane up"
      "s-k" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-up))))
      :desc "Focus pane down"
      "s-j" (lambda () (interactive) (move-and-maybe-maximize (lambda () (windmove-down))))
      :desc "Move window to the left"
      "s-H" #'+evil/window-move-left
      :desc "Move window to the right"
      "s-L" #'+evil/window-move-right
      :desc "Move window to the right"
      "M-s-[" #'persp-prev
      :desc "Move window to the right"
      "M-s-]" #'persp-next
      :desc "Split pane vertically"
      "s-d" #'split-and-balance-windows-vertically
      :desc "Split pane horizontally"
      "s-D" #'split-and-balance-windows-horizontally
      :desc "Previous buffer"
      "s-[" #'previous-buffer
      :desc "Next buffer"
      "s-]" #'next-buffer
      :desc "Toggle pane maximization"
      "s-K" #'toggle-maximize-window
      :desc "Jump to definition"
      "s-b" #'+lookup/definition

      ;; Swiper.
      :desc "Swiper"
      "C-/" #'swiper

      ;; Files.
      :desc "Find file"
      "s-o" #'consult-fd
      :desc "Grep in project"
      "s-i" #'consult-ripgrep
      :desc "Create file"
      "s-n" #'projectile-create-new-file

      ;; Neotree.
      :desc "Toggle neotree"
      "s-g" #'neotree-project-dir

      ;; System clipboard.
      :desc "Paste from system clipboard"
      "s-v" #'paste-from-system-clipboard
      :desc "Copy to system clipboard"
      "s-c" #'copy-region-to-system-clipboard

      ;; Next/previous error.
      :desc "Next error"
      "M-]" #'next-error
      :desc "Previous error"
      "M-[" #'previous-error

      ;; Drag stuff.
      :desc "Drag stuff up"
      "M-k" #'drag-stuff-up
      :desc "Drag stuff down"
      "M-j" #'drag-stuff-down
      :desc "Drag stuff left"
      "M-h" #'drag-stuff-left
      :desc "Drag stuff right"
      "M-l" #'drag-stuff-right

      ;; Expand-region.
      :desc "Expand region"
      "M-s-k" #'er/expand-region
      :desc "Contract region"
      "M-s-j" #'er/contract-region

      ;; Comments.
      :desc "Comment LOC"
      "s-/" (lambda () (interactive)
                                  (evilnc-comment-or-uncomment-lines 1)
                                  (forward-line 1))
      )
