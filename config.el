;; -*- no-byte-compile: t; -*-

(add-hook 'doom-after-init-hook
          (lambda () (doom/quickload-session t)))

(defadvice! reload-with-tangle ()
  "Tangle README.org before reloading Doom Emacs."
  :before #'doom/reload
  (tangle-readme-org))

(setq warning-suppress-types '((obsolete)))

(map! :leader
      ;; File/directory navigation.
      :desc "Find files in homedir" "f j" (lambda () (interactive) (counsel-find-file "~"))
      :desc "Find files in homedir" "f k" #'counsel-fzf
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
      :desc "Open tasks.org" "d i" (lambda () (interactive) (find-file "~/org/tasks.org"))

      ;; Call this with default=nil so that 'projectile-switch-project-hook is used.
      ;; :desc "Switch project" "p p" (lambda () (interactive) (counsel-projectile-switch-project nil))

      ;; Java
      :desc "Gradle test" "j j" #'gradle-test
      :desc "Gradle build" "j k" #'gradle-build

      ;; Shell interaction.
      :desc "Kill process" "m k" #'kill-process

      ;; Frequently edited files (SPC k).
      :desc "Edit config" "k k" (lambda () (interactive) (edit-config-file "~/.doom.d/README.org"))
      :desc "Edit config" "k p" (lambda () (interactive) (edit-config-file "~/.doom.d/packages.el"))
      :desc "Edit config" "k i" (lambda () (interactive) (edit-config-file "~/.doom.d/init.el"))
      )

(map! :map global-map
      ;; Editor navigation.
      :desc "Move tab right" "s-S-<right>" #'centaur-tabs-move-current-tab-to-right
      :desc "Move tab left" "s-S-<left>" #'centaur-tabs-move-current-tab-to-left
      :desc "Change to tab right" "s-<right>" #'centaur-tabs-forward
      :desc "Change to tab left" "s-<left>" #'centaur-tabs-backward
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

      ;; Projectile
      :desc "Find file" "s-o" #'+ivy/projectile-find-file
      :desc "Create file" "s-n" #'projectile-create-new-file

      ;; Counsel-rg (ripgrep)
      :desc "Ripgrep in project" "s-i" #'counsel-rg

      ;; Neotree.
      :desc "Toggle neotree" "s-g" #'neotree-project-dir

      ;; System clipboard.
      :desc "Paste from system clipboard" "s-v" #'paste-from-system-clipboard
      :desc "Copy to system clipboard" "s-c" #'copy-region-to-system-clipboard

      ;; Old habits die hard.
      :desc "Edit config" "s-," (lambda () (interactive) (edit-config-file "~/.doom.d/README.org"))
      )

;; Font.
(setq doom-font (font-spec :family "Monaco" :size 20)
      doom-variable-pitch-font (font-spec :family "Monaco" :size 20)
      doom-big-font (font-spec :family "Monaco" :size 26))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;; Theme.
(setq doom-theme 'doom-one-light)

;; Disable line numbers.
(setq display-line-numbers-type nil)

;; Minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(load-theme 'modus-operandi t)

;; Choose some fonts
;; (set-face-attribute 'default nil :family "Iosevka")
;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(defun open-scratch-in-new-tab ()
  "Open a new tab with a *scratch* buffer."
  (interactive)
  (tab-new)
  (switch-to-buffer "*scratch*"))

(defun split-and-balance-windows-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (select-window (next-window)))

(defun split-and-balance-windows-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (select-window (next-window)))

(defun close-window-or-tab ()
  (interactive)
  (if (one-window-p)
      (tab-close)
    (progn
      (delete-window)
      (balance-windows))
    ))

(defun my-list-windows ()
  "List all windows in the current tab along with their widths."
  (interactive)
  (let ((window-info '()))
    (walk-windows
     (lambda (w)
       (push (format "%s (width: %d)" (buffer-name (window-buffer w)) (window-width w)) window-info))
     nil t)
    (message "Windows in current tab: %s" (mapconcat 'identity window-info ", "))))

(defun window-is-maximized ()
  "Check if any window in the current tab has a width under 16 characters."
  (cl-some (lambda (w) (< (window-width w) 16))
           (window-list)))

(defun toggle-maximize-window ()
  "Toggle the maximization state of the current window."
  (interactive)
  (if (window-is-maximized)
      (balance-windows)    ; If the window is maximized, balance the windows.
      (maximize-window)))  ; If the window is not maximized, maximize it.

(defun move-and-maybe-maximize (move-fn)
  "Move using the lambda function MOVE-FN and maximize if the window is already maximized."
  (funcall move-fn)
  (when (window-is-maximized)
    (maximize-window)))

;; Disable the system clipboard.
(setq select-enable-clipboard nil)
(setq select-enable-primary nil)

;; Function to paste directly from the system clipboard
(defun paste-from-system-clipboard ()
  "Paste text from the system clipboard."
  (interactive)
  (insert (shell-command-to-string "pbpaste")))

(defun copy-region-to-system-clipboard (start end)
  "Copy the region to the system clipboard."
  (interactive "r")
  (when (display-graphic-p)
    (let ((selection-value (buffer-substring-no-properties start end)))
      (x-set-selection 'CLIPBOARD selection-value)
      (message "Region copied to system clipboard"))))

(defun gradle-test ()
  "Run the 'test' task using the Gradle wrapper."
  (interactive)
  (gradle-run-from-root "test"))

(defun gradle-build ()
  "Run the 'build' task using the Gradle wrapper."
  (interactive)
  (gradle-run-from-root "build"))

(defun gradle-run-from-root (task)
  "Run the Gradle task `task` from the top-level directory of the current Git repository."
  (let ((default-directory (projectile-project-root)))
    (compile (concat "./gradlew " task))))

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
                 (file+headline "~/org/tasks.org" "Inbox")
                 "** INBOX %?\n"))
  (add-to-list 'org-capture-templates
               '("d" "Inbox item" entry
                 (file+headline "~/org/tasks.org" "Inbox")
                 "** DOING %?\n"))
  (add-to-list 'org-capture-templates
               '("n" "Inbox item" entry
                 (file+headline "~/org/tasks.org" "Inbox")
                 "** NEXT %?\n"))
  )

(after! org-agenda
  (map! :map org-agenda-mode-map
        "<escape>" #'org-agenda-exit))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   ;; Add other languages here if needed
   ))

;; Org-mode settings
(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "◀── now ─────────────────────────────────────────────────")

;; Ellipsis styling
(setq org-ellipsis "…")
(set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

;; Enable org-modern
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (global-org-modern-mode))

;; Disable .dir-locals.el warning.
(setq enable-local-variables :all)

;; Projectile
(after! projectile
  (setq projectile-known-projects '(
                                    "~/.doom.d/"
                                    "~/org"
                                    "~/life"
                                    "~/src/projects/java-dsa"
                                    "~/src/projects/nuxt-docs-clone"
                                    )
        projectile-completion-system 'ivy
        projectile-auto-discover nil
        projectile-project-search-path nil
        projectile-cache-file (concat doom-cache-dir "projectile.cache")
        projectile-enable-caching t
        ;; counsel-projectile-switch-project-action (lambda (input)
                                                   ;; (message "Custom project switch action!!")
                                                   ;; (treemacs-add-and-display-current-project-exclusively))
        projectile-track-known-projects-automatically nil)
        )

(defun projectile-switch-project-by-name-no-prompt (project-to-switch &optional arg)
  "Switch to project by project name PROJECT-TO-SWITCH.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  ;; let's make sure that the target directory exists and is actually a project
  ;; we ignore remote folders, as the check breaks for TRAMP unless already connected
  (unless (or (file-remote-p project-to-switch) (projectile-project-p project-to-switch))
    (projectile-remove-known-project project-to-switch)
    (error "Directory %s is not a project" project-to-switch))
  (let ((switch-project-action (if arg
                                   'projectile-commander
                                 projectile-switch-project-action)))
    (let* ((default-directory project-to-switch)
           (switched-buffer
            ;; use a temporary buffer to load PROJECT-TO-SWITCH's dir-locals
            ;; before calling SWITCH-PROJECT-ACTION
            (with-temp-buffer
              (hack-dir-local-variables-non-file-buffer)
              ;; Normally the project name is determined from the current
              ;; buffer. However, when we're switching projects, we want to
              ;; show the name of the project being switched to, rather than
              ;; the current project, in the minibuffer. This is a simple hack
              ;; to tell the `projectile-project-name' function to ignore the
              ;; current buffer and the caching mechanism, and just return the
              ;; value of the `projectile-project-name' variable.
              (let ((projectile-project-name (funcall projectile-project-name-function
                                                      project-to-switch)))
                ;; (funcall switch-project-action)
                (current-buffer)))))
      ;; If switch-project-action switched buffers then with-temp-buffer will
      ;; have lost that change, so switch back to the correct buffer.
      (when (buffer-live-p switched-buffer)
        (switch-to-buffer switched-buffer)))))

(defun switch-to-project-by-index (index)
  "Switch to the project by INDEX in `projectile-known-projects`."
  (when (and (>= index 0) (< index (length projectile-known-projects)))
    (setq projectile-project-root (nth index projectile-known-projects))
    (projectile-switch-project-by-name-no-prompt projectile-project-root)))

(defun projectile-create-new-file (filename)
  "Create a new file called FILENAME in the project's root directory."
  (interactive "GNew file name!: ")
  (let ((file (concat (projectile-project-root) filename)))
    (unless (file-exists-p file)
      (write-region "" nil file))
    (find-file file)))

  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

(defun centaur-tabs-buffer-groups ()
  "Group buffers by their Projectile project."
  (if (projectile-project-p)
      (list (projectile-project-name))
    (list "Misc")))

;; Apply the custom grouping function
;; (advice-add 'centaur-tabs-buffer-groups :override #'centaur-tabs-buffer-groups)

(centaur-tabs-mode)

;; Magit
(after! magit
  (map! :map magit-mode-map
        "<escape>" #'magit-mode-bury-buffer))

(defun magit-commit-update ()
  "Commit with message 'Update' in Magit."
  (interactive)
  (magit-commit-create `("-m" "Update")))

;; Ivy
(after! ivy
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))

;;;###autoload
(defun org-trello-pull-buffer (&optional from)
  "Execute the sync of the entire buffer to trello.
If FROM is non nil, execute the sync of the entire buffer from trello."
  (interactive "P")
  (org-trello--apply-deferred
   (cons 'org-trello-log-strict-checks-and-do
         (if from
             '("Request 'sync org buffer from trello board'"
               orgtrello-controller-do-sync-buffer-from-trello)
           '("Request 'sync org buffer from trello board'"
             orgtrello-controller-do-sync-buffer-from-trello)))))

;; LeetCode
(setq leetcode-prefer-language "java")

;; Expand-region
(use-package! expand-region
  :bind ("M-k" . er/expand-region)
  :bind ("M-j" . er/contract-region)
  )

(defun edit-config-file (filename)
  ;; (switch-to-project-by-index 0)
  (find-file filename))

(defun tangle-readme-org ()
  "Tangle the README.org file."
  (org-babel-tangle-file (expand-file-name "README.org" doom-private-dir)))
;; Change!
