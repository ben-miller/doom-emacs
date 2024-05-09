;; -*- no-byte-compile: t; -*-
;;
;; Unsorted config functions, etc.

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

(defun magit-commit-update ()
  "Commit with message 'Update' in Magit."
  (interactive)
  (magit-commit-create `("-m" "Update")))

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
  (gradle-run-from-root "test"))

(defun gradle-run-from-root (task)
  "Run the Gradle task `task` from the top-level directory of the current Git repository."
  (let ((default-directory (projectile-project-root)))
    (compile (concat "./gradlew " task))))

(after! projectile
  (setq projectile-known-projects '(
                                    "~/.doom.d/"
                                    "~/org"
                                    "~/life"
                                    "~/src/projects/java-dsa"
                                    )
        projectile-completion-system 'ivy
        projectile-auto-discover nil
        projectile-project-search-path nil
        projectile-cache-file (concat doom-cache-dir "projectile.cache")
        projectile-enable-caching t
        projectile-track-known-projects-automatically nil))

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

(add-hook 'doom-after-init-hook
          (lambda () (doom/quickload-session t)))

(after! persp-mode
  (setq persp-autosave-fname "autosave"
        persp-save-dir (concat doom-cache-dir "workspaces/")
        persp-auto-save-opt 1
        persp-auto-resume-time 0))

(defun treemacs-remove-project-from-workspace-no-prompt (&optional arg)
  "Remove the project at point from the current workspace without prompting.
With a prefix ARG select project to remove by name."
  (interactive "P")
  (let ((project (treemacs-project-at-point))
        (save-pos))
    (when (or arg (null project))
      (setf project (treemacs--select-project-by-name)
            save-pos (not (equal project (treemacs-project-at-point)))))
    (pcase (if save-pos
               (treemacs-save-position
                (treemacs-do-remove-project-from-workspace project nil nil))
             (treemacs-do-remove-project-from-workspace project nil nil))
      (`success
       (whitespace-cleanup)
       (treemacs-pulse-on-success "Removed project %s from the workspace."
         (propertize (treemacs-project->name project) 'face 'font-lock-type-face)))
      (`user-cancel
       (ignore))
      (`cannot-delete-last-project
       (treemacs-pulse-on-failure "Cannot delete the last project."))
      (`(invalid-project ,reason)
       (treemacs-pulse-on-failure "Cannot delete project: %s"
         (propertize reason 'face 'font-lock-string-face))))))

(after! treemacs
  (setq treemacs-width 36)
  (treemacs-follow-mode nil)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred)
  (define-key treemacs-mode-map (kbd "D") #'treemacs-remove-project-from-workspace-no-prompt))

(defun edit-config-file (filename)
  (switch-to-project-by-index 0)
  (find-file filename))
