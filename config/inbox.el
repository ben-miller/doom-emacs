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
        projectile-track-known-projects-automatically nil
        ))

(add-hook 'doom-after-init-hook
          (lambda () (doom/quickload-session t)))

(after! treemacs
  (setq treemacs-width 30))

(after! persp-mode
  (setq persp-autosave-fname "autosave"
        persp-save-dir (concat doom-cache-dir "workspaces/")
        persp-auto-save-opt 1
        persp-auto-resume-time 0))

(after! treemacs
  (setq treemacs-persist-file (concat doom-cache-dir "treemacs-persist"))
  (treemacs-follow-mode nil)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'deferred))

