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
