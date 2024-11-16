
(defun neotree-dir-from-persp ()
  "Open NeoTree with the directory of the current perspective and maintain window focus."
  (interactive)
  (let* ((current-window (selected-window))
         (current-persp-name (persp-name (persp-curr)))
         (persp-project-path (cdr (assoc current-persp-name persp-proj-map))))
    (message "neotree-dir-from-persp: %s %s" (persp-name (persp-curr)) persp-project-path)
    (cond
     ;; If perspective path is available, use it
     (persp-project-path
      (when (neo-global--window-exists-p)
        (neotree-dir persp-project-path)))
     ;; Default to home directory if no project path is found
     (t
      (when (neo-global--window-exists-p)
        (neotree-dir "~"))))
    ;; Restore focus to the original window
    (select-window current-window)))

(after! neotree
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 40))
