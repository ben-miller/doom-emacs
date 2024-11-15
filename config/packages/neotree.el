
(defun neotree-dir-from-persp ()
  "Open NeoTree with the directory of the current perspective and maintain window focus."
  (let* ((current-window (selected-window))  ;; Save the current window
         (current-persp-name (persp-name (persp-curr)))
         (persp-project-path (cdr (assoc current-persp-name persp-proj-map))))
    ;; Toggle NeoTree and set directory based on perspective
    (neotree-toggle)
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

(defun neotree-project-dir ()
  (interactive)
  (neotree-dir-from-persp))

(after! neotree
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 40))
