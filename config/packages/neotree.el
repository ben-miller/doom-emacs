(defun neotree-project-dir ()
  "Open NeoTree using the perspective's project path, the projectile project root, or the current buffer's directory."
  (interactive)
  (let* ((current-persp-name (persp-name (persp-curr)))  ;; Get current perspective name
         (persp-project-path (cdr (assoc current-persp-name persp-proj-map)))  ;; Lookup perspective path
         (project-dir (projectile-project-root))  ;; Get the projectile project root
         (file-name (buffer-file-name)))  ;; Get current buffer file name
    (neotree-toggle)
    (cond
     ;; If perspective path is available, use it
     (persp-project-path
      (if (neo-global--window-exists-p)
          (progn
            (neotree-dir persp-project-path)
            (when file-name
              (neotree-find file-name)))
        (message "NeoTree window does not exist.")))

     ;; If project root is available (from projectile), use it
     (project-dir
      (if (neo-global--window-exists-p)
          (progn
            (neotree-dir project-dir)
            (when file-name
              (neotree-find file-name)))
        (message "NeoTree window does not exist.")))

     ;; Fallback to the directory of the current buffer file
     (file-name
      (let ((buffer-dir (file-name-directory file-name)))
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir buffer-dir)
              (neotree-find file-name))
          (message "NeoTree window does not exist."))))

     ;; If none of the above work, display an error message
     (t (message "Could not determine project directory.")))))


(after! neotree
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 40))
