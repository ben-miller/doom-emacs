
(defun neotree-dir-from-persp ()
  (neotree-toggle)
  (let* ((current-persp-name (persp-name (persp-curr)))
         (persp-project-path (cdr (assoc current-persp-name persp-proj-map))))
    (message (concat "persp name: " (prin1-to-string current-persp-name)))
    (cond
     ;; If perspective path is available, use it
     (persp-project-path
      (if (neo-global--window-exists-p)
          (progn
            (neotree-dir persp-project-path))
        (message "NeoTree window does not exist.")))

     (t
      (if (neo-global--window-exists-p)
          (progn
            (neotree-dir "~")))))))


(defun neotree-project-dir ()
  (interactive)
  (neotree-dir-from-persp))

(after! neotree
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 40))
