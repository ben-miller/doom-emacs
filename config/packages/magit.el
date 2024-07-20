(after! magit
  (map! :map magit-mode-map
        "<escape>" #'magit-mode-bury-buffer)
  (remove-hook 'magit-mode-hook #'flyspell-mode))

(defun magit-commit-update ()
  "Commit with message 'Update' in Magit."
  (interactive)
  (magit-commit-create `("-m" "Update")))
