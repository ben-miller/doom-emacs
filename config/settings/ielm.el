(defun ielm-reload ()
  "Reload the current IELM buffer."
  (interactive)
  (let ((ielm-buffer (current-buffer)))
    (with-current-buffer ielm-buffer
      (comint-send-eof))
    (let ((buffer-name (buffer-name ielm-buffer)))
      (kill-buffer ielm-buffer)
      (ielm)
      (rename-buffer buffer-name))))
