(defun my/only-one-visible-tab-p ()
  "Return t if there is only one visible tab in the current tab group."
  (let ((tabs (centaur-tabs-view (centaur-tabs-current-tabset t))))
    (eq (length tabs) 1)))
