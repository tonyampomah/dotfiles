;;; ~/.config/doom/autoload/tonyampomah.el -*- lexical-binding: t; -*-
;;;###autoload
(defun tonyampomah/day-view ()
  (interactive)
  (progn (org-agenda nil "a")
	 (org-agenda-day-view)))

;;;###autoload
(defun tonyampomah/week-view ()
  "Show 3 days before today and 5 days after today."
  (interactive)
  (let ((org-agenda-span 9)
        (org-agenda-start-day "-3d"))
    (org-agenda-list)))

;;;###autoload
(defun tonyampomah/month-view ()
  (interactive)
  (progn (org-agenda nil "a")
	 (org-agenda-month-view)))

;;;###autoload
(defun tonyampomah/visit-host-file ()
  (interactive)
  (find-file "/sudo:root@localhost:/etc/hosts"))

;;;###autoload
(defun tonyampomah/visit-ssh-config ()
  (interactive)
  (find-file "~/.ssh/config"))

