;;; init-configuration-files.el --- configuration-files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun kd/find-in-emacs-directory ()
  (interactive)
  (counsel-find-file "~/.emacs.d"))

(defun kd/visit-host-file ()
  (interactive)
  (find-file "/sudo:root@localhost:/etc/hosts"))

(defun kd/visit-ssh-config ()
  (interactive)
  (counsel-find-file "~/.ssh"))

(defun kd/find-in-notes ()
  (interactive)
  (counsel-find-file "~/org"))

(defun kd/find-home-directory ()
  (interactive)
  (find-file "~/"))

(defun kd/sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun kd/sudo-this-file (&optional arg)
  "Edit currently visited file as root.

  With a prefix ARG prompt for a file to visit.
  Will also prompt for a file to visit if current
  buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(kd/leader-key-def
  "fe"  'kd/find-in-emacs-directory
  "fC"  'itecytony/copy-this-file
  "fvp" 'kd/visit-emacs-config
  "fdd" '(lambda() (interactive) (find-file "~/Downloads"))
  "fdD" '(lambda() (interactive) (find-file "~/Documents"))
  "f1"  '(lambda() (interactive) (find-file "~/Repo/1Projects"))
  "f2"  '(lambda() (interactive) (find-file "~/Repo/2Areas"))
  "f3"  '(lambda() (interactive) (find-file "~/Repo/3Resources"))
  "f4"  '(lambda() (interactive) (find-file "~/Repo/4Archives"))
  "fw1"  '(lambda() (interactive) (find-file "~/RadiusRepo/1Projects"))
  "fw2"  '(lambda() (interactive) (find-file "~/RadiusRepo/2Areas"))
  "fw3"  '(lambda() (interactive) (find-file "~/RadiusRepo/3Resources"))
  "fw4"  '(lambda() (interactive) (find-file "~/RadiusRepo/4Archives"))

  "fD"  'delete-file
  "ff"  'counsel-find-file
  "fh"  'kd/find-home-directory
  "fvh"  'kd/visit-host-file
  "fvs"  'kd/visit-ssh-config
  "fu"  'kd/sudo-find-file
  "fU"  'kd/sudo-this-file
  "fl"  'locate
  "fr"  'recentf-open-files
  "fs"  'save-buffer
  "fS"  'write-file
  "ft"  'counsel-tramp
  )

(provide 'init-configuration-files)
;;; init-configuration-files.el ends here
