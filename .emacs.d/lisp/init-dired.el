;;; init-dired --- Dired -*- lexical-binding: t -*-
;;; Commentary: Emacs Directory Editor
;;; Code:
(use-package all-the-icons-dired :defer t)

(use-package dired
  :ensure nil
  :straight nil
  :defer t
  :commands (dired dired-jump)
  :config
  (setq 
   dired-dwim-target t
   delete-by-moving-to-trash t
   dired-clean-up-buffers-too t
   dired-recursive-copies 'always
   dired-recursive-deletes 'top
   ;; dired-listing-switches "lhvA"
   dired-omit-verbose nil
   dired-hide-details-hide-symlink-targets nil)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              ;; (dired-omit-mode 1)
              (dired-hide-details-mode 1)
              (hl-line-mode 1)))

  (if (eq system-type 'darwin)
      (setq trash-directory "~/.Trash"))

  (use-package dired-single
    :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t)

  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

  (use-package dired-hide-dotfiles
    :demand t
    :config
    (dired-hide-dotfiles-mode))

  (evil-collection-define-key 'normal 'dired-mode-map
    "." 'dired-hide-dotfiles-mode
    "h" 'dired-single-up-directory
    "H" 'dired-omit-mode
    "l" 'dired-single-buffer
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))

;; This allows me to  uncompress a .zip file
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes 
                '("\\.zip\\'" ".zip" "unzip")))

(use-package async
  :defer t
  :config
  (dired-async-mode 1))

(use-package dired-open
  ;; :hook (dired-mode . dired-open-hook)
  ;; :defer t
  :ensure t
  :config
  (setq dired-open-extensions
        '(("avi" . "mpv")
          ("cbr" . "comix")
          ("doc" . "libreoffice")
          ("docx" . "libreoffice")
          ("gif" . "ffplay")
          ("gnumeric" . "gnumeric")
          ("jpeg" . "eog")
          ("jpg" . "eog")
          ("mkv" . "mpv")
          ("mov" . "mpv")
          ("mp3" . "mpc")
          ("mp4" . "mpv")
          ("pdf" . "evince")
          ("docx" . "libreoffice")
          ("epub" . "foliate")
          ("png" . "feh")
          ("webm" . "mpv")
          ("xls" . "gnumeric")
          ("xlsx" . "gnumeric"))))

(provide 'init-dired)
;;; init-dired.el ends here
