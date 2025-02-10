;;; init-directory-clean.el --- Keep Emacs Directory Clean -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq create-lockfiles nil)
(setq backup-directory-alist `(("." . ,(expand-file-name "/tmp/backups/" user-emacs-directory))))
;; (setq make-backup-files nil)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(provide 'init-directory-clean)
;;; init-directory-clean.el ends here
