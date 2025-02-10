;;; init-package-management.el --- Package Manangement -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; * PACKAGE MANAGEMENT

(defmacro comment (&rest body)
  "Ignore forms in BODY, returning nil. Used for rich comments."
  nil)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file :no-error-if-file-is-missing)

;; DEBUG
;; (setq debug-on-error t)
;; (setq debug-on-quit t)


;; ** ELPA
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; ** USE-PACKAGE
(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

;; Don't show byte compilation warnings when installing packages
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(require 'use-package)

(use-package use-package
  :no-require
  :custom
  (use-package-enable-imenu-support t))

(defvar os/emacs-tmp-dir (concat user-emacs-directory "tmp/")
  "Scratch space for stuff...")

(setq package-user-dir (concat user-emacs-directory "elpa"))
(setq package-native-compile t)

;; ** STRAIGHT
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(setq straight-check-for-modifications '(watch-files find-when-checking))

(provide 'init-package-management)
;;; init-package-management.el ends here
