;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)
;;;; per https://github.com/emacs-lsp/lsp-mode#performance
(setq read-process-output-max (* 10 1024 1024)) ;; 10mb
(setq gc-cons-threshold 200000000)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq package-enable-at-startup nil)
(setq tramp-histfile-override "/dev/null")
(setq org-element-use-cache nil)
(setq tramp-default-method "ssh")
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))
(setq auth-sources '("~/.authinfo.gpg"))
(setq initial-major-mode 'org-mode)

;; Native Compilation
(setq comp-async-report-warnings-errors nil)
(setq native-comp-async-report-warnings-errors nil)

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

(defconst *is-a-mac* (eq system-type 'darwin))

(defun conditional-disable-modes ()
  (when (> (buffer-size) (* 3 1024 1024))
    (flycheck-mode -1)
    (font-lock-mode -1)
    (fundamental-mode)
    (which-function-mode -1)
    (linum-mode 0)
    )
  )
(add-hook 'prog-mode-hook 'conditional-disable-modes)
(add-hook 'text-mode-hook 'conditional-disable-modes)


;; Bootstrap config
(require 'init-package-management)
(require 'init-exec-path-from-shell)
(require 'init-directory-clean)
(require 'init-keyboard-bindings)

;; General Configuration
(require 'init-fonts)
(require 'init-user-interface)
(require 'init-themes)
(require 'init-modeline)
(require 'init-completion)
(require 'init-workspace)
(require 'init-configuration-files)
(require 'init-search-n-lookup)

;; File Browsing
(require 'init-dired)

;; Window
(require 'init-window-management)

;; Development
(require 'init-git)
(require 'init-projectile)

;; Tools
(require 'init-docker)
(require 'init-vagrant)
(require 'init-editorconfig)
(require 'init-debugger)
(require 'init-lsp)
(require 'init-formatting)

;; Productivity
(require 'init-syntax-checking)
(require 'init-spell-checking)
(require 'init-grammar-checking)
(require 'init-autocompletion)
(require 'init-snippets)
(require 'init-multiple-cursors)
(require 'init-smart-parens)
(require 'init-colour-highlighting)
(require 'init-writing)

;; Languages
(require 'init-php)
(require 'init-web)
(require 'init-javascript)
(require 'init-typescript)
(require 'init-dotenv)
(require 'init-css)
(require 'init-robot)
(require 'init-haskell)
(require 'init-restclient)
(require 'init-csv)
(require 'init-markdown)
(require 'init-fountain)
(require 'init-yaml)
(require 'init-finance)
(require 'init-org-mode)

;; Applications
(kd/leader-key-def
  "a"  '(:ignore t :which-key "apps"))
(require 'init-calendar)
(require 'init-alert)
(require 'init-chat)
(require 'init-eshell)
(require 'init-terminal)
(require 'init-password-manager)
(require 'init-reading)
(require 'init-touch-typing)
(require 'init-mail)
;; (require 'init-eaf)
;;; init.el ends here
