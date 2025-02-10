;;; init-user-interface.el --- User Interface Tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Clean up Emacs' user interface, make it more minimal.
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar


;; (global-visual-line-mode 1)

;;; Set up the visible bell
(setq visible-bell t)

;;; Improve scrolling
(setq 
 mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
 mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 mouse-wheel-follow-mouse 't ;; scroll window under mouse
 scroll-step 1 ;; keyboard scroll one line at a time
 use-dialog-box nil)

;;; Enable line numbers and customize their format.
;; (column-number-mode)

;; Enable line numbers for some modes
(setq display-line-numbers-type 'relative)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Don't warn for large files (shows up when launching videos)
(setq large-file-warning-threshold nil)

;;; Don't warn for following symlinked files
(setq vc-follow-symlinks t)

;;; Don't warn when advice is added for functions
(setq ad-redefinition-action 'accept)

;;; Don't show me Emacs startup message
(setq inhibit-startup-message t
      initial-scratch-message nil)

;;; Don't show me flashing ring bell at the top
(setq ring-bell-function 'ignore)

;;; Stop blinking cursor
(blink-cursor-mode 0)

;;; Let yes be y and no be n please
(fset 'yes-or-no-p 'y-or-n-p)

(global-prettify-symbols-mode +1)

(use-package rainbow-delimiters)

(use-package command-log-mode)

(provide 'init-user-interface)
;;; init-user-interface.el ends here
