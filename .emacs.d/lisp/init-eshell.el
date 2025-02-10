;;; init-eshell.el --- eshell -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defalias 'clear 'clear-scrollback)

(defun kd/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq
   eshell-scroll-to-bottom-on-input 'all
   eshell-error-if-no-glob t
   eshell-hist-ignoredups t
   eshell-list-files-after-cd t
   eshell-save-history-on-exit t
   eshell-prefer-lisp-functions nil
   eshell-destroy-buffer-when-process-dies t
   eshell-history-size         10000
   eshell-buffer-maximum-lines 10000
   eshell-scroll-to-bottom-on-input t))

(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(use-package eshell
  :hook (eshell-first-time-mode . kd/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim"))))

;; (use-package eshell-git-prompt
;;   :config
;;   (eshell-git-prompt-use-theme 'powerline))

(use-package eshell-toggle)

(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq pcomplete-cycle-completions nil)))

(use-package fish-completion)

(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))

(global-set-key (kbd "§") #'eshell-toggle)
(global-set-key (kbd "M-<return>") 'projectile-run-eshell)

(provide 'init-eshell)
;;; init-eshell.el ends here
