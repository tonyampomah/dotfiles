;;; init-smart-parens.el --- smart-parens -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package smartparens
  :diminish
  :defer t
  :hook (prog-mode . smartparens-mode))

;; Hightlight Matching Braces
(use-package paren
  :diminish
  :defer t
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(provide 'init-smart-parens)
;;; init-smart-parens.el ends here
