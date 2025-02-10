;;; init-dotenv.el --- dotenv -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dotenv-mode
  :defer t)
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

(provide 'init-dotenv)
;;; init-dotenv.el ends here
