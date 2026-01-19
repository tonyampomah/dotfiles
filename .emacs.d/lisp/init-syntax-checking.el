;;; init-syntax-checking.el --- syntax-checking -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

(provide 'init-syntax-checking)
;;; init-syntax-checking.el ends here
