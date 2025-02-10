;;; init-syntax-checking.el --- syntax-checking -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package flycheck
;;   :defer t
;;   :config
;;   (setq flycheck-emacs-lisp-load-path 'inherit
;;         flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
;;         flycheck-check-syntax-automatically '(save idle-change mode-enabled)
;;         flycheck-idle-change-delay 0.2
;;         flycheck-global-modes '(not org-mode)
;; 	flycheck-phpcs-standard "PSR12")
;;   (global-flycheck-mode))

;; (use-package flycheck-yamllint
;;   :ensure t
;;   :defer t
;;   :init
;;   (progn
;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

(provide 'init-syntax-checking)
;;; init-syntax-checking.el ends here
