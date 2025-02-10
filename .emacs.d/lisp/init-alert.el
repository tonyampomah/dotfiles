;;; init-alert.el --- Alert -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package alert
  :demand t
  :defer t
  :commands alert
  :config
  (setq alert-default-style 'notifications))

(provide 'init-alert)
;;; init-alert.el ends here
