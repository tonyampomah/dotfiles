;;; init-notifications.el --- notifications -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

(provide 'init-notifications)
;;; init-notifications.el ends here
