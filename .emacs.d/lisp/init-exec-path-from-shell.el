;;; init-exec-path-from-shell.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package exec-path-from-shell
  :defer t
  :demand t
  :init
  (setq exec-path-from-shell-check-startup-files nil))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path-from-shell)
;;; init-exec-path-from-shell.el ends here
