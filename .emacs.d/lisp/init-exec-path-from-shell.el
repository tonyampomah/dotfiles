;;; init-exec-path-from-shell.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package exec-path-from-shell
  :demand t
  :init
  ;; Avoid warnings if startup files changed
  (setq exec-path-from-shell-check-startup-files nil)
  ;; Import environment variables from the shell
  (exec-path-from-shell-initialize))


(provide 'init-exec-path-from-shell)
;;; init-exec-path-from-shell.el ends here
