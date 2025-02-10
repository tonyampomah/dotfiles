;;; init-debugger.el --- debugger -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package dap-mode
  :defer t
  :commands dap-mode
  :config
  (dap-mode 1)
  (require 'dap-php)
  (require 'dap-ui)
  (require 'dap-php)
  (require 'dap-lldb)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

(provide 'init-debugger)
;;; init-debugger.el ends here
