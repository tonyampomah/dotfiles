;;; init-elastic-search.el --- elastic-search -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package es-mode
  :defer t)
(autoload 'es-mode "es-mode.el"
  "Major mode for editing Elasticsearch queries" t)
(add-to-list 'auto-mode-alist '("\\.es$" . es-mode))


(provide 'init-elastic-search)
;;; init-elastic-search.el ends here
