;;; init-yaml.el --- yaml -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package ansible
  :defer t)

(use-package ansible-doc
  :defer t)

(use-package jinja2-mode
  :defer t)

(use-package company-ansible
  :defer t)

(use-package yaml-mode
  :defer t
  :mode "Procfile\\'")
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)

(provide 'init-yaml)
;;; init-yaml.el ends here
