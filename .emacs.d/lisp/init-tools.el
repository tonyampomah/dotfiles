;;; init-tools.el --- tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Docker
(use-package dockerfile-mode
  :defer t)

(use-package docker-compose-mode
  :defer t)

(use-package docker
  :defer t)

(use-package ztree
  :defer t)

(kd/leader-key-def
  "d"  '(:ignore t :which-key "docker")
  "di" 'docker-images
  "dc" 'docker-containers
  "dC" 'docker-compose
  "dv" 'docker-volumes
  "dn" 'docker-networks
  "dM" 'docker-machines)

;; Vagrant
(use-package vagrant
  :defer t)

(use-package vagrant-tramp
  :defer t)


;; Editorconfig
(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode 1))


;; Elastic Search
(use-package es-mode
  :defer t)
(autoload 'es-mode "es-mode.el"
  "Major mode for editing Elasticsearch queries" t)
(add-to-list 'auto-mode-alist '("\\.es$" . es-mode))


;; Debugging
;; (use-package dap-mode
;;   :defer t
;;   :commands dap-mode
;;   :config
;;   (dap-mode 1)
;;   (require 'dap-php)
;;   (require 'dap-ui)
;;   (require 'dap-php)
;;   (require 'dap-lldb)
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (tooltip-mode 1))


(provide 'init-tools)
;;; init-tools.el ends here
