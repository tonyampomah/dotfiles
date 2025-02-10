;;; init-kubernetes.el --- kubernetes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package kubernetes
  :defer t)

(use-package kubernetes-evil
  :defer t)

;; (kd/leader-key-def
;;   "d"  '(:ignore t :which-key "kubernetes")
;;   "dd"  'kubernetes
;;   "di" 'kubernetes-images
;;   "dc" 'kubernetes-containers
;;   "dC" 'kubernetes-compose
;;   "dv" 'kubernetes-volumes
;;   "dn" 'kubernetes-networks
;;   "dM" 'kubernetes-machines)

(provide 'init-kubernetes)
;;; init-kubernetes.el ends here
