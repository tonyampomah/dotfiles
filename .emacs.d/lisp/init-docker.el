;;; init-docker.el --- docker -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package dockerfile-mode
  :defer t)
(use-package docker-compose-mode
  :defer t)
(use-package docker
  :defer t)
(kd/leader-key-def
  "d"  '(:ignore t :which-key "docker")
  "dd"  'docker
  "di" 'docker-images
  "dc" 'docker-containers
  "dC" 'docker-compose
  "dv" 'docker-volumes
  "dn" 'docker-networks
  "dM" 'docker-machines)

(provide 'init-docker)
;;; init-docker.el ends here
