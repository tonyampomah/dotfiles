;;; init-projectile.el --- Projectile -*- lexical-binding: t -*-
;;; Commentary: Manage projects inside of Emacs
;;; Code:
(use-package projectile
  :diminish
  :defer t
  :config
  (setq projectile-project-search-path '(
					 "~/Documents/Joint/2Areas"
					 "~/Documents/Joint/1Projects"
					 "~/Documents/Personal/2Areas"
					 "~/Documents/Personal/1Projects"
					 "~/RadiusRepo/3Resources"
					 "~/RadiusRepo/2Areas"
					 "~/RadiusRepo/1Projects"
					 "~/Repo/3Resources"
					 "~/Repo/2Areas"
					 "~/Repo/1Projects"
					 "~/"
					 ))
  (setq projectile-completion-system 'ivy)
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-mode))

(kd/leader-key-def
	;;; <leader> p --- projects
  "pp"  'projectile-switch-project
  "pi"  'projectile-invalidate-cache
  "pt"  'projectile-run-shell
  "pv"  'projectile-run-vterm
  "pe"  'projectile-run-eshell
  "pb"  'persp-counsel-switch-buffer
  "ps"  'counsel-projectile-rg
  "pc"  'projectile-compile-project
  "pB"  'counsel-switch-buffer
  "pd"  'projectile-discover-projects-in-search-path)


(global-set-key (kbd "M-p") 'projectile-find-file)
(global-set-key (kbd "M-S-p") 'projectile-switch-project)
(global-set-key (kbd "H-p") 'projectile-find-file)

(provide 'init-projectile)
;;; init-projectile.el ends here
