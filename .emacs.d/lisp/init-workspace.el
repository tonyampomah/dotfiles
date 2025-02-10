;;; init-workspace.el --- workspace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (setq persp-sort 'created)
  (persp-mode))

(use-package persp-projectile)

(kd/leader-key-def
        ;;; <leader> w --- workspaces
  "<tab><tab>" 'persp-switch
  "<tab>[" 'persp-prev
  "<tab>]" 'persp-next
  "<tab><left>" 'persp-prev
  "<tab><right>" 'persp-next
  "<tab>n" 'persp-switch
  "<tab>s" 'persp-state-save
  "<tab>a" 'persp-add-buffer
  "<tab>d" 'persp-kill
  "<tab>b" 'persp-set-buffer
  "<tab>r" 'persp-rename
  "<tab>k" 'persp-remove-buffer
  "<tab>i" 'persp-import
  "<tab>l" 'persp-state-load
  "<tab>." 'persp-switch
  )

(global-set-key (kbd "M-1") #'(lambda () (interactive) (persp-switch-by-number 1)))
(global-set-key (kbd "M-2") #'(lambda () (interactive) (persp-switch-by-number 2)))
(global-set-key (kbd "M-3") #'(lambda () (interactive) (persp-switch-by-number 3)))
(global-set-key (kbd "M-4") #'(lambda () (interactive) (persp-switch-by-number 4)))
(global-set-key (kbd "M-5") #'(lambda () (interactive) (persp-switch-by-number 5)))
(global-set-key (kbd "M-6") #'(lambda () (interactive) (persp-switch-by-number 6)))
(global-set-key (kbd "M-7") #'(lambda () (interactive) (persp-switch-by-number 7)))
(global-set-key (kbd "M-8") #'(lambda () (interactive) (persp-switch-by-number 8)))
(global-set-key (kbd "M-9") #'(lambda () (interactive) (persp-switch-by-number 9)))

(provide 'init-workspace)
;;; init-workspace.el ends here
