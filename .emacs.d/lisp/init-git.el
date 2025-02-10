;;; init-git.el --- Git -*- lexical-binding: t -*-
;;; Commentary: The magical git client.
;;; Code:

;;; Magit
(use-package magit
  :defer t
  :init
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :commands (magit-status magit-blame magit-log-buffer-file magit-log-all)
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package browse-at-remote
  :defer t)
(use-package with-editor
  :defer t )
(use-package magit-todos
  :defer t )
(use-package magit-gitflow
  :defer t )
(use-package yagist
  :defer t )
(use-package bug-reference-github
  :defer t )
(use-package github-review
  :defer t )

;;; Highlight uncommitted changes
(use-package diff-hl
  :defer t
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(kd/leader-key-def
  ;;   ;;; <leader> g --- git
  "gR"  'vc-revert
  "gg"  'magit-status
  "gb"  'magit-branch-checkout
  "g/"  'magit-dispatch
  "gD"  'magit-file-delete
  "gB"  'magit-blame-addition
  "gC"  'magit-clone
  "gF"  'magit-fetch
  "gL"  'magit-log
  "gS"  'magit-stage-file
  "gU"  'magit-unstage-file)

(provide 'init-git)
;;; init-git.el ends here
