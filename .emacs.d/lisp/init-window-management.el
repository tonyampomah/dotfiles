;;; init-window-management.el --- window-manangement -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Frame Scaling / Zooming
(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

;; Window history
(use-package winner
  :after evil
  :config
  (winner-mode)
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

(general-def 'normal
  "C-j" 'evil-window-down
  "C-h" 'evil-window-left
  "C-k" 'evil-window-up
  "C-l" 'evil-window-right)

(provide 'init-window-management)
;;; init-window-management.el ends here
