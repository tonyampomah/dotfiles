;;; init-writing.el --- writing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package edit-indirect
  :defer t)

(use-package writeroom-mode
  :defer t
  :config
  (setq writeroom-maximize-window nil
	writeroom-width 80
	writeroom-mode-line t
	writeroom-fullscreen-effect 'fullboth))

(provide 'init-writing)
;;; init-writing.el ends here
