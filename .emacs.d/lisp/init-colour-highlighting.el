;;; init-colour-highlighting.el --- colour-highlighting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish
  :defer t
  :hook (org-mode
	 emacs-lisp-mode
	 web-mode
	 typescript-mode
	 js2-mode))

(provide 'init-colour-highlighting)
;;; init-colour-highlighting.el ends here
