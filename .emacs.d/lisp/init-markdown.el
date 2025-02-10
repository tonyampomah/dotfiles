;;; init-markdown.el --- markdown -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "pandoc --no-highlight"))

(provide 'init-markdown)
;;; init-markdown.el ends here
