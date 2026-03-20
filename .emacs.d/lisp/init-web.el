;;; init-web.el --- Web -*- lexical-binding: t -*-
;;; Commentary: For HTML, Templates like blade etc
;;; Code:

(use-package emmet-mode)
(use-package web-beautify
  :defer t)

(setq major-mode-remap-alist
      '((javascript-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(provide 'init-web)
;;; init-web.el ends here
