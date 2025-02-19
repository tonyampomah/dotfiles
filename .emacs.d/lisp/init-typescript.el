;;; init-typescript.el --- Typescript -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (use-package tsi
;;   :after tree-sitter
;;   :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
;;   ;; define autoload definitions which when actually invoked will cause package to be loaded
;;   :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;   :init
;;   (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;   (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;   (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
;;   (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(provide 'init-typescript)
;;; init-typescript.el ends here
