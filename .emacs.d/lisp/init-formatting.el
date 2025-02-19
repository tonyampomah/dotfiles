;;; init-formatting.el --- formatting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (use-package apheleia
;;   :config
;;   (setf (alist-get 'prettier apheleia-formatters)
;;         '(npx "prettier"
;;               "--trailing-comma"  "es5"
;;               "--tab-width"       "2"
;;               "--semi"            "true"
;;               "--single-quote"    "true"
;;               "--quote-props"    "as-needed"
;;               "--bracket-same-line" "true"
;;               file))
;;   (add-to-list 'apheleia-mode-alist '(web-mode . prettier))
;;   (add-to-list 'apheleia-mode-alist '(yaml-mode . prettier))
;;   )

(setq prettier-js-args '(
                "--trailing-comma"  "es5"
                "--tab-width"       "2"
                "--semi"            "true"
                "--single-quote"    "true"
                "--quote-props"    "as-needed"
                "--bracket-same-line" "true"
))
(use-package prettier-js
  :ensure t)

(add-hook 'web-mode-hook #'(lambda ()
			     (enable-minor-mode
			      '("\\.tsx?\\'" . prettier-js-mode))
			     (enable-minor-mode
			      '("\\.jsx?\\'" . prettier-js-mode))
			     ))


(provide 'init-formatting)
;;; init-formatting.el ends here
