;;; init-formatting.el --- formatting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (use-package apheleia
;;   :ensure apheleia
;;   :diminish ""
;;   :defines
;;   apheleia-formatters
;;   apheleia-mode-alist
;;   :functions
;;   apheleia-global-mode
;;   :config
;;   (setf (alist-get 'prettier-json apheleia-formatters)
;;         '("prettier" "--stdin-filepath" filepath))
;;   (apheleia-global-mode +1))


(use-package apheleia
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              "--trailing-comma"  "es5"
              "--tab-width"       "2"
              "--semi"            "true"
              "--single-quote"    "true"
              "--quote-props"    "as-needed"
              "--bracket-same-line" "true"
              file))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(rjsx-mode . prettier))
  (apheleia-global-mode t))

(provide 'init-formatting)
;;; init-formatting.el ends here
