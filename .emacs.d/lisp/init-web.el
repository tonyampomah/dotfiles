;;; init-web.el --- Web Mode -*- lexical-binding: t -*-
;;; Commentary: For HTML, Templates like blade etc
;;; Code:

(use-package emmet-mode)

(use-package web-mode
  :defer t
  :mode (
	 ("\\.js\\'" .  web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" .  web-mode)
	 ("\\.tsx\\'" .  web-mode)
	 ("\\.vue\\'" . web-mode)
	 ("\\.hbs\\'" . web-mode)
	 ("\\.html\\'" . web-mode))

  :config
  (setq web-mode-attr-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-indent-style 2
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset 2)

  (setq web-mode-ac-sources-alist
        '(("php" . (ac-source-php-extras
                    ac-source-yasnippet
                    ac-source-gtags
                    ac-source-abbrev
                    ac-source-dictionary
                    ac-source-words-in-same-mode-buffers))
          ("css" . (ac-source-css-property
                    ac-source-abbrev
                    ac-source-dictionary
                    ac-source-words-in-same-mode-buffers))))

  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-style-padding 2)
              (yas-minor-mode t)
              (emmet-mode)
              ;; (flycheck-add-mode 'html-tidy 'web-mode)
              (flycheck-mode)))

  (add-hook 'web-mode-before-auto-complete-hooks
            #'(lambda ()
		(let ((web-mode-cur-language (web-mode-language-at-pos)))
                  (if (string= web-mode-cur-language "php")
                      (yas-activate-extra-mode 'php-mode)
                    (yas-deactivate-extra-mode 'php-mode))
                  (if (string= web-mode-cur-language "css")
                      (setq emmet-use-css-transform t)
                    (setq emmet-use-css-transform nil))))))

(use-package web-beautify
  :defer t)

(straight-use-package
 '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss"))

(provide 'init-web)
;;; init-web.el ends here
