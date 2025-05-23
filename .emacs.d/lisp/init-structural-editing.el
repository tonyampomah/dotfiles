;;; init-structural-editing.el --- structural-editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
	(bash-mode . bash-ts-mode)
	(js2-mode . js-ts-mode)
	(typescript-mode . typescript-ts-mode)
	(json-mode . json-ts-mode)
	(css-mode . css-ts-mode)
	(python-mode . python-ts-mode)))
;; (use-package treesit
;;   :mode (("\\.tsx\\'" . tsx-ts-mode)
;;          ("\\.js\\'"  . typescript-ts-mode)
;;          ("\\.mjs\\'" . typescript-ts-mode)
;;          ("\\.mts\\'" . typescript-ts-mode)
;;          ("\\.cjs\\'" . typescript-ts-mode)
;;          ("\\.ts\\'"  . typescript-ts-mode)
;;          ("\\.jsx\\'" . tsx-ts-mode)
;;          ("\\.json\\'" .  json-ts-mode)
;;          ("\\.Dockerfile\\'" . dockerfile-ts-mode)
;;          ("\\.prisma\\'" . prisma-ts-mode)
;;          ;; More modes defined here...
;;          )
;;   :preface
;;   (defun os/setup-install-grammars ()
;;     "Install Tree-sitter grammars if they are absent."
;;     (interactive)
;;     (dolist (grammar
;;              '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
;;                (bash "https://github.com/tree-sitter/tree-sitter-bash")
;;                (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
;;                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
;;                (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
;;                (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
;;                (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
;;                (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;                (make "https://github.com/alemuller/tree-sitter-make")
;;                (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;                (cmake "https://github.com/uyha/tree-sitter-cmake")
;;                (c "https://github.com/tree-sitter/tree-sitter-c")
;;                (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;                (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
;;                (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
;;                (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
;;                (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
;;       (add-to-list 'treesit-language-source-alist grammar)
;;       ;; Only install `grammar' if we don't already have it
;;       ;; installed. However, if you want to *update* a grammar then
;;       ;; this obviously prevents that from happening.
;;       (unless (treesit-language-available-p (car grammar))
;;         (treesit-install-language-grammar (car grammar)))))

;;   ;; Optional, but recommended. Tree-sitter enabled major modes are
;;   ;; distinct from their ordinary counterparts.
;;   ;;
;;   ;; You can remap major modes with `major-mode-remap-alist'. Note
;;   ;; that this does *not* extend to hooks! Make sure you migrate them
;;   ;; also
;;   (dolist (mapping
;;            '((python-mode . python-ts-mode)
;;              (css-mode . css-ts-mode)
;;              (typescript-mode . typescript-ts-mode)
;;              (js-mode . typescript-ts-mode)
;;              (js2-mode . typescript-ts-mode)
;;              (c-mode . c-ts-mode)
;;              (c++-mode . c++-ts-mode)
;;              (c-or-c++-mode . c-or-c++-ts-mode)
;;              (bash-mode . bash-ts-mode)
;;              (css-mode . css-ts-mode)
;;              (json-mode . json-ts-mode)
;;              (js-json-mode . json-ts-mode)
;;              (sh-mode . bash-ts-mode)
;;              (sh-base-mode . bash-ts-mode)))
;;     (add-to-list 'major-mode-remap-alist mapping))
;;   :config
;;   (os/setup-install-grammars))



(provide 'init-structural-editing)
;;; init-structural-editing.el ends here
