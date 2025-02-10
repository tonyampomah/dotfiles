;;; init-reading.el --- reading -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family kd/variable-pitch-font
			   :height 1.4))
;; Reading EPUB
(use-package nov
  :defer t)

(add-hook 'nov-mode-hook 'my-nov-font-setup)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'writeroom-mode)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(use-package dash-docs
  :defer t
  :config
  (setq dash-docs-enable-debugging nil))

(straight-use-package
 '(openapi-preview :type git :host github :repo "merrickluo/openapi-preview"))

;; Reading PDFs
(use-package pdf-tools
  :defer t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-use-scaling t
		pdf-view-use-imagemagick nil)
  :config (pdf-tools-install))


(general-def 'normal 'nov-mode-map
  "<" 'nov-history-back
  ">" 'nov-history-forward)


(provide 'init-reading)
;;; init-reading.el ends here
