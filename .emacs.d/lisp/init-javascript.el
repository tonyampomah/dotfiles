;;; init-javascript.el --- Javascript -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq js-indent-level 2)
(add-hook 'coffee-mode-hook
          (lambda ()
            (yas-minor-mode 1)
            (setq coffee-tab-width 2)))

(use-package json-mode
  :defer t)
(use-package js2-mode
  :defer t)
(use-package coffee-mode
  :defer t)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	  (funcall (cdr my-pair)))))

(use-package prettier-js
  :ensure t)
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))
			     (enable-minor-mode
                              '("\\.tsx?\\'" . prettier-js-mode))))

(provide 'init-javascript)
;;; init-javascript.el ends here
