;;; init-spell-checking.el --- spell-checking -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package flyspell-correct
  :defer t)

(use-package flyspell-correct-popup
  :defer t)

(use-package flyspell-correct-ivy
  :defer t
  :bind ("C-M-;" . flyspell-correct-wrapper)
  :config
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  (setq ispell-program-name "aspell"))

(use-package flyspell
  :diminish
  :defer t
  :delight
  :hook ((markdown-mode org-mode text-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom
  (flyspell-abbrev-p t)
  (flyspell-default-dictionary "en_GB")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil))

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
(add-hook 'text-mode-hook 'flyspell-mode)
(global-set-key (kbd "s-\\") 'ispell-word)

(use-package define-word
  :defer t
  :config
  (global-set-key (kbd "M-\\") 'define-word-at-point))

(provide 'init-spell-checking)
;;; init-spell-checking.el ends here
