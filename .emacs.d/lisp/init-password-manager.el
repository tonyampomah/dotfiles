;;; init-password-manager.el --- password-manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq auth-sources
;;       '((:source "~/.authinfo.gpg")))

(fset 'epg-wait-for-status 'ignore)

(use-package pass
  :defer t)

(use-package ivy-pass
  :defer t)

(use-package password-store
  :defer t
  :config
  (setq password-store-password-length 12))

(use-package lastpass
  :defer t
  :config
  (setq lastpass-trust-login t)
  ;; Enable lastpass custom auth-source
  (lastpass-auth-source-enable))

(use-package auth-source-pass
  :defer t
  :config
  (auth-source-pass-enable))

(kd/leader-key-def

  "ap" '(:ignore t :which-key "pass")
  "app" 'password-store-copy
  "apc" 'password-store-copy
  "apd" 'password-store-remove
  "apr" 'password-store-rename
  "ape" 'password-store-edit
  "api" 'password-store-insert
  "apg" 'password-store-generate)

(provide 'init-password-manager)
;;; init-password-manager.el ends here
