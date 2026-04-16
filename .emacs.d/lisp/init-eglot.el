;;; init-eglot.el --- eglot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; Use built-in Eglot (Emacs 29+)
(require 'eglot)

;; Start Eglot automatically for programming modes
(add-hook 'prog-mode-hook 'eglot-ensure)


(setq eglot-ignored-server-capabilities
      '(:codeLensProvider))

;; Performance tuning
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(setq gc-cons-threshold 100000000)

;; Reduce noisy updates
(setq eglot-sync-connect nil)
(setq eglot-autoshutdown t)
(setq eglot-events-buffer-config 0)

;; Optional: less UI interruption
(setq eglot-extend-to-xref t)

;; php
(add-to-list 'eglot-server-programs
             '(php-mode . ("intelephense" "--stdio")))

;; JS / TS / React / Next.js
(add-to-list 'eglot-server-programs
             '((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode)
               . ("typescript-language-server" "--stdio")))

;; Python / Ansible
(add-to-list 'eglot-server-programs
             '(python-mode . ("pyright-langserver" "--stdio")))

;; Ansible
(add-to-list 'eglot-server-programs
             '(yaml-mode . ("ansible-language-server" "--stdio")))

;; CSS
(add-to-list 'eglot-server-programs
             '(css-mode . ("vscode-css-language-server" "--stdio")))
(add-to-list 'eglot-server-programs
             '(html-mode . ("tailwindcss-language-server" "--stdio")))



(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(robot-mode . ("robotframework-lsp"))))

(add-hook 'robot-mode-hook
          (lambda ()
            (setq-local eglot-ignored-server-capabilities
                        '(:codeLensProvider))))

(setq eglot-send-changes-idle-time 0.5) ;; delay updates

(dolist (pair '((js-mode . js-ts-mode)
                (typescript-mode . typescript-ts-mode)
                (python-mode . python-ts-mode)
                (css-mode . css-ts-mode)))
  (add-to-list 'major-mode-remap-alist pair))

(provide 'init-eglot)
;;; init-eglot.el ends here
