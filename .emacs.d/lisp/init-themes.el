;;; init-themes.el --- themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'catppuccin-theme)

(use-package catppuccin-theme
  :ensure t
  :init
  ;; Start with a default flavor
  (setq catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin t))

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((catppuccin) (catppuccin)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript nil)
  (auto-dark-allow-powershell nil)
  :hook
  (auto-dark-dark-mode
   . (lambda ()
       (setq catppuccin-flavor 'mocha)
       (catppuccin-reload)
       (message "🌙 Catppuccin Mocha enabled")
        ))
  (auto-dark-light-mode
   . (lambda ()
       (setq catppuccin-flavor 'latte)
       (catppuccin-reload)
       (message "🌞 Catppuccin Latte enabled")
        ))
  :init (auto-dark-mode))

(provide 'init-themes)
;;; init-themes.el ends here
