;;; init-themes.el --- themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (straight-use-package 'catppuccin-theme)

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

;; (straight-use-package 'doom-themes)

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Load the default theme
;;   (load-theme 'doom-tomorrow-night t))

;; (use-package auto-dark
;;   :ensure t
;;   :custom
;;   ;; Use same theme for both, or swap if you want a light one later
;;   (auto-dark-themes '((doom-tomorrow-night) (doom-tomorrow-night)))
;;   (auto-dark-polling-interval-seconds 5)
;;   (auto-dark-allow-osascript nil)
;;   (auto-dark-allow-powershell nil)
;;   :hook
;;   (auto-dark-dark-mode
;;    . (lambda ()
;;        (load-theme 'doom-tomorrow-night t)
;;        (message "🌙 doom-tomorrow-night enabled")))
;;   (auto-dark-light-mode
;;    . (lambda ()
;;        ;; optional: switch to a light doom theme
;;        ;; (load-theme 'doom-one-light t)
;;        (load-theme 'doom-tomorrow-day t)
;;        (message "🌞 doom-tomorrow-night (light mode fallback) enabled")))
;;   :init (auto-dark-mode))

(provide 'init-themes)
;;; init-themes.el ends here
