;;; init-eaf.el --- eaf -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'eaf-file-sender)
;; (require 'eaf-music-player)
;; (require 'eaf-js-video-player)
;; (require 'eaf-camera)
;; (require 'eaf-rss-reader)
;; (require 'eaf-terminal)
;; (require 'eaf-image-viewer)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-file-browser)
;; (require 'eaf-file-manager)
;; (require 'eaf-mindmap)
;; (require 'eaf-video-player)
;; (require 'eaf-org-previewer)
;; (require 'eaf-airshare)
;; (require 'eaf-jupyter)
;; (require 'eaf-git)
;; (require 'eaf-system-monitor)
;; (require 'eaf-markmap)

;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :custom
;; 					; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   ;; (defalias 'browse-web #'eaf-open-browser)
;;   ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   )

;; (use-package eaf
;;   :straight (eaf
;;              :type git
;;              :host github
;;              :repo "emacs-eaf/emacs-application-framework"           
;;              :files ("*.el" "*.py" "core" "app" "*.json")
;;              :includes (eaf-pdf-viewer eaf-browser) ; Straight won't try to search for these packages when we make further use-package invocations for them
;;              :pre-build (("python3" "install-eaf.py" "--install" "pdf-viewer" "browser" "--ignore-sys-deps"))
;;              )
;;   :init (evil-set-initial-state 'eaf-mode 'emacs))

;; (setq eaf-browser-dark-mode "false")
;; (use-package eaf-browser
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t))

;; (use-package eaf-pdf-viewer)
;; (use-package eaf-terminal)
;; (use-package eaf-map)
;; (use-package eaf-markdown-previewer)
;; (use-package eaf-file-browser)
;; (use-package eaf-file-manager)
;; (use-package eaf-mindmap)

;; (use-package eaf-video-player)
;; (use-package eaf-org-previewer)
;; (use-package eaf-git)

;; (require 'eaf-demo)
;; (require 'eaf-file-sender)
;; (require 'eaf-music-player)
;; (require 'eaf-js-video-player)
;; (require 'eaf-camera)
;; (require 'eaf-rss-reader)
;; (require 'eaf-terminal)
;; (require 'eaf-image-viewer)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (require 'eaf-map)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-file-browser)
;; (require 'eaf-file-manager)
;; (require 'eaf-mindmap)
;; (require 'eaf-vue-tailwindcss)
;; (require 'eaf-video-player)
;; (require 'eaf-org-previewer)
;; (require 'eaf-airshare)
;; (require 'eaf-jupyter)
;; (require 'eaf-netease-cloud-music)
;; (require 'eaf-git)
;; (require 'eaf-system-monitor)
;; (require 'eaf-pyqterminal)
;; (require 'eaf-markmap)



(provide 'init-eaf)
;;; init-eaf.el ends here
