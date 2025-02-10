;;; init-search-n-lookup.el --- Searching & Lookup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; engine-mode
(use-package engine-mode
  :defer t
  :config
  (engine-mode t))

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "g")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

(defengine youtube
  "https://www.youtube.com/results?search_query=%s")

;;; synosaurus
(use-package synosaurus
  :defer t
  :config
  (global-set-key (kbd "s-|") 'powerthesaurus-lookup-word-dwim))

;;; wordnut
(use-package wordnut
  :defer t)

;;; Helpers
(defun +kd/search-project-for-symbol-at-point ()
  "Bring up a `counsel-dash' search interface with symbol at point."
  (interactive)
  (counsel-rg
   (substring-no-properties (or (thing-at-point 'symbol) ""))))

;;; Bindings
(kd/leader-key-def
  "sb" #'swiper
  ;; "sd" #'+kd/search-cwd
  ;; "sD" #'+kd/search-other-cwd
  ;; "sf" #'+lookup/file
  "si" #'imenu
  "sl" #'link-hint-open-link
  "sL" #'ffap-menu
  "sm" #'bookmark-jump
  "so" #'counsel-search
  ;; "sO" #'+lookup/online-select
  "sd" #'counsel-dash
  "sD" #'counsel-dash-at-point
  "sp" #'counsel-projectile-rg
  "sP" #'+kd/search-project-for-symbol-at-point
  "s." #'+kd/search-project-for-symbol-at-point
  ;; "sP" #'+kd/search-other-project
  "ss" #'swiper-isearch
  "sS" #'swiper-isearch-thing-at-point
  "sw" #'wordnut-search
  "sW" #'synosaurus-choose-and-replace
  )

(provide 'init-search-n-lookup)
;;; init-search-n-lookup.el ends here

