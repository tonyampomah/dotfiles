;;; init-keyboard-bindings.el --- Keyboard Bindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; Core Bindings
(global-set-key [(super a)] 'mark-whole-buffer)
(global-set-key [(super v)] 'yank)
(global-set-key [(super c)] 'kill-ring-save)
(global-set-key [(super s)] 'save-buffer)
(global-set-key [(super l)] 'goto-line)
(global-set-key [(super w)]
		(lambda () (interactive) (delete-window)))
(global-set-key [(super z)] 'undo)
(global-set-key [(super y)] 'redo)
(global-set-key (kbd "s-/") 'comment-line)
(global-set-key (kbd "s-f") 'swiper)
(global-set-key (kbd "M-3") #'(lambda () (interactive) (insert "#")))
(global-set-key (kbd "s-o") (kbd "C-x o"))
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)
(global-set-key (kbd "s-w") (kbd "C-x 0"))
(global-set-key (kbd "s-<return>") 'multi-eshell)
(global-set-key (kbd "s-P") 'projectile-switch-project)
(global-set-key (kbd "s-p") 'projectile-find-file)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "s-k") 'kill-buffer)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "H-/") 'comment-line)
(global-set-key (kbd "H-f") 'swiper)
(global-set-key (kbd "H-o") (kbd "C-x o"))
(global-set-key (kbd "H-w") (kbd "C-x 0"))
(global-set-key (kbd "H-b") 'ivy-switch-buffer)
(global-set-key (kbd "H-P") 'projectile-switch-project)
(global-set-key (kbd "H-q") 'save-buffers-kill-emacs)
(global-set-key (kbd "H-k") 'kill-buffer)
(global-set-key (kbd "H-x") 'kill-region)
(global-set-key (kbd "H-s") 'save-buffer)

;; Vim Emulation inside of Emacs with (Evil Mode)
(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-C-u-scroll t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq evil-search-module 'evil-search)
  :ensure t
  :config
  (evil-mode 1)
  ;; (define-key evil-insert-state-map "jj" 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-commentary
  :diminish
  :demand t
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-collection
  :demand t
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-want-unimpaired-p nil)
  (evil-collection-init))

(use-package evil-surround
  :demand t
  :config
  (global-evil-surround-mode 1))

(use-package undo-fu
  :demand t)

(use-package undo-fu-session
  :demand t)

(use-package link-hint
  :ensure t)

;; Simplify Leader Bindings (general)
(use-package general
  :config
  (setq general-override-states '(insert emacs hybrid normal visual motion operator replace))
  (require 'general)
  (general-evil-setup)

  (general-create-definer kd/leader-key-def
    :states '(normal insert visual emacs hybrid)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer kd/ctrl-c-keys
    :prefix "C-c")

  (general-create-definer kd/my-local-leader-def
    :keymaps 'override
    :prefix ","))


(general-define-key
 :states 'insert 
 "C-x C-l"    'company-whole-lines
 "C-x C-k"    'company-dict-or-keywords
 "C-x C-f"    'company-files
 "C-x C-]"    'company-etags
 "C-x s"      'company-ispell
 "C-x C-s"    'company-yasnippet
 "C-x C-o"    'company-capf
 "C-x C-n"    'company-dabbrev)

(kd/leader-key-def 'normal 'override
  ":"  'eval-expression
  "<SPC>" 'projectile-find-file
  "/"  'swiper
  "x"  'counsel-M-x
  "RET" 'bookmark-jump

  ;;; <leader> b --- buffers
  "b["  #'previous-buffer
  "b]"  #'next-buffer
  "bb"  #'persp-counsel-switch-buffer
  "bB"  #'switch-to-buffer
  "be" #'eval-buffer
  "bc"  #'clone-indirect-buffer
  "bC"  #'clone-indirect-buffer-other-window
  "bk"  #'kill-current-buffer
  "bl"  #'evil-switch-to-windows-last-buffer
  "bi"  #'ibuffer
  "bm"  #'bookmark-set
  "bM"  #'bookmark-delete
  "bn"  #'next-buffer
  "bN"  #'evil-buffer-new
  ;; "bO"   #'kill-other-buffers
  "bp"   #'previous-buffer
  "br"   #'revert-buffer
  "bs"   #'basic-save-buffer
  "bS"   #'evil-write-all
  "bz"   #'bury-buffer

  ;;; <leader> c --- code
  "cc"  'compile

  ;;; <leader> i --- insert
  "ie" 'emoji-cheat-sheet-plus-insert
  "ie" #'emojify-insert-emoji
  "if" #'+default/insert-file-path
  ;; "iF" (cmd!! #'+default/insert-file-path t)
  "is" #'yas-insert-snippet
  "iu" #'counsel-unicode-char

  ;;; <leader> w --- window
  "w"  'evil-window-map
  "wd" (kbd "C-x 0")

   ;;; <leader> h --- help
  "h"  'help-map
  "r" 'counsel-imenu

  ;;; <leader> o --- open application
  "oc" 'calendar
  "om" 'mu4e
  "od"  'docker
  "op"  'pass
  "oe"  'eshell-toggle
  "oE"  'eshell-toggle
  "ot" 'shell-pop
  "oT" 'open-terminal-in-projectile-root
  "o-" 'dired-jump
  "oo" 'dired-open-xdg
  )

(kd/leader-key-def
  "ts"  'treemacs
  "tc"  'company-mode
  "tf"  'flycheck-mode
  "tr"  'display-line-numbers
  "ttl"  'toggle-truncate-lines
  "tvl"  'global-visual-line-mode
  "te"  'global-emojify-mode)

;; Keybinding Panel (which-key)
(setq which-key-idle-delay 0.3)
(setq which-key-side-window-max-height 0.5)
(setq which-key-popup-type 'minibuffer)
(use-package which-key
  :config (which-key-mode)
  (which-key-setup-minibuffer))

;; Enable keychord bind with (use-package-chords)
(use-package use-package-chords
  :disabled
  :config (key-chord-mode 1))

(provide 'init-keyboard-bindings)
;;; init-keyboard-bindings.el ends here
