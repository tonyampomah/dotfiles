;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;
;;

(setq initial-buffer-choice "*scratch*")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Tony Ampomah"
      user-mail-address "tony@arksolutions.it")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Cascadia Code" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/gtd/")
;; (setq org-agenda-files
;;       '("~/gtd/inbox.org"
;; 	"~/gtd/tasks.org"
;; 	"~/gtd/calendar"))


;;
;;; Keybinds
(map! :n [tab] (cmds! (and (featurep! :editor fold)
                           (save-excursion (end-of-line) (invisible-p (point))))
                      #'+fold/toggle
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)
      :v [tab] (cmds! (and (bound-and-true-p yas-minor-mode)
                           (or (eq evil-visual-selection 'line)
                               (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                      #'yas-insert-snippet
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)

      :g "s-p" #'projectile-find-file
      :g "s-P" #'projectile-switch-project
      :g "s-k" #'kill-buffer
      :g "s-s" #'save-buffer


      ;; Easier window movement
      :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right


      (:after dired
       :map dired-mode-map
       :n "q" #'quit-window
       :n "v" #'evil-visual-char
       :nv "j" #'dired-next-line
       :nv "k" #'dired-previous-line
       :n "h" #'dired-up-directory
       :n "l" #'dired-find-file
       :n "#" #'dired-flag-auto-save-files
       :n "." #'evil-repeat
       :n "~" #'dired-flag-backup-files
       ;; Comparison commands
       :n "=" #'dired-diff
       :n "|" #'dired-compare-directories
       ;; move to marked files
       :m "[m" #'dired-prev-marked-file
       :m "]m" #'dired-next-marked-file
       :m "[d" #'dired-prev-dirline
       :m "]d" #'dired-next-dirline
       ;; Lower keys for commands not operating on all the marked files
       :desc "wdired" :n "w" #'wdired-change-to-wdired-mode
       :n "a" #'dired-find-alternate-file
       :nv "d" #'dired-flag-file-deletion
       :n "K" #'dired-do-kill-lines
       :n "r" #'dired-do-redisplay
       :nv "m" #'dired-mark
       :nv "t" #'dired-toggle-marks
       :nv "u" #'dired-unmark           ; also "*u"
       :nv "p" #'dired-unmark-backward
       ;; :n "W" #'browse-url-of-dired-file
       :n "x" #'dired-do-flagged-delete
       :n "y" #'dired-copy-filename-as-kill
       :n "Y" (cmd! (dired-copy-filename-as-kill 0))
       :n "+" #'dired-create-directory
       :n "O" #'dired-open-mac
       :n "o" #'dired-preview-mac
       ;; hiding
       :n "<tab>" #'dired-hide-subdir ;; FIXME: This can probably live on a better binding.
       :n "<backtab>" #'dired-hide-all
       :n "$" #'dired-hide-details-mode
       ;; misc
       :n "U" #'dired-undo
       ;; subtree
       )

      :leader
      "f t" #'counsel-tramp
      "f vs" #'+tonyampomah/visit-ssh-config
      "f vh" #'+tonyampomah/visit-host-file
      "f vr" #'+tonyampomah/visit-resolv-config
      "f ve" #'+tonyampomah/visit-emacs-config
      "f vd" #'+tonyampomah/visit-dwm-config

      "o p" #'pass
      "o d" #'docker

      "b e" #'eval-buffer
      )

(map! :leader
      "x" #'execute-extended-command)


(after! projectile
  ;; Where projectile stores cache/index data
  (setq projectile-cache-file
        (expand-file-name "projectile.cache" doom-cache-dir)
        projectile-known-projects-file
        (expand-file-name "projectile.projects" doom-cache-dir))

  ;; Enable caching for faster project switching
  (setq projectile-enable-caching t)

  ;; Search paths for auto-discovering projects
  (setq projectile-project-search-path
        '(
          "~/Documents/Joint/2Areas"
          "~/Documents/Joint/1Projects"
          "~/Documents/Personal/2Areas"
          "~/Documents/Personal/1Projects"
          "~/Documents/2Areas"
          "~/Documents/1Projects"
          "~/RadiusRepo/3Resources"
          "~/RadiusRepo/2Areas"
          "~/Repo/3Resources/"
          "~/Repo/2Areas/"
          ))

  )



;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Ignore directories
;; (add-to-list 'projectile-globally-ignored-directories "node_modules")
;; (add-to-list 'projectile-globally-ignored-directories ".git"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `with-eval-after-load' block, otherwise Doom's defaults may override your
;; settings. E.g.
;;
;;   (with-eval-after-load 'PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look them up).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
