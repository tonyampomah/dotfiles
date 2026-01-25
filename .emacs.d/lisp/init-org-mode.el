;;; init-org-mode.el --- org-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq-default fill-column 80)

;; Turn on indentation and auto-fill mode for Org files
(defun kd/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 1)
  (visual-line-mode 1)
  (company-mode 0)
  (setq evil-auto-indent nil))

(defun kd/day-view ()
  (interactive)
  (progn (org-agenda nil "a")
	 (org-agenda-day-view)))

(defun kd/week-view ()
  (interactive)
  (progn (org-agenda nil "a")
	 (org-agenda-week-view)))

(defun kd/month-view ()
  (interactive)
  (progn (org-agenda nil "a")
	 (org-agenda-month-view)))

(use-package org
  :straight (:type built-in)
  :hook (org-mode . kd/org-mode-setup)
  :config
  (setq org-ellipsis " ⤵"
	org-hide-emphasis-markers t
	org-agenda-archives-mode t
	org-src-fontify-natively t
	org-agenda-start-with-log-mode t
	;; org-log-done 'time
	org-log-into-drawer t
	org-src-tab-acts-natively t
	org-edit-src-content-indentation 2
	org-hide-block-startup nil
	org-src-preserve-indentation nil
	org-agenda-skip-scheduled-if-done t
	org-startup-folded t
	org-cycle-separator-lines 2)

  (setq org-modules
	'(org-crypt
	  org-habit
	  org-bookmark
	  org-eshell
	  org-irc))

  (setq org-refile-targets '((nil :maxlevel . 3)
			     (org-agenda-files :maxlevel . 3)))

  (setq org-habit-graph-column 60)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (shell . t)
     (ledger . t)
     (python . t)
     (sql . t)
     (ruby . t)
     (php . t)
     (scheme . t)
     (sqlite . t)
     ))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(use-package org-contrib :after org)

(use-package org-pomodoro
  :defer t
  :after org
  :commands (org-mode org-pomodoro-mode)
  :config
  (setq org-pomodoro-long-break-length 60)
  (setq org-pomodoro-long-break-frequency 10))

(setq org-caldav-url "https://nextcloud.theampomahs.com/remote.php/caldav/calendars/kwamedat")
(setq org-caldav-calendars
      '(
	(:calendar-id "personal" :inbox "~/gtd/calendar/pcal.org")
	(:calendar-id "spiritual" :inbox "~/gtd/calendar/scal.org")
	(:calendar-id "events" :inbox "~/gtd/calendar/ecal.org")
	(:calendar-id "tony-tayo" :inbox "~/gtd/calendar/jcal.org")
	))

(use-package org-caldav
  :after org
  :defer t)

(use-package ox-twbs
  :defer t
  :after org
  :init (add-to-list 'org-export-backends 'twbs))

;; (use-package ob-php
;;   :after org
;;   :defer t)

(use-package ox-pandoc
  :after org
  :defer t
  :init (add-to-list 'org-export-backends 'pandoc))

(use-package ob-async
  :after org
  :defer t)

(use-package ob-restclient
  :after org
  :defer t)

(use-package ob-async
  :after org
  :defer t)

(use-package ob-tmux
  ;; Install package automatically (optional)
  :ensure t
  :custom
  (org-babel-default-header-args:tmux
   '((:results . "silent")	;
     (:session . "default")	; The default tmux session to send code to
     (:socket  . nil)))		; The default tmux socket to communicate with
  ;; The tmux sessions are prefixed with the following string.
  ;; You can customize this if you like.
  (org-babel-tmux-session-prefix "ob-")
  ;; The terminal that will be used.
  ;; You can also customize the options passed to the terminal.
  ;; The default terminal is "gnome-terminal" with options "--".
  (org-babel-tmux-terminal "xterm")
  (org-babel-tmux-terminal-opts '("-T" "ob-tmux" "-e"))
  ;; Finally, if your tmux is not in your $PATH for whatever reason, you
  ;; may set the path to the tmux binary as follows:
  (org-babel-tmux-location "/usr/bin/tmux"))

(use-package evil-org
  ;; :defer t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Fix evil org issue
(fset 'evil-redirect-digit-argument 'ignore) ;;# before evil-org loaded

(evil-define-key 'motion 'evil-org-mode
  (kbd "0") 'evil-org-beginning-of-line)

;; When editing a code snippet, use the current window rather than
;; popping open a new one (which shows the same information).
(setq org-src-window-setup 'current-window)

;; Don't indent newly expanded blocks, even if they're under a heading.
(setq org-adapt-indentation nil)

;; Automatically put quick capture into insert mode
(add-hook 'org-capture-mode-hook 'evil-insert-state)

;; Refile targets configuration 
(setq org-refile-targets
      '((nil :maxlevel . 3)
	(org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

(setq org-capture-templates
      '(
	("i" "Inbox" entry (file "~/gtd/inbox.org")
	 "* TODO %?\n %i\n")
	("j" "jw")
	("jb" "Bible Reading" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/bible-reading.org"))
	("jm" "Microphone" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/microphone.org"))
	("jr" "Return Visit" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/return-visit.org"))
	("jc" "Cong. Bible Study Reader" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/cong-bible-study-reader.org"))
	("jt" "Talk" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/talk.org"))
	("jw" "Watchtower Reader" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/watchtower-reader.org"))
	("jk" "Kingdom Cleaning" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/cleaning.org"))
	("ji" "Initial Call" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/initial-call.org"))
	("ja" "Audio Visual")
	("jam" "Audio Visual Main" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/audio-visual.org"))
	("jaa" "Audio Visual Assistant" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/audio-visual-assistant.org"))
	("jab" "Broadcast" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/broadcast.org"))
	("jp" "Prayer/Platform")
	("jpo" "Opening Prayer" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/opening-prayer.org"))
	("jpc" "Closing Prayer" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/closing-prayer.org"))
	("jpp" "Platform" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/platform.org"))
	("p" "Project" entry (file "~/gtd/inbox.org")
	 (file "~/gtd/templates/project.org"))
	("w" "work")
	("wi" "Inbox" entry (file "~/gtd/work-inbox.org")
	 "* TODO %?\n %i\n")
	("wo" "On boarding" entry (file+headline "~/org-work/work-todo.org" "Projects")
	 (file "~/org-work/templates/onboarding.org"))
	("wp" "Work Project" entry (file+headline "~/org-work/work-todo.org" "Projects")
	 (file "~/org-work/templates/project.org"))
	("we" "Epic Ticket" entry (file+headline "~/org-work/work-inbox.org" "Tickets")
	 (file "~/org-work/templates/epic.org"))
	("ws" "Story Ticket" entry (file+headline "~/org-work/work-inbox.org" "Tickets")
	 (file "~/org-work/templates/story.org"))
	("wb" "Bug Ticket" entry (file+headline "~/org-work/work-inbox.org" "Tickets")
	 (file "~/org-work/templates/bug.org"))
	("m" "Mail" entry (file+headline "~/gtd/inbox.org" "")
	 "* TODO [#A] Process Email %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n:PROPERTIES:\n:CREATED: %U\n:END:\n %a" :immediate-finish t :prepend t)
	))

;; I prefer indented in org mode please.
(setq org-startup-indented t)

;; Stop asking to confirm
(setq org-confirm-babel-evaluate nil)

;; Block Templates
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

;; Fonts & Bullets
;; I like to see an outline of pretty bullets instead of a list of asterisks.
(use-package org-bullets
  :defer t
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; Generate Table of Contents Dynamically
(use-package toc-org
  :after org
  :init (add-hook 'org-mode-hook #'toc-org-enable))

;; Increase the size of various headings
(set-face-attribute 'org-document-title nil :font kd/variable-pitch-font :weight 'bold :height 1.3)
(dolist (face '((org-level-1 . 1.2)
		(org-level-2 . 1.1)
		(org-level-3 . 1.05)
		(org-level-4 . 1.0)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font kd/variable-pitch-font :weight 'medium :height (cdr face))
  )

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org-roam/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n j" . org-roam-dailies-capture-today)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package ox-reveal)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
		 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package org-gtd
  :ensure t
  :after org
  :demand t
  :init
  (setq org-gtd-update-ack "4.0.0")

  :custom

  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "CNCL(c)")))

  :config
  (org-edna-mode 1)
  (setq org-agenda-files (list org-gtd-directory))

  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   ("C-c d n" . org-gtd-show-all-next)
   ("C-c d s" . org-gtd-reflect-stuck-projects)

   ;; Keybinding for organizing items (only works in clarify buffers)
   :map org-gtd-clarify-mode-map
   ("C-c c" . org-gtd-organize)

   ;; Quick actions on tasks in agenda views (optional but recommended)
   :map org-agenda-mode-map
   ("C-c ." . org-gtd-agenda-transient)))


(setq org-gtd-organize-hooks nil)

(defun org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))

(defun org-capture-mail ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "@"))

;; Use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)


;; Only if you use mu4e
;; (require 'mu4e)
;; (define-key mu4e-headers-mode-map (kbd "C-c i") 'org-capture-mail)
;; (define-key mu4e-view-mode-map    (kbd "C-c i") 'org-capture-mail)

;; Refile
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))


(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

;; Agenda
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))
	  ))))


;; Bindings
(define-key global-map            (kbd "C-c a") 'org-agenda)
(define-key global-map            (kbd "C-c c") 'org-capture)
(define-key global-map            (kbd "C-c i") 'org-capture-inbox)

(kd/leader-key-def
      ;;; <leader> n --- notes
  "nf" '(lambda() (interactive) (org-roam-node-find))
  "ng" '(lambda() (interactive) (org-roam-graph))
  "ni" '(lambda() (interactive) (find-file "~/gtd/inbox.org"))
  "nn" '(lambda() (interactive) (org-capture))
  "nt" '(lambda() (interactive) (org-roam-dailies-capture-today))
  "np" 'org-gtd-process-inbox
  "ne" 'org-gtd-engage
  "na" 'org-agenda
  "nd" 'kd/day-view
  "nw" 'kd/week-view
  "nm" 'kd/month-view)

(kd/my-local-leader-def 'normal org-mode-map
  "c" 'org-ctrl-c-ctrl-c
  "o" 'org-open-at-point
  "t" 'org-todo
  "o" 'org-gtd-organize
  "s" 'org-schedule
  "d" 'org-deadline
  "a" 'org-agenda
  "e" 'org-export-dispatch
  "n" 'org-narrow-to-element
  "w" 'widen
  "q" 'org-set-tags-command
  "r" 'org-refile)

(kd/my-local-leader-def 'normal org-agenda-mode-map
  :prefix "C-c"
  "vd" 'org-agenda-week-view
  "vw" 'org-agenda-week-view
  "vm" 'org-agenda-month-view
  "vy" 'org-agenda-year-view)

(general-define-key
 :states '(normal insert)
 :keymaps 'org-mode-map
 "s-<up>"   #'org-metaup
 "s-<down>" #'org-metadown
 "s-<left>"  #'org-metaleft
 "s-<right>" #'org-metaright
 "s-k"   #'org-metaup
 "s-j" #'org-metadown
 "s-h"  #'org-metaleft
 "s-l" #'org-metaright)


(provide 'init-org-mode)
;;; init-org-mode.el ends here
