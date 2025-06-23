;;; init-org-mode.el --- org-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq-default fill-column 80)

;; Task Management & Agenda Views
;; (setq org-directory "~/org")

(setq org-agenda-files '(
			 "~/org"
			 "~/org-work"
			 ))

;; (defun kd/pretty-org-agenda ()
;;   (variable-pitch-mode 1)
;;   )

;; (add-hook 'org-agenda-mode-hook 'kd/pretty-org-agenda)
;; Turn on indentation and auto-fill mode for Org files
(defun kd/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 1)
  (visual-line-mode 1)
  (company-mode 0)
  (setq evil-auto-indent nil))

(use-package org
  ;; :defer t
  :straight (:type built-in)
  ;; :straight org-plus-contrib
  :hook (org-mode . kd/org-mode-setup)
  :config
  (setq org-ellipsis " ⤵"
	org-hide-emphasis-markers t
	org-agenda-archives-mode t
	org-src-fontify-natively t
	org-agenda-start-with-log-mode t
	org-log-done 'time
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

  ;; (setq org-outline-path-complete-in-steps nil)
  ;; (setq org-refile-use-outline-path t)
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
     (mermaid . t)
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
	(:calendar-id "personal" :inbox "~/org/calendar/pcal.org")
	(:calendar-id "spiritual" :inbox "~/org/calendar/scal.org")
	(:calendar-id "events" :inbox "~/org/calendar/ecal.org")
	(:calendar-id "tony-tayo" :inbox "~/org/calendar/jcal.org")
	))

(use-package org-caldav
  :after org
  :defer t)

(use-package ox-twbs
  :defer t
  :after org
  :init (add-to-list 'org-export-backends 'twbs))

;; (use-package org-jira
;;   :defer t
;;   :hook (org-mode . org-jira-mode)
;;   :after org
;;   :config
;;   (setq jiralib-url ""))

(use-package ob-php
  :after org
  :defer t)

(use-package ox-pandoc
  :after org
  :defer t
  :init (add-to-list 'org-export-backends 'pandoc))

;; (use-package ox-jira
;;   :after org
;;   :defer t
;;   :init (add-to-list 'org-export-backends 'jira))

(use-package ox-slack
  :after org
  :defer t
  :init (add-to-list 'org-export-backends 'slack))

(straight-use-package
 '(ox-tailwind :type git :host github :repo "vascoferreira25/ox-tailwind"))

(use-package ox-slimhtml
  :after org
  :defer t
  :init (add-to-list 'org-export-backends 'slimhtml))

(use-package ob-async
  :after org
  :defer t)

(use-package ob-php
  :defer t)

(use-package ob-restclient
  :after org
  :defer t)

(use-package ob-async
  :after org
  :defer t)

(use-package ob-mermaid
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

(setq org-todo-keywords
      '((sequence
	 "TODO(t)"  ; A task that needs doing & is ready to do
	 "PROJ(p)"  ; A project, which usually contains other tasks
	 "STRT(s)"  ; A task that is in progress
	 "WAIT(w)"  ; Something external is holding up this task
	 "HOLD(h)"  ; This task is paused/on hold because of me
	 "|"
	 "DONE(d)"  ; Task successfully completed
	 "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
	(sequence
	 "[ ](T)"   ; A task that needs doing
	 "[-](S)"   ; Task is in progress
	 "[?](W)"   ; Task is being held up or paused
	 "|"
	 "[X](D)")) ; Task was completed
      org-todo-keyword-faces
      '(("[-]"  . org-todo-active)
	("STRT" . (:foreground "orange" :weight bold))
	("[?]"  . (:foreground "red" :weight bold))
	("WAIT" . (:foreground "red" :weight bold))
	("HOLD" . (:foreground "red" :weight bold))
	("PROJ" . org-todo-project)))

(setq org-capture-templates
      '(
	("i" "Inbox" entry (file "~/org/inbox.org")
	 "* TODO %?\n %i\n")
	("j" "jw")
	("jb" "Bible Reading" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/bible-reading.org"))
	("jm" "Microphone" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/microphone.org"))
	("jr" "Return Visit" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/return-visit.org"))
	("jc" "Cong. Bible Study Reader" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/cong-bible-study-reader.org"))
	("jt" "Talk" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/talk.org"))
	("jw" "Watchtower" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/watchtower-reader.org"))
	("jk" "Kingdom Cleaning" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/cleaning.org"))
	("ji" "Initial Call" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/initial-call.org"))
	("ja" "Audio Visual")
	("jam" "Audio Visual Main" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/audio-visual.org"))
	("jaa" "Audio Visual Assistant" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/audio-visual-assistant.org"))
	("jab" "Broadcast" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/broadcast.org"))
	("jp" "Prayer/Platform")
	("jpo" "Opening Prayer" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/opening-prayer.org"))
	("jpc" "Closing Prayer" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/closing-prayer.org"))
	("jpp" "Platform" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/platform.org"))
	("p" "Project" entry (file+headline "~/org/todo.org" "1Projects")
	 (file "~/org/templates/project.org"))
	("e" "Epic Ticket" entry (file+headline "~/org/work/jira.org" "Tickets")
	 (file "~/org/templates/epic.org"))
	("s" "Story Ticket" entry (file+headline "~/org/work/jira.org" "Tickets")
	 (file "~/org/templates/story.org"))
	("b" "Bug Ticket" entry (file+headline "~/org/work/jira.org" "Tickets")
	 (file "~/org/templates/bug.org"))
	("m" "Mail" entry (file+headline "~/org/inbox.org" "")
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

;; Organisation (org and PARA)
;; A project is “any outcome that will
;; take more than one action step to complete.” As a result of
;; implementing Tiago Forte’s “PARA” system, I can ensure that I
;; always have an up to date project list.
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/Notes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-graph)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n j" . org-roam-dailies-capture-today)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package ox-reveal)

;; Projects
(defun go-to-projects ()
  (interactive)
  (find-file "~/org/todo.org")
  (widen)
  (beginning-of-buffer)
  (re-search-forward "* Projects")
  (beginning-of-line))

(defun project-overview ()
  (interactive)
  (go-to-projects)
  (org-narrow-to-subtree)
  (org-sort-entries t ?p)
  (org-columns))

(defun project-deadline-overview ()
  (interactive)
  (go-to-projects)
  (org-narrow-to-subtree)
  (org-sort-entries t ?d)
  (org-columns))

(defun my-org-agenda-list-stuck-projects ()
  (interactive)
  (go-to-projects)
  (org-agenda nil "#" 'subtree))

;; Areas
(defun go-to-areas ()
  (interactive)
  (find-file "~/org/todo.org")
  (widen)
  (beginning-of-buffer)
  (re-search-forward "* Areas")
  (beginning-of-line))

(defun areas-overview ()
  (interactive)
  (go-to-areas)
  (org-narrow-to-subtree)
  (org-columns))

;; Reviews
(defun my-new-daily-review ()
  (interactive)
  (let ((org-capture-templates '(("d" "Review: Daily Review" entry (file+olp+datetree "/tmp/reviews.org")
				  (file "~/org/reviews/daily-review-template.org")))))
    (progn
      (org-capture nil "d")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (fetch-calendar)
      (org-clock-in))))

(defun my-new-weekly-review ()
  (interactive)
  (let ((org-capture-templates '(("w" "Review: Weekly Review" entry (file+olp+datetree "/tmp/reviews.org")
				  (file "~/org/reviews/weekly-review-template.org")))))
    (progn
      (org-capture nil "w")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (fetch-calendar)
      (org-clock-in))))

(defun my-new-monthly-review ()
  (interactive)
  (let ((org-capture-templates '(("m" "Review: Monthly Review" entry (file+olp+datetree "/tmp/reviews.org")
				  (file "~/org/reviews/monthly-review-template.org")))))
    (progn
      (org-capture nil "m")
      (org-capture-finalize t)
      (org-speed-move-safe 'outline-up-heading)
      (org-narrow-to-subtree)
      (fetch-calendar)
      (org-clock-in))))

(bind-keys :prefix-map review-map
	   :prefix "C-c r"
	   ("d" . my-new-daily-review)
	   ("w" . my-new-weekly-review)
	   ("m" . my-new-monthly-review))

(defun kd/day-view ()
  (interactive)
  (progn (org-agenda nil "a")
	 (org-agenda-day-view)))

(defun kd/week-view ()
  (interactive)
  (progn (org-agenda nil "a")
	 (org-agenda-week-view)))

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

(setq org-agenda-custom-commands
      `(("A" "Daily agenda and top priority tasks"
         ((tags-todo "*"
                     ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                      (org-agenda-skip-function
                       `(org-agenda-skip-entry-if
                         'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                      (org-agenda-block-separator nil)
                      (org-agenda-overriding-header "Important tasks without a date\n")))
          (agenda "" ((org-agenda-span 1)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-scheduled-past-days 0)
                      ;; We don't need the `org-agenda-date-today'
                      ;; highlight because that only has a practical
                      ;; utility in multi-day views.
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %-e %B %Y")
                      (org-agenda-overriding-header "\nToday's agenda\n")))
          (agenda "" ((org-agenda-start-on-weekday 1)
                      (org-agenda-start-day "+1d")
                      (org-agenda-span 3)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nNext three days\n")))
          (agenda "" ((org-agenda-time-grid nil)
                      (org-agenda-start-on-weekday 1)
                      ;; We don't want to replicate the previous section's
                      ;; three days, so we start counting from the day after.
                      (org-agenda-start-day "+4d")
                      (org-agenda-span 14)
                      (org-agenda-show-all-dates nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-agenda-entry-types '(:deadline))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))))
        ("P" "Plain text daily agenda and top priorities"
         ((tags-todo "*"
                     ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                      (org-agenda-skip-function
                       `(org-agenda-skip-entry-if
                         'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                      (org-agenda-block-separator nil)
                      (org-agenda-overriding-header "Important tasks without a date\n")))
          (agenda "" ((org-agenda-span 1)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-scheduled-past-days 0)
                      ;; We don't need the `org-agenda-date-today'
                      ;; highlight because that only has a practical
                      ;; utility in multi-day views.
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %-e %B %Y")
                      (org-agenda-overriding-header "\nToday's agenda\n")))
          (agenda "" ((org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+1d")
                      (org-agenda-span 3)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nNext three days\n")))
          (agenda "" ((org-agenda-time-grid nil)
                      (org-agenda-start-on-weekday nil)
                      ;; We don't want to replicate the previous section's
                      ;; three days, so we start counting from the day after.
                      (org-agenda-start-day "+4d")
                      (org-agenda-span 14)
                      (org-agenda-show-all-dates nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-agenda-entry-types '(:deadline))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n"))))
         ((org-agenda-with-colors nil)
          (org-agenda-prefix-format "%t %s")
          (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
          (org-agenda-fontify-priorities nil)
          (org-agenda-remove-tags t))
         ("agenda.txt"))))

;; And this is what I actually use.  The `defvar' is stored in my
;; kwamedat-org.el file.  In the video I explain why I use this style.

(defvar kwamedat-org-custom-daily-agenda
  ;; NOTE 2021-12-08: Specifying a match like the following does not
  ;; work.
  ;;
  ;; tags-todo "+PRIORITY=\"A\""
  ;;
  ;; So we match everything and then skip entries with
  ;; `org-agenda-skip-function'.
  `((tags-todo "*"
               ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                (org-agenda-skip-function
                 `(org-agenda-skip-entry-if
                   'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                (org-agenda-block-separator nil)
                (org-agenda-overriding-header "Important tasks without a date\n")))
    (agenda "" ((org-agenda-span 1)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 0)
                ;; We don't need the `org-agenda-date-today'
                ;; highlight because that only has a practical
                ;; utility in multi-day views.
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")
                (org-agenda-overriding-header "\nToday's agenda\n")))
    (agenda "" ((org-agenda-start-on-weekday nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 3)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nNext three days\n")))
    (agenda "" ((org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                ;; We don't want to replicate the previous section's
                ;; three days, so we start counting from the day after.
                (org-agenda-start-day "+4d")
                (org-agenda-span 14)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:deadline))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n"))))
  "Custom agenda for use in `org-agenda-custom-commands'.")

(setq org-agenda-custom-commands
      `(("A" "Daily agenda and top priority tasks"
         ,kwamedat-org-custom-daily-agenda)
        ("P" "Plain text daily agenda and top priorities"
         ,kwamedat-org-custom-daily-agenda
         ((org-agenda-with-colors nil)
          (org-agenda-prefix-format "%t %s")
          (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
          (org-agenda-fontify-priorities nil)
          (org-agenda-remove-tags t))
         ("agenda.txt"))))

;; Bindings
(kd/leader-key-def
      ;;; <leader> n --- notes
  "nf" '(lambda() (interactive) (org-roam-node-find))
  "ng" '(lambda() (interactive) (org-roam-graph))
  "ni" '(lambda() (interactive) (find-file "~/org/inbox.org"))
  "nl" '(lambda() (interactive) (org-roam-buffer-toggle))
  "nc" '(lambda() (interactive) (org-roam-capture))
  "nn" '(lambda() (interactive) (org-capture))
  "nt" '(lambda() (interactive) (org-roam-dailies-capture-today))
  "np" '(lambda() (interactive) (find-file "~/org/todo.org"))
  "nr" '(lambda() (interactive) (find-file "~/org/routine.org"))
  "na" 'org-agenda
  "nd" 'kd/week-view)

(kd/my-local-leader-def 'normal org-mode-map
  "c" 'org-ctrl-c-ctrl-c
  "o" 'org-open-at-point
  "t" 'org-todo
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


(provide 'init-org-mode)
;;; init-org-mode.el ends here
