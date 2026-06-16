;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq initial-buffer-choice "*scratch*")

(setq user-full-name "Tony Ampomah"
      user-mail-address "tony@arksolutions.it")

;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Cascadia Code" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13))

(setq doom-theme 'doom-tomorrow-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq org-gtd-default-file-name "tasks")

(setq org-gtd-files
      '("~/gtd/inbox.org"
        "~/gtd/tasks.org"))


(setq org-gtd-update-ack "4.0.0")

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
      "f vs" #'tonyampomah/visit-ssh-config
      "f vh" #'tonyampomah/visit-host-file

      "o p" #'pass
      "o d" #'docker

      "b e" #'eval-buffer

      "n f" #'org-roam-node-find
      "n e" #'org-gtd-engage
      "n i" #'(lambda() (interactive) (find-file "~/gtd/inbox.org"))
      "n p" #'org-gtd-process-inbox
      "n d" #'tonyampomah/day-view
      "n r" #'tonyampomah/org-weekly-review
      "n w" #'tonyampomah/week-view
      "n m" #'tonyampomah/month-view
      )

(map! :leader
      "x" #'execute-extended-command)

(after! org
  (setq org-directory "~/gtd/")
  (setq org-agenda-files
        '("~/gtd/inbox.org"
          "~/gtd/tasks.org"
          "~/gtd/calendar"))
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
	  ("jz" "Zoom host" entry (file "~/gtd/inbox.org")
	   (file "~/gtd/templates/zoom-host.org"))
	  ("js" "Spiritual Gems" entry (file "~/gtd/inbox.org")
	   (file "~/gtd/templates/spiritual-gems.org"))
	  ("jf" "Field Service Conductor" entry (file "~/gtd/inbox.org")
	   (file "~/gtd/templates/field-service-conductor.org"))
	  ("jr" "Return Visit" entry (file "~/gtd/inbox.org")
	   (file "~/gtd/templates/return-visit.org"))
	  ("jc" "Cong. Bible Study Reader" entry (file "~/gtd/inbox.org")
	   (file "~/gtd/templates/cong-bible-study-reader.org"))
	  ("jt" "Talk" entry (file "~/gtd/inbox.org")
	   (file "~/gtd/templates/talk.org"))
	  ("jd" "Discussion" entry (file "~/gtd/inbox.org")
	   (file "~/gtd/templates/discussion.org"))
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
	  ("p" "personal")
	  ("pp" "Personal Project" entry (file "~/gtd/inbox.org")
	   (file "~/gtd/templates/project.org"))
	  ("pc" "House Cleaning" entry (file "~/gtd/inbox.org")
	   (file "~/gtd/templates/house-cleaning.org"))
	  ("w" "work")
	  ("wi" "Inbox" entry (file "~/gtd/work-inbox.org")
	   "* TODO %?\n %i\n")
	  ("wo" "On boarding" entry (file+headline "~/gtd/work-inbox.org" "Projects")
	   (file "~/org-work/templates/onboarding.org"))
	  ("wp" "Work Project" entry (file+headline "~/gtd/work-inbox.org" "Projects")
	   (file "~/org-work/templates/project.org"))
	  ("we" "Epic Ticket" entry (file+headline "~/gtd/work-inbox.org" "Tickets")
	   (file "~/org-work/templates/epic.org"))
	  ("ws" "Story Ticket" entry (file+headline "~/gtd/work-inbox.org" "Tickets")
	   (file "~/org-work/templates/story.org"))
	  ("wb" "Bug Ticket" entry (file+headline "~/gtd/work-inbox.org" "Tickets")
	   (file "~/org-work/templates/bug.org"))
	  ("m" "Mail" entry (file+headline "~/gtd/inbox.org" "")
	   "* TODO [#A] Process Email %:fromname on %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n:PROPERTIES:\n:CREATED: %U\n:END:\n %a" :immediate-finish t :prepend t)
	  ))

  ;; I prefer indented in org mode please.
  (setq org-startup-indented t)

  ;; Stop asking to confirm
  (setq org-confirm-babel-evaluate nil)

  (setq org-caldav-url "https://nextcloud.theampomahs.com/remote.php/caldav/calendars/kwamedat")
  (setq org-caldav-calendars
        '(
          (:calendar-id "personal"
           :files ("~/gtd/calendar/personal.org")
           :inbox "~/gtd/calendar/inbox.org")
          (:calendar-id "spiritual"
           :files ("~/gtd/calendar/spiritual.org")
           :inbox "~/gtd/calendar/inbox.org")
          (:calendar-id "events"
           :files ("~/gtd/calendar/events.org")
           :inbox "~/gtd/calendar/inbox.org")
          (:calendar-id "tony-tayo"
           :files ("~/gtd/calendar/joint.org")
           :inbox "~/gtd/calendar/inbox.org")
          ))

  (use-package! org-gtd
    :after org
    :config
    (require 'org-edna)
    (org-edna-mode 1)

    (setq org-gtd-update-ack "4.0.0")

    ;; org-gtd files
    (setq org-gtd-default-file-name "tasks")
    (setq org-gtd-files
          '("~/gtd/inbox.org"
            "~/gtd/tasks.org"))

    ))


(after! org-roam
  (setq org-roam-directory
        (file-truename "~/org-roam/"))
  )


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

;; (after! org-gtd
;;   (org-edna-mode 1)
;;   (setq org-gtd-default-file-name "tasks")
;;   (setq org-gtd-files '("~/gtd/inbox.org"
;;                         "~/gtd/tasks.org"))
;;   )

;; Ignore directories
;; (add-to-list 'projectile-globally-ignored-directories "node_modules")
;; (add-to-list 'projectile-globally-ignored-directories ".git"))
