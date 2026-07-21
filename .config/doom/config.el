;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq initial-buffer-choice "*scratch*")
(setq doom-localleader-key ",")

(setq user-full-name "Tony Ampomah"
      user-mail-address "tony@arksolutions.it")

(add-to-list 'auto-mode-alist '("\\.zshrc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\hosts\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\inventory\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.bash_profile\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.zprofile\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.profile\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.aliases\\'" . conf-mode))

(setq doom-font (font-spec :family "Cascadia Code" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13))


(setq doom-theme 'doom-tomorrow-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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
      :g "s-\\" #'ispell-word


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
       :n "S" #'dired-do-symlink
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
      "n i" #'(lambda() (interactive) (find-file "~/org/gtd/inbox.org"))
      "n p" #'org-gtd-process-inbox
      "n d" #'tonyampomah/day-view
      "n r" #'tonyampomah/org-weekly-review
      "n w" #'tonyampomah/week-view
      "n m" #'tonyampomah/month-view
      )

(map! :leader
      "x" #'execute-extended-command
      "p e" #'projectile-run-eshell
      "p t" #'projectile-run-vterm
      "r" #'consult-imenu)

;; Ignore directories
;; (add-to-list 'projectile-globally-ignored-directories "node_modules")
;; (add-to-list 'projectile-globally-ignored-directories ".git"))

;;; Org Mode ------------------------------------------------------------
(after! org
  (map! :map org-mode-map
        :ni "M-h" #'org-metaleft
        :ni "M-j" #'org-metadown
        :ni "M-k" #'org-metaup
        :ni "M-l" #'org-metaright
        :ni "M-<left>" #'org-metaleft
        :ni "M-<down>" #'org-metadown
        :ni "M-<up>" #'org-metaup
        :ni "M-<right>" #'org-metaright
        )

  (map! :map org-mode-map
        :localleader
        :n
        "c" '("Ctrl-C Ctrl-C" . org-ctrl-c-ctrl-c)
        "o" '("Open at point" . org-open-at-point)
        "g" '("GTD organize" . org-gtd-organize)
        "t" '("Todo" . org-todo)
        "o" '("Organise" . org-gtd-organize)
        "s" '("Schedule" . org-schedule)
        "d" '("Deadline" . org-deadline)
        "a" '("Agenda" . org-agenda)
        "e" '("Export" . org-export-dispatch)
        "n" '("Narrow" . org-narrow-to-element)
        "w" '("Widen" . widen)
        "q" '("Tags" . org-set-tags-command)
        "r" '("Refile" . org-refile))

  (setq org-directory "~/org/gtd/")
  (setq org-agenda-files
        '("~/org/gtd/inbox.org"
          "~/org/gtd/tasks.org"
          "~/org/gtd/calendar"))
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


  (setq org-refile-targets
        '((nil :maxlevel . 3)
	  (org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (setq org-capture-templates
        '(
	  ("i" "Inbox" entry (file "~/org/gtd/inbox.org")
	   "* TODO %?\n %i\n")
	  ("j" "jw")
	  ("jb" "Bible Reading" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/bible-reading.org"))
	  ("jm" "Microphone" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/microphone.org"))
	  ("jz" "Zoom host" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/zoom-host.org"))
	  ("js" "Spiritual Gems" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/spiritual-gems.org"))
	  ("jf" "Field Service Conductor" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/field-service-conductor.org"))
	  ("jr" "Return Visit" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/return-visit.org"))
	  ("jc" "Cong. Bible Study Reader" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/cong-bible-study-reader.org"))
	  ("jt" "Talk" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/talk.org"))
	  ("jd" "Discussion" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/discussion.org"))
	  ("jw" "Watchtower Reader" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/watchtower-reader.org"))
	  ("jk" "Kingdom Cleaning" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/cleaning.org"))
	  ("ji" "Initial Call" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/initial-call.org"))
	  ("ja" "Audio Visual")
	  ("jam" "Audio Visual Main" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/audio-visual.org"))
	  ("jaa" "Audio Visual Assistant" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/audio-visual-assistant.org"))
	  ("jab" "Broadcast" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/broadcast.org"))
	  ("jp" "Prayer/Platform")
	  ("jpo" "Opening Prayer" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/opening-prayer.org"))
	  ("jpc" "Closing Prayer" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/closing-prayer.org"))
	  ("jpp" "Platform" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/platform.org"))
	  ("p" "personal")
	  ("pp" "Personal Project" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/project.org"))
	  ("pc" "House Cleaning" entry (file "~/org/gtd/inbox.org")
	   (file "~/org/gtd/templates/house-cleaning.org"))
	  ("w" "work")
	  ("wi" "Inbox" entry (file "~/org/gtd/work-inbox.org")
	   "* TODO %?\n %i\n")
	  ("wo" "On boarding" entry (file+headline "~/org/gtd/work-inbox.org" "Projects")
	   (file "~/org-work/templates/onboarding.org"))
	  ("wp" "Work Project" entry (file+headline "~/org/gtd/work-inbox.org" "Projects")
	   (file "~/org-work/templates/project.org"))
	  ("we" "Epic Ticket" entry (file+headline "~/org/gtd/work-inbox.org" "Tickets")
	   (file "~/org-work/templates/epic.org"))
	  ("ws" "Story Ticket" entry (file+headline "~/org/gtd/work-inbox.org" "Tickets")
	   (file "~/org-work/templates/story.org"))
	  ("wb" "Bug Ticket" entry (file+headline "~/org/gtd/work-inbox.org" "Tickets")
	   (file "~/org-work/templates/bug.org"))
	  ("m" "Mail" entry (file+headline "~/org/gtd/inbox.org" "")
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
           :files ("~/org/gtd/calendar/personal.org")
           :inbox "~/org/gtd/calendar/inbox.org")
          (:calendar-id "spiritual"
           :files ("~/org/gtd/calendar/spiritual.org")
           :inbox "~/org/gtd/calendar/inbox.org")
          (:calendar-id "events"
           :files ("~/org/gtd/calendar/events.org")
           :inbox "~/org/gtd/calendar/inbox.org")
          (:calendar-id "tony-tayo"
           :files ("~/org/gtd/calendar/joint.org")
           :inbox "~/org/gtd/calendar/inbox.org")
          ))

  (require 'ox-twbs)

  )

;; (after! org-roam
;;   (setq org-roam-directory (file-truename "~/org-roam/")
;;         org-roam-db-location
;;         (expand-file-name ".org-roam.db" doom-cache-dir)

;;         ;; don't auto-sync everything at startup
;;         org-roam-db-update-on-save t)

;;   ;; enable autosync after startup instead of rebuilding immediately
;;   (org-roam-db-autosync-mode))


;;; Projectile  ------------------------------------------------------------
(after! projectile
  ;; (setq projectile-switch-project-action #'projectile-dired)
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

(use-package! org-gtd
  :after org
  :init
  (setq org-gtd-update-ack "4.0.0")
  (setq org-gtd-directory "~/org/gtd/")
  :custom
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "CNCL(c)")))
  :config
  (org-edna-mode)
  ;; (setq org-gtd-update-ack "4.0.0")
  (setq org-gtd-prefix-width 50)
  (setq org-gtd-organize-hooks nil)
  (setq org-gtd-default-file-name "tasks")
  (setq org-gtd-files
        '("~/org/gtd/inbox.org"
          "~/org/gtd/tasks.org"))
  (setq org-agenda-files
        '("~/org/gtd/inbox.org"
          "~/org/gtd/tasks.org"
          "~/org/gtd/calendar"))
  )


;;; Robot Framework ------------------------------------------------------------
(use-package! robot-mode
  :mode "\\.robot\\'")


(add-hook! 'robot-mode-hook
  (setq-local eglot-ignored-server-capabilities
              '(:codeLensProvider))
  (eglot-ensure))

(defun tonyampomah/find-robot-project-root ()
  "Locate the Robot Framework project root."
  (or
   (locate-dominating-file default-directory "pyproject.toml")
   (locate-dominating-file default-directory "robot")
   (locate-dominating-file default-directory "tests")
   (when-let ((root (vc-root-dir)))
     (when (file-exists-p (expand-file-name "robot" root))
       root))))

(defun tonyampomah/run-robot-test ()
  "Run Robot Framework test at point."
  (interactive)
  (let* ((test-name (string-trim (thing-at-point 'line t)))
         (file-name (buffer-file-name))
         (project-root (tonyampomah/find-robot-project-root)))
    (unless project-root
      (user-error "Robot project root not found"))

    (let ((default-directory project-root))
      (compile
       (format "robot -L debug --test %S %S"
               test-name
               (file-relative-name file-name project-root))))))

(defun tonyampomah/run-robot-file ()
  "Run current Robot Framework file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (project-root (tonyampomah/find-robot-project-root)))
    (unless project-root
      (user-error "Robot project root not found"))

    (let ((default-directory project-root))
      (compile
       (format "robot -L debug %S"
               (file-relative-name file-name project-root))))))

(defun tonyampomah/run-robot-project ()
  "Run all Robot Framework tests."
  (interactive)
  (let ((project-root (tonyampomah/find-robot-project-root)))
    (unless project-root
      (user-error "Robot project root not found"))

    (let ((default-directory project-root))
      (compile "robot -L debug tests"))))

(defun tonyampomah/open-robot-report ()
  "Open Robot Framework report and log."
  (interactive)
  (let* ((report-dir (locate-dominating-file default-directory "report.html"))
         (log-file (and report-dir
                        (expand-file-name "log.html" report-dir)))
         (report-file (and report-dir
                           (expand-file-name "report.html" report-dir))))

    (when (and log-file (file-exists-p log-file))
      (browse-url log-file))

    (when (and report-file (file-exists-p report-file))
      (browse-url report-file))))

(defun tonyampomah/robot-format-buffer ()
  "Format current Robot Framework file using robotidy."
  (interactive)
  (when buffer-file-name
    (shell-command
     (format "robotidy %s"
             (shell-quote-argument buffer-file-name)))
    (revert-buffer t t t)))

(after! robot-mode
  (map! :map robot-mode-map
        :localleader
        (:prefix ("t" . "test")
         :desc "Run test at point" "t" #'tonyampomah/run-robot-test
         :desc "Run current file" "c" #'tonyampomah/run-robot-file
         :desc "Run project"      "p" #'tonyampomah/run-robot-project)
        :desc "Format buffer" "f" #'tonyampomah/robot-format-buffer
        :desc "Open report"   "o" #'tonyampomah/open-robot-report))

(after! php-mode
  (map! :map php-mode-map
        :localleader
        :n
        "t t" #'phpunit-current-test
        "t p" #'phpunit-current-project
        "t c" #'phpunit-current-class
        "f"   #'php-search-documentation
        "r"   #'consult-imenu))


(after! eglot
  ;; PHP
  (add-to-list 'eglot-server-programs
               '(robot-mode . ("robotframework_ls")))

  ;; PHP
  (add-to-list 'eglot-server-programs
               '(php-mode . ("intelephense" "--stdio")))

  ;; JS / TS / React
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode
                  typescript-mode typescript-ts-mode
                  tsx-ts-mode)
                 . ("typescript-language-server" "--stdio")))

  ;; Python
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))

  ;; Ansible/YAML
  (add-to-list 'eglot-server-programs
               '(yaml-mode . ("ansible-language-server" "--stdio")))

  ;; CSS
  (add-to-list 'eglot-server-programs
               '(css-mode . ("vscode-css-language-server" "--stdio")))

  ;; HTML / Tailwind
  (add-to-list 'eglot-server-programs
               '(html-mode . ("tailwindcss-language-server" "--stdio"))))



(use-package! ledger-mode
  :mode ("\\.\\(ledger\\|ldg\\|dat\\)\\'" . ledger-mode)
  :hook (ledger-mode . smartparens-mode)
  :custom
  (ledger-clear-whole-transactions t)
  (ledger-highlight-xact-under-point nil)
  (ledger-use-iso-dates nil)
  (ledger-add-transaction-prompt-for-text nil)
  (ledger-mode-should-check-version nil)
  (ledger-post-amount-alignment-column 65)
  (ledger-reports
   '(("bal" "%(binary) -f %(ledger-file) bal")
     ("bal this quarter"
      "%(binary) -f %(ledger-file) --period \"this quarter\" bal")
     ("bal last quarter"
      "%(binary) -f %(ledger-file) --period \"last quarter\" bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(use-package! flycheck-ledger
  :after ledger-mode)

(after! ledger-mode
  ;; ledger-mode bindings
  (map! :map ledger-mode-map
        :localleader
        "a"  #'ledger-add-transaction
        "hd" #'ledger-delete-current-transaction
        "b"  #'ledger-post-edit-amount
        "c"  #'ledger-toggle-current
        "C"  #'ledger-mode-clean-buffer
        "l"  #'ledger-display-ledger-stats
        "p"  #'ledger-display-balance-at-point
        "q"  #'ledger-post-align-xact
        "r"  #'ledger-reconcile
        "R"  #'ledger-report
        "t"  #'ledger-insert-effective-date
        "y"  #'ledger-set-year

        "[[" #'ledger-navigate-prev-xact-or-directive
        "]]" #'ledger-navigate-next-xact-or-directive))

(after! ledger-report
  ;; ledger-report-mode bindings
  (map! :map ledger-report-mode-map
        :localleader
        "q"   #'ledger-report-quit
        "RET" #'ledger-report-edit-report
        "gd"  #'ledger-report-visit-source
        "gr"  #'ledger-report-redo))

;; Open .epub files with nov-mode
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(after! nov
  (map! :map nov-mode-map
        :n "<" #'nov-history-back
        :n ">" #'nov-history-forward)


  (defun my-nov-font-setup ()
    (face-remap-add-relative
     'variable-pitch
     :family "Noto Sans"
     :height 1.6))

  (add-hook 'nov-mode-hook 'my-nov-font-setup)
  ;; (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'writeroom-mode)
  )


(after! writeroom-mode
  (setq writeroom-maximize-window nil
        writeroom-width 80
        writeroom-mode-line t
        writeroom-fullscreen-effect 'fullboth))

(add-to-list 'auto-mode-alist '("\\.kdl\\'" . kdl-mode))

(after! restclient
  (require 'restclient-jq))
