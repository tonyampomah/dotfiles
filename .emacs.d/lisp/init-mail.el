;;; init-mail.el --- mail -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (if (memq window-system '(mac ns))
;;     (add-to-list 'load-path "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e")
;;   (setq mu4e-mu-binary "/opt/homebrew/bin/mu"))
(require 'mu4e)

;; This is set to 't' to avoid mail syncing issues when using mbsync
(setq mu4e-change-filenames-when-moving t)

;; Refresh mail using isync every 10 minutes
(setq mu4e-update-interval (* 10 60))
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-maildir "~/Mail")
(setq mu4e-context-policy 'pick-first)

;; Configure the function to use for sending mail
(setq message-send-mail-function 'smtpmail-send-it)

(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "Personal"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/tony@arksolutions.it" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "tony@arksolutions.it")
		(user-full-name    . "Tony Ampomah")
		(mu4e-compose-signature . (concat "Tony Ampomah\n" "Arksolutions.it"))
                (smtpmail-smtp-server  . "smtppro.zoho.com")
                (smtpmail-smtp-service . 465)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/tony@arksolutions.it/Drafts")
                (mu4e-sent-folder  . "/tony@arksolutions.it/Sent Mail")
                (mu4e-refile-folder  . "/tony@arksolutions.it/All Mail")
                (mu4e-trash-folder  . "/tony@arksolutions.it/Trash")))

       (make-mu4e-context
        :name "Joint"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/tony.tayo@theampomahs.com" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "tony.tayo@theampomahs.com")
                (user-full-name    . "Tony & Tayo")
		(mu4e-compose-signature . (concat "Many thanks,\n" "Tony & Tayo"))
                (smtpmail-smtp-server  . "smtp.zoho.com")
                (smtpmail-smtp-service . 465)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/tony.tayo@theampomahs.com/Drafts")
                (mu4e-sent-folder  . "/tony.tayo@theampomahs.com/Sent")
                (mu4e-refile-folder  . "/tony.tayo@theampomahs.com/All Mail")
                (mu4e-trash-folder  . "/tony.tayo@theampomahs.com/Trash")))

       (make-mu4e-context
        :name "Gmail"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/itechytony@gmail.com" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "itechytony@gmail.com")
                (user-full-name    . "Tony Ampomah")
		(mu4e-compose-signature . (concat "Tony Ampomah\n" "Arksolutions.it"))
                (smtpmail-smtp-server  . "smtp.gmail.com")
                (smtpmail-smtp-service . 465)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/itechytony@gmail.com/[Gmail]/Drafts")
                (mu4e-sent-folder  . "/itechytony@gmail.com/[Gmail]/Sent Mail")
                (mu4e-refile-folder  . "/itechytony@gmail.com/[Gmail]/All Mail")
                (mu4e-trash-folder  . "/itechytony@gmail.com/[Gmail]/Trash")))

       (make-mu4e-context
        :name "Spiritual"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/tony.ampomah.jw@gmail.com" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "tony.ampomah.jw@gmail.com")
                (user-full-name    . "Tony Ampomah")
		(mu4e-compose-signature . (concat "Warm love,\n" "Tony"))
                (smtpmail-smtp-server  . "smtp.gmail.com")
                (smtpmail-smtp-service . 465)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/tony.ampomah.jw@gmail.com/[Gmail]/Drafts")
                (mu4e-sent-folder  . "/tony.ampomah.jw@gmail.com/[Gmail]/Sent Mail")
                (mu4e-refile-folder  . "/tony.ampomah.jw@gmail.com/[Gmail]/All Mail")
                (mu4e-trash-folder  . "/tony.ampomah.jw@gmail.com/[Gmail]/Trash")))


       ))

(setq mu4e-maildir-shortcuts
      '(("/tony@arksolutions.it/Inbox"             . ?i)
        ("/tony@arksolutions.it/Sent Mail" . ?s)
        ("/tony@arksolutions.it/Trash"     . ?t)
        ("/tony@arksolutions.it/Drafts"    . ?d)
        ("/tony@arksolutions.it/All Mail"  . ?a)))

(setq org-mime-export-options '(:section-numbers nil
						 :with-author nil
						 :with-toc nil))

(use-package org-mime
  :ensure t)

(add-hook 'org-mime-html-hook
          (lambda ()
            (org-mime-change-element-style
             "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                           "#E6E1DC" "#232323"))))

(add-hook 'message-send-hook 'org-mime-htmlize)

(provide 'init-mail)
;;; init-mail.el ends here
