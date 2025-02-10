;;; init-chat.el --- chat -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; slack
(use-package slack
  :defer t
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "custom-gateway"
   :default t
   :token (auth-source-pick-first-password
           :host "custom-gateway.slack.com"
           :user "tony.ampomah@kornit.com")
   :subscribed-channels '(architects askme general new-relic random)
   :full-and-display-names t)


  (evil-define-key 'normal slack-info-mode-map
    ",u" 'slack-room-update-messages)
  (evil-define-key 'normal slack-mode-map
    ",c" 'slack-buffer-kill
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete
    ",u" 'slack-room-update-messages
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message)
  (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel))


(kd/leader-key-def
  "ass" 'slack-start
  "asj" 'slack-channel-select
  "asd" 'slack-im-select
  "asq" 'slack-ws-close
  )

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(provide 'init-chat)
;;; init-chat.el ends here
