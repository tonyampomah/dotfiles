;;; init-terminal.el--- terminal -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'sh-mode-hook (setq sh-basic-offset 2 sh-indentation 2))

(defun my-shell-mode-hook ()
  "Custom `shell-mode' behaviours."
  ;; Kill the buffer when the shell process exits.
  (let* ((proc (get-buffer-process (current-buffer)))
	 (sentinel (process-sentinel proc)))
    (set-process-sentinel
     proc
     `(lambda (process signal)
	;; Call the original process sentinel first.
	(funcall #',sentinel process signal)
	;; Kill the buffer on an exit signal.
	(and (memq (process-status process) '(exit signal))
	     (buffer-live-p (process-buffer process))
	     (kill-buffer (process-buffer process)))))))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(use-package vterm
  :defer t
  :commands vterm-mode
  :config
  (setq vterm-shell "/bin/zsh")
  (setq vterm-max-scrollback 5000)
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-kill-buffer-on-exit t))

(add-hook 'vterm-mode-hook (lambda () (setq global-hl-line-mode nil)))

(use-package vterm-toggle
  :defer t)

(use-package shell-pop
  :defer t
  :config
  (defun shell-pop--set-exit-action ()
    (if (string= shell-pop-internal-mode "eshell")
	(add-hook 'eshell-exit-hook 'shell-pop--kill-and-delete-window nil t)
      (let ((process (get-buffer-process (current-buffer))))
	(when process
	  (set-process-sentinel
	   process
	   (lambda (_proc change)
	     (when (string-match-p "\\(?:finished\\|exited\\)" change)
	       (if (one-window-p)
		   (switch-to-buffer shell-pop-last-buffer)
		 (kill-buffer-and-window)))))))))
  )

(custom-set-variables
 '(shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda nil (vterm)))))
 '(shell-pop-window-size 50)
 '(shell-pop-full-span t)
 '(shell-pop-term-shell "/usr/zsh")
 '(shell-pop-window-position "bottom"))


(use-package multi-vterm
  :defer t
  :config
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (setq-local evil-insert-state-cursor 'box)
	      (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(defun open-terminal-in-projectile-root ()
  (interactive)
  (let ((workdir (if (projectile-project-root)
		     (projectile-project-root)
		   (projectile-project-root))))
    (call-process-shell-command
     (concat "alacritty --working-directory  "workdir) nil 0)))


(straight-use-package 'ob-tmux)

(provide 'init-terminal)
;;; init-terminal.el ends here
