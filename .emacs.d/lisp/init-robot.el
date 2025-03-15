;;; init-robot.el --- robot -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package robot-mode
  :ensure t)

(defun lsp-robot-framework-server-command ()
  "Return the command to start the Robot Framework Language Server."
  (list "robotframework_ls"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-robot-framework-server-command)
  :major-modes '(robot-mode)
  :server-id 'robotframework-ls))


(defun find-robot-project-root ()
  "Find the root directory of the Robot Framework project."
  (or (locate-dominating-file default-directory "pyproject.toml")  ;; Change marker if needed
      (locate-dominating-file default-directory "robot_tests")     ;; Alternative marker
      (vc-root-dir)))  ;; Use version control root as fallback

(defun run-robot-test-in-project ()
  "Run the Robot Framework test case at point within the project root using a relative path."
  (interactive)
  (let* ((test-name (thing-at-point 'line t))  ;; Get current test name
         (test-name (string-trim test-name))   ;; Trim whitespace
         (file-name (buffer-file-name))        ;; Get absolute path of test file
         (project-root (find-robot-project-root))  ;; Find project root
         (relative-file (file-relative-name file-name project-root)))  ;; Get relative path
    (unless project-root
      (error "Project root not found!"))
    (let ((default-directory project-root))  ;; Set working directory
      (compile (format "robot --test \"%s\" \"%s\"" test-name relative-file)))))

(global-set-key (kbd "C-c r") 'run-robot-test-in-project)  ;; Shortcut to run a test

(defun run-robot-file-in-project ()
  "Run the Robot Framework test case file."
  (interactive)
  (let* (
         (file-name (buffer-file-name))        ;; Get absolute path of test file
         (project-root (find-robot-project-root))  ;; Find project root
         (relative-file (file-relative-name file-name project-root)))  ;; Get relative path
    (unless project-root
      (error "Project root not found!"))
    (let ((default-directory project-root))  ;; Set working directory
      (compile (format "robot \"%s\"" relative-file)))))

(defun run-robot-project ()
  "Run the Robot Framework test case within an entire tests directory."
  (interactive)
  (let* (
         (project-root (find-robot-project-root))  ;; Find project root
         )  ;; Get relative path
    (unless project-root
      (error "Project root not found!"))
    (let ((default-directory project-root))  ;; Set working directory
      (compile (format "robot tests")))))

(defun robot-format-on-save ()
  "Format Robot Framework file using robotidy before saving."
  (when (eq major-mode 'robot-mode)
    (shell-command (format "robotidy %s" (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))  ;; Reload buffer to reflect changes

;; (add-hook 'before-save-hook 'robot-format-on-save)

;; (global-set-key (kbd "C-c t") 'robot-format-manually)  ;; Bind to C-c t



(kd/my-local-leader-def'normal robot-mode-map
			       "tt" 'run-robot-test-in-project
			       "tf" 'run-robot-file-in-project
			       "f" 'lsp-format-buffer
			       "tp" 'run-robot-project)

(provide 'init-robot)
;;; init-robot.el ends here
