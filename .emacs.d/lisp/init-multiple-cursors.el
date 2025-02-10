;;; init-multiple-cursors.el --- multiple-cursors -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package evil-mc
  :defer t
  :commands (evil-mc-make-cursor-here
	     evil-mc-make-all-cursors
	     evil-mc-undo-all-cursors
	     evil-mc-pause-cursors
	     evil-mc-resume-cursors
	     evil-mc-make-and-goto-first-cursor
	     evil-mc-make-and-goto-last-cursor
	     evil-mc-make-cursor-in-visual-selection-beg
	     evil-mc-make-cursor-in-visual-selection-end
	     evil-mc-make-cursor-move-next-line
	     evil-mc-make-cursor-move-prev-line
	     evil-mc-make-cursor-at-pos
	     evil-mc-has-cursors-p
	     evil-mc-make-and-goto-next-cursor
	     evil-mc-skip-and-goto-next-cursor
	     evil-mc-make-and-goto-prev-cursor
	     evil-mc-skip-and-goto-prev-cursor
	     evil-mc-make-and-goto-next-match
	     evil-mc-skip-and-goto-next-match
	     evil-mc-skip-and-goto-next-match
	     evil-mc-make-and-goto-prev-match
	     evil-mc-skip-and-goto-prev-match)
  :ensure t
  :config
  (global-evil-mc-mode +1))

(use-package evil-multiedit
  :defer t)

(general-define-key
 :states 'normal
 :prefix "gz"
 "d" #'evil-mc-make-and-goto-next-match
 "D" #'evil-mc-make-and-goto-prev-match
 "j" #'evil-mc-make-cursor-move-next-line
 "k" #'evil-mc-make-cursor-move-prev-line
 "m" #'evil-mc-make-all-cursors
 "n" #'evil-mc-make-and-goto-next-cursor
 "N" #'evil-mc-make-and-goto-last-cursor
 "p" #'evil-mc-make-and-goto-prev-cursor
 "P" #'evil-mc-make-and-goto-first-cursor
 "q" #'evil-mc-undo-all-cursors
 "t" #'+multiple-cursors/evil-mc-toggle-cursors
 "u" #'+multiple-cursors/evil-mc-undo-cursor
 "z" #'+multiple-cursors/evil-mc-toggle-cursor-here
 "I" #'evil-mc-make-cursor-in-visual-selection-beg
 "A" #'evil-mc-make-cursor-in-visual-selection-end)

(general-nmap "M-d" 'evil-multiedit-match-symbol-and-next)
(general-nmap "M-D" 'evil-multiedit-match-symbol-and-prev)
(general-vmap "R" 'evil-multiedit-match-all)
(general-vmap "M-d" 'evil-multiedit-match-and-next)
(general-vmap "M-D" 'evil-multiedit-match-and-prev)

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
