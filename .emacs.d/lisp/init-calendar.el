;;; init-calendar.el --- calendar -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq calendar-week-start-day 1)
(use-package calfw)
(use-package calfw-org)
(use-package calfw-cal)

(kd/leader-key-def
  "cc"  '(cfw:open-org-calendar :which-key "calendar"))

(provide 'init-calendar)
;;; init-calendar.el ends here
