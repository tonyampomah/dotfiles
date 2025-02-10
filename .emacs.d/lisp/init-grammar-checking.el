;;; init-grammar-checking.el --- grammar-checking -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package langtool
  :defer t
  :commands
  (langtool-check
   langtool-check-done
   langtool-show-message-at-point
   langtool-correct-buffer))

(use-package writegood-mode
  :defer t)

(provide 'init-grammar-checking)
;;; init-grammar-checking.el ends here
