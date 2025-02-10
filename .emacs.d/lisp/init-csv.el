;;; init-csv.el --- csv -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package csv-mode
  :defer t
  :config
  (setq csv-separators '("," ";" "|" " ")))

(provide 'init-csv)
;;; init-csv.el ends here
