;;; init-finance.el --- finance -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package ledger-mode
  :defer t
  :config
  (add-hook 'ledger-mode-hook 'smartparens-mode)
  (setq ledger-clear-whole-transactions 1
	ledger-highlight-xact-under-point nil
	ledger-use-iso-dates nil
	ledger-add-transaction-prompt-for-text nil
	ledger-mode-should-check-version nil
	ledger-post-amount-alignment-column 60)
  
  :mode ("\\.\\(ledger\\|ldg\\|dat\\)\\'" . ledger-mode)
  :custom
  (ledger-reports '(("bal" "%(binary) -f %(ledger-file) bal")
		    ("bal this quarter" "%(binary) -f %(ledger-file) --period \"this quarter\" bal")
		    ("bal last quarter" "%(binary) -f %(ledger-file) --period \"last quarter\" bal")
		    ("reg" "%(binary) -f %(ledger-file) reg")
		    ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
		    ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(use-package flycheck-ledger)

(kd/my-local-leader-def 'normal ledger-mode-map
  "a" 'ledger-add-transaction
  "]]" #'ledger-navigate-next-xact-or-directive
  "[[" #'ledger-navigate-prev-xact-or-directive)

(kd/my-local-leader-def 'normal ledger-report-mode-map
  "q"   #'ledger-report-quit
  "RET" #'ledger-report-edit-report
  "gd"  #'ledger-report-visit-source
  "gr"  #'ledger-report-redo)

(kd/my-local-leader-def 'normal ledger-mode-map
  "hd" #'ledger-delete-current-transaction
  "a" #'ledger-add-transaction
  "b" #'ledger-post-edit-amount
  "c" #'ledger-toggle-current
  "C" #'ledger-mode-clean-buffer
  "l" #'ledger-display-ledger-stats
  "p" #'ledger-display-balance-at-point
  "q" #'ledger-post-align-xact
  "r" #'ledger-reconcile
  "R" #'ledger-report
  "t" #'ledger-insert-effective-date
  "y" #'ledger-set-year)


(provide 'init-finance)
;;; init-finance.el ends here
