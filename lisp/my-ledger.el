(defun my-ledger--bal-period (period)
  (format "%%(binary) -f %%(ledger-file) --invert --period \"%s\" -S amount bal ^Income ^Expenses" period))

(ledger-reports-add "bal-this-month" (my-ledger--bal-period "this month"))
(ledger-reports-add "bal-last-month" (my-ledger--bal-period "last month"))

(provide 'my-ledger)
