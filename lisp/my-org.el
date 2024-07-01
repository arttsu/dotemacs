(defun my-org-end-of-subtree ()
  (interactive)
  (org-end-of-subtree)
  (org-back-to-heading))

(provide 'my-org)
