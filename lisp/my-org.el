(defun my-org-end-of-subtree ()
  (interactive)
  (org-end-of-subtree)
  (org-back-to-heading))

(defun my-org-setup ()
  (setq-local fill-column 120)
  (auto-fill-mode 1))

(add-hook 'org-mode-hook 'my-org-setup)

(provide 'my-org)
