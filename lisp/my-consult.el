(defun my-consult-outline-advice (location)
  (when (derived-mode-p 'org-mode)
    (beginning-of-line)))

(advice-add 'consult-outline :after 'my-consult-outline-advice)

(provide 'my-consult)
