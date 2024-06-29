(defun my-project-open-new-frame ()
  (interactive)
  (other-frame-prefix)
  (call-interactively 'project-switch-project)
  (set-frame-name (project-name (project-current))))

(provide 'my-project)
