(defun my-project-open-new-frame ()
  (interactive)
  (other-frame-prefix)
  (call-interactively 'project-switch-project)
  (set-frame-name (project-name (project-current))))

(defun my-project-vterm ()
  (interactive)
  (if-let ((path (when (project-current) (project-root (project-current)))))
      (let ((buffer-name (format "*%s: vterm*" (project-name (project-current)))))
        (unless (get-buffer buffer-name)
          (let ((default-directory path))
            (vterm buffer-name)))
        (switch-to-buffer buffer-name))
    (message "Not in a project")))

(add-to-list 'project-switch-commands '(my-project-vterm "Vterm" "V") t)

(provide 'my-project)
