;;; my-vterm.el --- Vterm helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun my-vterm-unbind-keys ()
  "Remove some of the self-insert bindings that conflict with useful global bindings."
  (local-unset-key (kbd "<f1>"))
  (local-unset-key (kbd "<f2>"))
  (local-unset-key (kbd "<f3>"))
  (local-unset-key (kbd "<f4>"))
  (local-unset-key (kbd "<f5>"))
  (local-unset-key (kbd "<f6>"))
  (local-unset-key (kbd "<f7>"))
  (local-unset-key (kbd "<f8>"))
  (local-unset-key (kbd "<f9>"))
  (local-unset-key (kbd "<f10>"))
  (local-unset-key (kbd "<f11>"))
  (local-unset-key (kbd "<f12>"))
  (local-unset-key (kbd "M-s")))

(defun my-vterm--switch-to-new-buffer (name dir)
  "Switch to a new Vterm buffer NAME and set default directory to DIR."
  (let ((default-directory dir))
    (vterm name)
    (switch-to-buffer name)))

(defun my-vterm-project (&optional prefix)
  "Open a Vterm buffer in the root directory of the current project.

With PREFIX always create a new buffer rather than reusing an existing one."
  (interactive "P")
  (if-let ((current-project (project-current)))
      (let ((root (project-root current-project))
            (target-buffer-name (format "*%s: vterm*" (project-name current-project))))
        (if (get-buffer target-buffer-name)
            (if prefix
                (let ((new-buffer-name (generate-new-buffer-name target-buffer-name)))
                  (my-vterm--switch-to-new-buffer new-buffer-name root))
              (switch-to-buffer target-buffer-name))
          (my-vterm--switch-to-new-buffer target-buffer-name root)))
    (message "Not in a project"))) ; TODO: Better to ask to select a project, similar to other project commands.

(with-eval-after-load 'project
  (add-to-list 'project-switch-commands '(my-vterm-project "Vterm" "V") t))

(provide 'my-vterm)

;;; my-vterm.el ends here.
