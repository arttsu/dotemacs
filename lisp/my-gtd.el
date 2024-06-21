(defvar my-gtd-dir)
(defvar my-gtd-shared-dir)

(defun my-gtd--path (name)
  (expand-file-name (format "%s.org" name) my-gtd-dir))

(defun my-gtd--shared-path (name)
  (expand-file-name (format "%s.org" name) my-gtd-shared-dir))

(setq my-gtd-inbox (my-gtd--path "inbox"))
(setq my-gtd-tasks (my-gtd--path "tasks"))
(setq my-gtd-calendar (my-gtd--path "calendar"))
(setq my-gtd-projects (my-gtd--path "projects"))
(setq my-gtd-someday (my-gtd--path "someday"))

(setq my-gtd-shared-inbox (my-gtd--shared-path "inbox"))
(setq my-gtd-shared-tasks (my-gtd--shared-path "tasks"))
(setq my-gtd-shared-calendar (my-gtd--shared-path "calendar"))
(setq my-gtd-shared-projects (my-gtd--shared-path "projects"))
(setq my-gtd-shared-someday (my-gtd--shared-path "someday"))

(setq org-capture-templates
      `(("i" "Inbox")
        ("ii" "Todo" entry (file+headline ,my-gtd-inbox "Inbox") "* TODO %?")))

(defun my-gtd-capture-to-inbox (&optional prefix)
  (interactive "P")
  (cond
   ((equal prefix nil) (org-capture nil "ii"))))

(provide 'my-gtd)
