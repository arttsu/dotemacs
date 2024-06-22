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

(defun my-gtd-template (name)
  (expand-file-name (concat "gtd-templates/" name ".txt") user-emacs-directory))

(setq org-capture-templates
      `(("i" "Inbox")
        ("ii" "Todo" entry (file+headline ,my-gtd-inbox "Inbox") "* TODO %?")
        ("iI" "Note" entry (file+headline ,my-gtd-inbox "Inbox") "* %?")
        ("il" "Todo link" entry (file+headline ,my-gtd-inbox "Inbox") "* TODO [[%c][%^{Description}]]%?")
        ("iL" "Note link" entry (file+headline ,my-gtd-inbox "Inbox") "* [[%c][%^{Description}]]%?")
        ("p" "Project")
        ("pp" "Project" entry (file ,my-gtd-projects) (file ,(my-gtd-template "project")))))

(defun my-gtd-capture-to-inbox (&optional prefix)
  (interactive "P")
  (cond
   ((equal prefix nil) (org-capture nil "ii"))
   ((equal prefix '(4)) (org-capture nil "il"))
   (t (message "Prefix '%s' not supported" prefix))))

(defun my-gtd-capture-note-to-inbox (&optional prefix)
  (interactive "P")
  (cond
   ((equal prefix nil) (org-capture nil "iI"))
   ((equal prefix '(4)) (org-capture nil "iL"))
   (t (message "Prefix '%s' not supported" prefix))))

(provide 'my-gtd)
