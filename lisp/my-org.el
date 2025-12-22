;;; my-org.el --- Org Helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; TODO: Function: Create Org directories.

(defun my-org-template (name)
  "Return the path to a template file.

NAME is the file name w/o the extension."
  (expand-file-name (concat "templates/" name ".txt") user-emacs-directory))

(defun my-org-inbox-target (org-dir context)
  "Return the capture target for the inbox in the specified context.

ORG-DIR is the path to the GTD & Knowledge Management directory.
CONTEXT should be either 'local' or 'shared'"
  (let ((file (expand-file-name (concat context "/gtd/inbox.org") org-dir)))
    (unless (file-writable-p file)
      (error "%s is not writable" file))
    `(file+headline ,file "Items")))

(defun my-org-capture-templates (org-dir)
  "Return a list of Org capture templates.

ORG-DIR is the path to the GTD & Knowledge Management directory."
  `(("i" "Inbox")
    ("in" "local note" entry ,(my-org-inbox-target org-dir "local") (file ,(my-org-template "note")))
    ("iN" "shared note" entry ,(my-org-inbox-target org-dir "shared") (file ,(my-org-template "note")))
    ("it" "local to-do" entry ,(my-org-inbox-target org-dir "local") (file ,(my-org-template "todo")))
    ("iT" "shared to-do" entry ,(my-org-inbox-target org-dir "shared") (file ,(my-org-template "todo")))))

(defun my-org-agenda-files (org-dir)
  "Return a list of Org agenda files.

ORG-DIR is the path to the GTD & Knowledge Management directory."
  (list (expand-file-name "local/gtd" org-dir)
        (expand-file-name "local/gtd/projects" org-dir)
        (expand-file-name "shared/gtd" org-dir)
        (expand-file-name "shared/gtd/projects" org-dir)))

(defun my-org-setup-gtd-and-knowledge-management (org-dir)
  "Create the GTD & Knowledge Management directory if it doesn't exist. Set
capture templates and agenda files.

ORG-DIR is the path to the GTD & Knowledge Management directory."
  (unless (file-directory-p org-dir)
    (copy-directory (expand-file-name "templates/org" user-emacs-directory) org-dir))
  (setq org-capture-templates (my-org-capture-templates org-dir))
  (setq org-agenda-files (my-org-agenda-files org-dir)))

(defun my-org-capture-note (&optional prefix)
  "Capture a note to the local inbox.

With a PREFIX argument 'C-u' capture a note to the shared inbox."
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "in"))
        ((equal prefix '(4)) (org-capture nil "iN"))
        (t (user-error "Invalid prefix argument: %s" prefix))))

(defun my-org-capture-todo (&optional prefix)
  "Capture a to-do to the local inbox.

With a PREFIX argument 'C-u' capture a to-do to the shared inbox."
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "it"))
        ((equal prefix '(4)) (org-capture nil "iT"))
        (t (user-error "Invalid prefix argument: %s" prefix))))

(provide 'my-org)

;;; my-org.el ends here.
