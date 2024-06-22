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

(defun my-gtd--template (name)
  (expand-file-name (concat "gtd-templates/" name ".txt") user-emacs-directory))

(defvar my-gtd--prompt-history nil)

(defun my-gtd--prompt (prompt var)
  (make-local-variable var)
  (set var (read-string (concat prompt ": ") nil 'my-gtd--prompt-history)))

(setq org-capture-templates
      `(("i" "Inbox")
        ("ii" "todo" entry (file+headline ,my-gtd-inbox "Inbox") "* TODO %?")
        ("iI" "note" entry (file+headline ,my-gtd-inbox "Inbox") "* %?")
        ("il" "todo link" entry (file+headline ,my-gtd-inbox "Inbox") "* TODO [[%c][%^{Description}]]%?")
        ("iL" "note link" entry (file+headline ,my-gtd-inbox "Inbox") "* [[%c][%^{Description}]]%?")
        ("p" "Project")
        ("pp" "project" entry (file ,my-gtd-projects) (file ,(my-gtd--template "project")))
        ("pP" "shared project" entry (file ,my-gtd-shared-projects) (file ,(my-gtd--template "project")))
        ("ps" "sub-project" entry (file+headline ,my-gtd-inbox "Inbox") (file ,(my-gtd--template "subproject")))
        ("s" "Someday")
        ("ss" "someday area" entry (file ,my-gtd-someday) (file ,(my-gtd--template "someday")))
        ("sS" "shared someday area" entry (file ,my-gtd-shared-someday) (file ,(my-gtd--template "someday")))))

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

(setq org-refile-targets
      `(((,my-gtd-inbox
          ,my-gtd-shared-inbox
          ,my-gtd-someday
          ,my-gtd-shared-someday
          ,my-gtd-tasks
          ,my-gtd-shared-tasks
          ,my-gtd-calendar
          ,my-gtd-shared-calendar)
         :level . 1)
        ((,my-gtd-projects
          ,my-gtd-shared-projects)
         :maxlevel . 2)))

(defun my-gtd-verify-refile-target ()
  (require 'my-elisp-helpers)
  (if (string-in-list-p (buffer-file-name) (list my-gtd-projects my-gtd-shared-projects))
      (when-let ((type (org-element-property :MY_GTD_TYPE (org-element-at-point))))
        (string= type "project"))
    t))

(setq org-refile-target-verify-function 'my-gtd-verify-refile-target)

(provide 'my-gtd)
