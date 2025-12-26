;;; my-org.el --- Org Helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; TODO: Function: Create Org directories.

(defun my-org-template (name)
  "Return the path to a template file.

NAME is the file name w/o the extension."
  (expand-file-name (concat "templates/" name ".txt") user-emacs-directory))

(defun my-org-inbox-target (context)
  "Return the capture target for the inbox in the specified context.

CONTEXT should be either \"local\" or \"shared\""
  (let ((file (expand-file-name (concat context "/gtd/inbox.org") my-org-dir)))
    (unless (file-writable-p file)
      (error "%s is not writable" file))
    `(file+headline ,file "Items")))

(defun my-org-gym-log-target (plan)
  "Return the capture target for a gym plan session.

PLAN should be either \"A\" or \"B\"."
  (let ((file (expand-file-name "local/gym/gym-log.org" my-org-dir))
        (heading (concat "Plan " plan)))
    `(file+headline ,file ,heading)))

(defun my-org-capture-templates ()
  "Return a list of Org capture templates."
  `(("i" "Inbox")
    ("in" "local note" entry ,(my-org-inbox-target "local") (file ,(my-org-template "note")))
    ("iN" "shared note" entry ,(my-org-inbox-target "shared") (file ,(my-org-template "note")))
    ("it" "local to-do" entry ,(my-org-inbox-target "local") (file ,(my-org-template "todo")))
    ("iT" "shared to-do" entry ,(my-org-inbox-target "shared") (file ,(my-org-template "todo")))
    ("g" "Gym")
    ("ga" "plan A session" entry ,(my-org-gym-log-target "A") (file ,(my-org-template "gym/plan-a")))
    ("gb" "plan B session" entry ,(my-org-gym-log-target "B") (file ,(my-org-template "gym/plan-b")))))

(defun my-org-agenda-files (context)
  "Return the list of Org agenda files in the specified context.

CONTEXT should be either \"local\" or \"shared\""
  (list (expand-file-name (concat context "/gtd") my-org-dir)
        (expand-file-name (concat context "/gtd/projects") my-org-dir)))

(defun my-org-capture-note (&optional prefix)
  "Capture a note to the local inbox.

With a PREFIX argument capture to the shared inbox."
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "in"))
        ((equal prefix '(4)) (org-capture nil "iN"))
        (t (user-error "Invalid prefix argument: %s" prefix))))

(defun my-org-capture-todo (&optional prefix)
  "Capture a to-do to the local inbox.

With a PREFIX argument capture to the shared inbox."
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "it"))
        ((equal prefix '(4)) (org-capture nil "iT"))
        (t (user-error "Invalid prefix argument: %s" prefix))))

(defun my-org-entry-scheduled-or-deadline-p (point)
  "Return t if the entry at POINT is scheduled or has a deadline."
  (or (org-get-scheduled-time point) (org-get-deadline-time point)))

(defun my-org-has-tag (&rest tags)
  "Return t if the entry at point has at least one of the TAGS."
  (let ((entry-tags (org-get-tags)))
    (seq-some (lambda (tag) (member tag entry-tags)) tags)))

(defun my-org-direct-parent-has-tag (&rest tags)
  "Return t if the direct parent of the entry at point has at least one of the TAGS."
  (save-excursion
    (let ((parent-pos (org-up-heading-safe)))
      (when parent-pos
        (apply 'my-org-has-tag tags)))))

(defun my-org-day-agenda-task-priority-too-low-p ()
  "Return t if the task at point priority is too low to display in the \"Day\" agenda."
  (let ((priority (org-get-priority (thing-at-point 'line t))))
    (or (= priority 0)
        (and (<= priority 2000) (not (my-org-direct-parent-has-tag "agenda"))))))

(defun my-org-day-agenda-skip-task ()
  "Decide whether or not to display a task in a \"Day\" agenda."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    ;; TODO: Skip low-prio tasks.
    (when (or (my-org-entry-scheduled-or-deadline-p (point))
              (my-org-day-agenda-task-priority-too-low-p))
      subtree-end)))

(defun my-org-get-first-heading ()
  "Return the first heading in the buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (unless (org-at-heading-p)
        (org-forward-heading-same-level 1))
      (when (org-at-heading-p)
        (org-get-heading t t t t)))))

(defun my-org-agenda-category ()
  "Return the category to display in the agenda for the entry at point."
  (if (derived-mode-p 'org-mode)
      (if (my-org-has-tag "project")
          ""
        (if-let ((top-level-heading (my-org-get-first-heading)))
            (if (> (length top-level-heading) 19)
                (concat (substring top-level-heading 0 18) "…")
              top-level-heading)
          (buffer-file-name)))
    ""))

(defun my-org-day-agenda-command (files)
  "Return \"Day\" agenda command.

FILES is a list of files to collect tasks and projects from."
  ;; TODO: Add projects.
  `((agenda "" ((org-agenda-span 1)
                (org-agenda-skip-scheduled-if-done t)
                (org-agenda-skip-deadline-if-done t)
                (org-agenda-skip-timestamp-if-done t)
                (org-agenda-files ',files)))
    (todo "TODO" ((org-agenda-overriding-header "Non-scheduled Tasks")
                  (org-agenda-skip-function 'my-org-day-agenda-skip-task)
                  (org-agenda-files ',files)))))

(defun my-org-day-agenda-commands (&optional include-shared-by-default)
  "Return \"Day\" and \"Day w/ or w/o Shared\" agenda commands.

If INCLUDE-SHARED-BY-DEFAULT is truthy the \"Day\" command will include
tasks and projects in the shared directory in addition to the local.
The second returned command will be \"Day w/o Shared\".  Otherwise, the
logic is inversed."
  (let* ((local-files (my-org-agenda-files "local"))
         (all-files (append local-files (my-org-agenda-files "shared")))
         (default-files (if include-shared-by-default all-files local-files))
         (secondary-files (if include-shared-by-default local-files all-files))
         (secondary-label (if include-shared-by-default "Day w/o Shared" "Day w/ Shared")))
    `(("d" "Day" ,(my-org-day-agenda-command default-files))
      ("D" ,secondary-label ,(my-org-day-agenda-command secondary-files)))))

(defun my-org-list-files (dir)
  "Return the list of all .org files in the directory specified by DIR."
  (directory-files dir t (rx ".org" string-end)))

(defun my-org-do-refile-note (refile-f)
  "Perform the refiling operation specified by REFILE-F, temporarily overriding refile targets."
  (let ((original-targets org-refile-targets))
    (unwind-protect
        (let ((targets (append (my-org-list-files (expand-file-name "local/notes" my-org-dir))
                               (my-org-list-files (expand-file-name "shared/notes" my-org-dir)))))
          (setq org-refile-targets `((,targets :tag "refile")))
          (apply refile-f ()))
      (setq org-refile-targets original-targets))))

(defun my-org-refile-note ()
  "Refile a tree to notes via move."
  (interactive)
  (my-org-do-refile-note 'org-refile))

(defun my-org-refile-copy-note ()
  "Refile a tree to notes via copy."
  (interactive)
  (my-org-do-refile-note 'org-refile-copy))

(defun my-org-setup-gtd-and-knowledge-management ()
  "Create GTD & Knowledge Management directory if it doesn't exist."
  (unless (file-directory-p my-org-dir)
    (copy-directory (expand-file-name "templates/org" user-emacs-directory) my-org-dir)))

(provide 'my-org)

;;; my-org.el ends here.
