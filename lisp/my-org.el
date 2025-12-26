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
  "Decide whether or not to display the task at point in the \"Day\" agenda."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    ;; TODO: Skip low-prio tasks.
    (when (or (my-org-entry-scheduled-or-deadline-p (point))
              (my-org-day-agenda-task-priority-too-low-p))
      subtree-end)))

(defun my-org-day-agenda-skip-project ()
  "Decide whether or not to display the project at point in the \"Day\" agenda."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (priority (org-get-priority (thing-at-point 'line t))))
    (when (= priority 0)
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
                  (org-agenda-files ',files)))
    (tags "project" ((org-agenda-overriding-header "Projects")
                     (org-agenda-sorting-strategy '(priority-down))
                     (org-agenda-skip-function 'my-org-day-agenda-skip-project)
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

(defun my-org-remove-priority-when-done ()
  "Remove priority from the entry at point if its to-do state changed to \"DONE\"."
  (ignore-errors
    (when (string= org-state "DONE")
      (org-entry-put (point) "PRIORITY" nil))))

(defun my-org-require-at-heading ()
  "Throw user error unless the point is at a heading."
  (unless (org-at-heading-p) (user-error "Must be at a heading")))

(defconst my-org-default-timestamp "[1900-01-01 Mon 00:00]")

(defun my-org-extract-created-timestamp ()
  "Return the 'created on' timestamp of the entry at point."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree) ; TODO: Narrow to element.
      (goto-char (point-min))
      (if (re-search-forward (rx "Created on " (group "[" (1+ (not "]")) "]")) nil t)
          (match-string-no-properties 1)
        my-org-default-timestamp))))

(defun my-org-extract-closed-timestamp ()
  "Return the 'CLOSED:' timestamp of the entry at point."
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree) ; TODO: Narrow to element.
      (goto-char (point-min))
      ;; TODO: DRY timestamp capture group.
      (if (re-search-forward (rx "CLOSED: " (group "[" (1+ (not "]")) "]")) nil t)
          (match-string-no-properties 1)
        my-org-default-timestamp))))

(defun my-org-sort-entries ()
  "Sort entries of the subtree at point."
  (interactive)
  (my-org-require-at-heading)
  (org-sort-entries nil ?f 'my-org-extract-created-timestamp)
  (org-sort-entries nil ?f 'my-org-extract-closed-timestamp)
  ;; By priority.
  (org-sort-entries nil ?p)
  ;; By to-do state.
  (org-sort-entries nil ?o))

(defun my-org-format-buffer ()
  "Format the current buffer."
  (interactive)
  (ignore-errors
    (org-map-entries #'my-org-sort-entries "sort" 'file)))

(defconst my-org-contexts '("Local" "Shared"))

(defun my-org-context-dir (context)
  "Return path to the org directory in the specified CONTEXT."
  (cond ((string= context "Local") (expand-file-name "local" my-org-dir))
        ((string= context "Shared") (expand-file-name "shared" my-org-dir))
        (t (error "Invalid context: %s" context))))

(defun my-org-priority-char-to-cookie (char)
  "Convert CHAR to a string representing Org priority."
  (let ((clean-char (cond ((memq char '(?\r ?\n)) ?D)
                          ((and (>= char ?a) (<= char ?e)) (upcase char))
                          ((and (>= char ?A) (<= char ?E)) char)
                          (t ?D))))
    (format "[#%c]" clean-char)))

(defun my-org-now-timestamp ()
  "Return the current time formatted as Org timestamp."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun my-org-create-project-contents (title priority)
  "Return file contents for a new project.

TITLE is the project title.

PRIORITY is a character representing the priority of the project."
  (let ((template (with-temp-buffer (insert-file-contents (my-org-template "project")) (buffer-string)))
        (priority-cookie (my-org-priority-char-to-cookie priority))
        (id (org-id-new))
        (timestamp (my-org-now-timestamp)))
    (format template priority-cookie title id timestamp)))

(defun my-org-find-file-in-new-session (path)
  "Find file in PATH in a new session."
  (let ((name (file-name-sans-extension (file-name-nondirectory path))))
    (easysession-switch-to-and-restore-geometry name)
    (find-file path)))

(defun my-org-create-project ()
  "Create a project from the template."
  (interactive)
  (let ((context (completing-read "Context: " my-org-contexts nil t))
        (title (read-string "Title: "))
        (priority (read-char-choice "Priority [A-E, default D]: " '(?A ?B ?C ?D ?E ?a ?b ?c ?d ?e ?\r ?\n))))
    (when (string-empty-p title)
      (user-error "Title cannot be empty"))
    (let ((dir (expand-file-name "gtd/projects" (my-org-context-dir context))))
      (require 'org-node)
      (let* ((filename (org-node-title-to-basename title))
             (path (expand-file-name filename dir)))
        (with-temp-buffer (insert (my-org-create-project-contents title priority)) (write-file path))
        (message "Project created: %s" path)
        (let ((choice (read-char-choice
                       "Open project in [c]urrent window, [o]ther window, new [t]ab, new [s]ession, [d]on't open: "
                       '(?c ?o ?t ?s ?d))))
          (cond ((eq choice ?c) (find-file path))
                ((eq choice ?o) (find-file-other-window path))
                ((eq choice ?t) (find-file-other-tab path))
                ((eq choice ?s) (my-org-find-file-in-new-session path))
                ((eq choice ?d) nil)))))))

(defun my-org-setup-gtd-and-knowledge-management ()
  "Create GTD & Knowledge Management directory if it doesn't exist."
  (unless (file-directory-p my-org-dir)
    (copy-directory (expand-file-name "templates/org" user-emacs-directory) my-org-dir)))

(provide 'my-org)

;;; my-org.el ends here.
