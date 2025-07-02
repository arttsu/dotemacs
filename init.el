(defun my-linux-p ()
  (eq system-type 'gnu/linux))

(defun my-windows-p ()
  (eq system-type 'windows-nt))

(defun my-macos-p ()
  (eq system-type 'darwin))

(setq my-font (cond ((my-linux-p) "Liberation Mono")
                    ((my-macos-p) "Menlo")
                    ((my-windows-p) "Cascadia Code")))
(setq my-font-height 125)

(setq my-vterm-shell nil)

(setq my-use-ripgrep nil)

(setq my-use-copilot nil)

(setq my-use-aider nil)

(setq my-use-jinx nil)

(let ((path-to-local-config (expand-file-name "local.el" user-emacs-directory)))
  (if (file-exists-p path-to-local-config)
      (progn
        (load path-to-local-config)
        (message "Loaded local config."))
    (message "No local config.")))

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(use-package exec-path-from-shell
  :ensure
  :when (my-macos-p)
  :config
  (exec-path-from-shell-initialize))

(setq custom-file-path (expand-file-name "custom.el" user-emacs-directory))

(if (not (file-exists-p custom-file-path))
    (message "No custom file")
  (load custom-file-path)
  (message "Loaded custom file"))

(defun my-pop-mark ()
  (interactive)
  (set-mark-command '(4)))

(defun my-jump-home ()
  (interactive)
  (find-file "~/"))

(use-package emacs
  :custom
  (inhibit-splash-screen t)
  (initial-major-mode 'text-mode)
  (initial-scratch-message "✅ All systems go! 🚀🪐✨")
  (disabled-command-function nil)
  (save-interprogram-paste-before-kill t)
  (create-lockfiles nil)
  (make-backup-files nil)
  (custom-file custom-file-path)
  (require-final-newline t)
  (indent-tabs-mode nil)
  (visible-bell t)
  (global-auto-revert-non-file-buffers t)
  (epg-pinentry-mode 'loopback)
  :config
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (global-auto-revert-mode)
  (global-subword-mode)
  (tab-bar-mode)
  (tab-bar-history-mode)
  (repeat-mode)
  :bind
  (("C-c j x" . scratch-buffer)
   ("C-c j h" . my-jump-home)
   ("M-g w" . forward-to-word)
   ("M-g W" . backward-to-word)
   ("C-M-; d" . duplicate-dwim)
   ("C-c d h" . erase-buffer)
   ("M-z" . zap-up-to-char)
   ("M-Z" . zap-to-char)
   ("C-c e b" . eval-buffer)
   ("C-M-<return>" . tab-switch)
   ("<f8>" . my-pop-mark)
   ([down-mouse-2] . mouse-set-point)
   ([mouse-2] . delete-window)))

(use-package dired
  :custom
  (dired-dwim-target t)
  (insert-directory-program (cond ((my-windows-p) insert-directory-program)
                                  ((my-macos-p) "gls")
                                  (t "ls")))
  (dired-listing-switches (cond ((my-windows-p) dired-listing-switches)
                                (t "-alh --group-directories-first")))
  :bind
  (("<f7>" . dired-jump)
   :map dired-mode-map
   ("o" . crux-open-with)
   ("<tab>" . dired-find-file-other-window)))

(use-package dired-du
  :ensure)

(use-package project
  :config
  (add-to-list 'project-switch-commands '(project-dired "Dired" "<return>") t))

(use-package magit
  :ensure
  :bind
  (("C-c g" . magit-file-dispatch)))

(use-package flymake
  :bind
  (:map prog-mode-map
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)))

(use-package transient
  :ensure)

(use-package hydra
  :ensure)

(defun my-org-require-at-heading ()
  (unless (org-at-heading-p)
    (error "Not at a heading")))

(defun my-org-duplicate-subtree ()
  (interactive)
  (my-org-require-at-heading)
  (beginning-of-line)
  (org-copy-subtree)
  (org-paste-subtree)
  (org-delete-property "ID"))

(defun my-gtd-update-attachments-heading ()
  "Find all attachments for a project/area file and list them under a top-level * Attachments heading."
  (interactive)
  (save-excursion
    ;; Step 1: Initial validation checks
    (unless (buffer-file-name)
      (error "Not in a file-visiting buffer"))

    ;; Check if this is a project or area file
    (goto-char (point-min))
    (unless (and (org-at-heading-p)
                 (member (org-entry-get (point) "GTD_TYPE") '("project" "area")))
      (error "Not a project or area file"))

    ;; Step 2: Get the headline ID
    (let ((project-id (org-id-get)))
      (unless project-id
        (error "Project/Area headline has no ID"))

      ;; Step 3: Get the list of attachment files
      (let* ((attach-dir (org-attach-dir-from-id project-id))
             (attachments (when (and attach-dir (file-directory-p attach-dir))
                            (directory-files attach-dir t nil t)))
             ;; Filter out . and .. entries
             (attachments (when attachments
                            (cl-remove-if (lambda (f) (member (file-name-nondirectory f) '("." ".."))) attachments))))

        ;; Step 4: Sort attachments by modification time (newest first)
        (when attachments
          (setq attachments
                (mapcar #'car
                        (sort (mapcar (lambda (f) (cons f (file-attribute-modification-time (file-attributes f)))) attachments)
                              (lambda (a b) (time-less-p (cdr b) (cdr a)))))))

        ;; Step 5: Locate and manage the top-level * Attachments heading
        (let ((attachments-heading-pos nil))
          ;; Find existing top-level Attachments heading
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "^\\* Attachments$" nil t)
              (setq attachments-heading-pos (match-beginning 0))))

          ;; Handle "No Attachments" case
          (if (not attachments)
              (when attachments-heading-pos
                (goto-char attachments-heading-pos)
                (org-mark-subtree)
                (delete-region (region-beginning) (region-end)))

            ;; Handle "Has Attachments" case
            (if attachments-heading-pos
                ;; Heading exists - clear its contents
                (progn
                  (goto-char attachments-heading-pos)
                  ;; Delete everything from after the heading line to end of subtree
                  (forward-line 1)
                  (let ((content-start (point)))
                    (org-end-of-subtree t)
                    (delete-region content-start (point)))
                  ;; Move back to the heading
                  (goto-char attachments-heading-pos))
              ;; Heading doesn't exist - create it at end of buffer
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert "* Attachments\n")
              ;; We're already positioned at the heading we just created
              )

            ;; Convert absolute path to home-relative path for portability
            (let ((relative-attach-dir
                   (if (string-prefix-p (expand-file-name "~") attach-dir)
                       (concat "~" (substring attach-dir (length (expand-file-name "~"))))
                     attach-dir)))
              ;; Set ATTACH_DIR property with home-relative path
              (org-entry-put (point) "ATTACH_DIR" relative-attach-dir))

            ;; Move to end of properties drawer to insert content
            (org-end-of-meta-data)
            ;; Ensure we have exactly one newline before content
            (when (not (looking-at "^$"))
              (insert "\n"))

            ;; Insert new content
            (dolist (file-path attachments)
              (let ((filename (file-name-nondirectory file-path)))
                (insert (format "- [[attachment:%s]]\n" filename))))))))))

(defun my-gtd-auto-update-attachments-on-save ()
  "Auto-update attachments heading when saving project/area files."
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (and (org-at-heading-p)
                 (member (org-entry-get (point) "GTD_TYPE") '("project" "area"))
                 (org-id-get))
        (condition-case nil
            (my-gtd-update-attachments-heading)
          (error nil))))))

(defconst my-org-personal-dir (expand-file-name "~/org-personal"))
(defconst my-gtd-personal-dir (expand-file-name "gtd" my-org-personal-dir))
(defconst my-gtd-personal-inbox (expand-file-name "personal-inbox.org" my-gtd-personal-dir))
(defconst my-gtd-personal-areas (expand-file-name "areas" my-gtd-personal-dir))
(defconst my-gtd-personal-projects (expand-file-name "projects" my-gtd-personal-dir))
(defconst my-gtd-personal-dirs (list my-gtd-personal-dir
                                     my-gtd-personal-areas
                                     my-gtd-personal-projects))

(defconst my-org-open-dir (expand-file-name "~/org-open"))
(defconst my-gtd-open-dir (expand-file-name "gtd" my-org-open-dir))
(defconst my-gtd-open-projects (expand-file-name "projects" my-gtd-open-dir))
(defconst my-gtd-open-areas (expand-file-name "areas" my-gtd-open-dir))
(defconst my-gtd-open-dirs (list my-gtd-open-dir
                                 my-gtd-open-areas
                                 my-gtd-open-projects))

(defconst my-org-local-dir (expand-file-name "~/org-local"))
(defconst my-gtd-local-dir (expand-file-name "gtd" my-org-local-dir))
(defconst my-gtd-local-projects (expand-file-name "projects" my-gtd-local-dir))
(defconst my-gtd-local-areas (expand-file-name "areas" my-gtd-local-dir))
(defconst my-gtd-local-dirs (list my-gtd-local-dir
                                  my-gtd-local-areas
                                  my-gtd-local-projects))

(defconst my-org-shared-dir (expand-file-name "~/org-shared"))
(defconst my-gtd-shared-dir (expand-file-name "gtd" my-org-shared-dir))
(defconst my-gtd-shared-projects (expand-file-name "projects" my-gtd-shared-dir))
(defconst my-gtd-shared-areas (expand-file-name "areas" my-gtd-shared-dir))
(defconst my-gtd-shared-dirs (list my-gtd-shared-dir
                                   my-gtd-shared-areas
                                   my-gtd-shared-projects))

(defconst my-gtd-all-dirs (append my-gtd-personal-dirs
                                  my-gtd-open-dirs
                                  my-gtd-local-dirs
                                  my-gtd-shared-dirs))

(defun my-org-remove-priority-when-done ()
  (when (string= org-state "DONE")
    (ignore-errors (org-entry-put (point) "PRIORITY" nil))))

(defun my-gtd-extract-created-timestamp ()
  (or (org-entry-get (point) "CREATED")
      "[1900-01-01 Mon 00:00]"))

(defun my-gtd-extract-closed-timestamp ()
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (if (re-search-forward "CLOSED: " nil t)
          (buffer-substring-no-properties (point) (line-end-position))
        "[1900-01-01 Mon 00:00]"))))

(defun my-gtd-extract-created-timestamp-for-reverse-sort ()
  "Extract created timestamp but return in reverse alphabetical order for newest-first sorting."
  (let ((timestamp (my-gtd-extract-created-timestamp)))
    ;; Convert to a format that sorts newest first when sorted alphabetically
    ;; We'll negate the comparison by inverting all time components
    (if (string-match "\\[\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\).*\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\]" timestamp)
        (let ((year (string-to-number (match-string 1 timestamp)))
              (month (string-to-number (match-string 2 timestamp)))
              (day (string-to-number (match-string 3 timestamp)))
              (hour (string-to-number (match-string 4 timestamp)))
              (minute (string-to-number (match-string 5 timestamp))))
          ;; Return a string that sorts newest first
          (format "[%04d-%02d-%02d %02d:%02d"
                  (- 9999 year) (- 99 month) (- 99 day)
                  (- 99 hour) (- 99 minute)))
      "[0000-00-00 00:00")))

(defun my-gtd-checklist-p ()
  (let ((style (org-entry-get (point) "STYLE")))
    (string= style "checklist")))

(defun my-gtd-log-p ()
  (let ((style (org-entry-get (point) "STYLE")))
    (string= style "log")))

(defun my-gtd-checklist-do-auto-advance ()
  (let ((point-before (point)))
    (org-forward-heading-same-level 1)
    (when (= (point) point-before)
      (org-up-heading-safe))))

(defun my-gtd-checklist-auto-advance ()
  (when (and (not (eq this-command 'org-agenda-todo))
             (string= org-state "DONE"))
    (let* ((current-element (org-element-at-point))
           (parent (org-element-property :parent current-element))
           (parent-style-prop (and parent (org-entry-get parent "STYLE")))
           (parent-style (or parent-style-prop "")))
      (when (string= parent-style "checklist")
        (run-with-idle-timer 0 nil 'my-gtd-checklist-do-auto-advance)))))

(defun my-gtd-insert-note ()
  (interactive)
  (org-insert-heading-respect-content)
  (org-entry-put (point) "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
  (end-of-line))

(defun my-gtd-insert-todo ()
  (interactive)
  (org-insert-heading-respect-content)
  (insert "TODO ")
  (org-entry-put (point) "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
  (end-of-line))

(defun my-gtd-sort-todos ()
  (interactive)
  (my-org-require-at-heading)
  (org-sort-entries nil ?f 'my-gtd-extract-created-timestamp)
  (org-sort-entries nil ?f 'my-gtd-extract-closed-timestamp)
  (org-sort-entries nil ?p)
  (org-sort-entries nil ?o)
  (org-cycle)
  (org-cycle))

(defun my-gtd-sort-by-style ()
  (interactive)
  (my-org-require-at-heading)
  (let ((style (org-entry-get (point) "STYLE")))
    (cond
     ((string= style "checklist")
      (my-gtd-sort-todos))
     ((string= style "log")
      (org-sort-entries nil ?f 'my-gtd-extract-created-timestamp-for-reverse-sort)
      (org-cycle)
      (org-cycle))
     (t
      (message "No supported STYLE property found")))))

(defun my-gtd-sort-entries ()
  (interactive)
  (my-org-require-at-heading)
  (let ((style (org-entry-get (point) "STYLE")))
    (if (or (string= style "checklist") (string= style "log"))
        (my-gtd-sort-by-style)
      (org-up-heading-safe)
      (let ((parent-style (org-entry-get (point) "STYLE")))
        (when (or (string= parent-style "checklist") (string= parent-style "log"))
          (my-gtd-sort-by-style))))))

(defun my-gtd-reset-checklist ()
  (interactive)
  (my-org-require-at-heading)
  (let ((style (org-entry-get (point) "STYLE")))
    (if (not (my-gtd-checklist-p))
        (error "Not at a checklist")
      (when (yes-or-no-p "Reset the checklist?")
        (org-map-entries (lambda ()
                           (org-todo "TODO"))
                         nil
                         'tree)
        (org-todo "")))))

(defun my-gtd-complete-as-wont-do ()
  (interactive)
  (my-org-require-at-heading)
  (let ((heading (org-get-heading t t t t)))
    (org-todo 'done)
    (org-edit-headline (format "+%s+" heading))))

(defun my-gtd-create-project ()
  "Create a new GTD project file from template with interactive prompts."
  (interactive)
  (let* ((context-options '("Personal" "Open" "Local" "Shared"))
         (selected-context (completing-read "Context: "
                                            context-options
                                            nil t nil nil "Personal"))
         (projects-dir (pcase selected-context
                         ("Personal" my-gtd-personal-projects)
                         ("Open" my-gtd-open-projects)
                         ("Local" my-gtd-local-projects)
                         ("Shared" my-gtd-shared-projects)
                         (_ (error "Invalid context selected: %s" selected-context))))

         ;; Get mandatory title with validation
         (title (let ((input ""))
                  (while (string-empty-p (string-trim input))
                    (setq input (read-string "Project Title: ")))
                  input))

         ;; Get optional priority with character input
         (priority (let ((char (read-char "Priority [A-E] (or RET for default '#D'): ")))
                     (cond
                      ((= char ?\r) "[#D]")  ; Return key
                      ((and (>= char ?a) (<= char ?e)) (format "[#%c]" (upcase char)))
                      ((and (>= char ?A) (<= char ?E)) (format "[#%c]" char))
                      (t "[#D]"))))  ; Default for any other input

         ;; Generate automatic data
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (file-timestamp (format-time-string "%Y-%m-%d"))
         (slug (replace-regexp-in-string
                "[^a-z0-9-]" ""
                (replace-regexp-in-string
                 "\\s-+" "-"
                 (downcase title))))
         (filename (concat file-timestamp "-" slug ".org"))
         (full-file-path (file-name-concat projects-dir filename))
         (org-id (org-id-new))

         ;; Read template from file and populate with data
         (template-content (with-temp-buffer
                             (insert-file-contents (my-org-capture-template-path "gtd-project"))
                             (buffer-string)))
         (template (format template-content priority title org-id timestamp)))

    ;; Check for file existence and write file
    (when (and (file-exists-p full-file-path)
               (not (y-or-n-p (format "File %s exists. Overwrite? " filename))))
      (user-error "File creation cancelled"))

    ;; Write template to file
    (write-region template nil full-file-path)

    ;; Open file and register org-id location
    (find-file full-file-path)
    (goto-char (point-min))
    (when (re-search-forward ":ID:" nil t)
      (org-id-get-create))  ; This registers the ID location

    ;; Ask about easysession switch
    (when (and (fboundp 'easysession-save-as)
               (y-or-n-p "Switch to a new easysession for this project? "))
      (let ((session-name slug))  ; Use normalized project name without timestamp
        ;; Switch to previous buffer before session change
        (when (> (length (buffer-list)) 1)
          (switch-to-buffer (other-buffer (current-buffer) t)))

        ;; Save current session and create new session
        (easysession-save-as session-name)

        ;; Clean up tabs - delete all other tabs and rename current to 'notes'
        (when (and (boundp 'tab-bar-mode) tab-bar-mode)
          (tab-bar-close-other-tabs)
          (tab-bar-rename-tab "notes"))

        ;; Ensure the project file is open in the new session
        (find-file full-file-path)))

    ;; Final confirmation
    (message "Project file created: %s" full-file-path)))

(defun my-gtd-create-area ()
  "Create a new GTD area file from template with interactive prompts."
  (interactive)
  (let* ((context-options '("Personal" "Open" "Local" "Shared"))
         (selected-context (completing-read "Context: "
                                            context-options
                                            nil t nil nil "Personal"))
         (areas-dir (pcase selected-context
                      ("Personal" my-gtd-personal-areas)
                      ("Open" my-gtd-open-areas)
                      ("Local" my-gtd-local-areas)
                      ("Shared" my-gtd-shared-areas)
                      (_ (error "Invalid context selected: %s" selected-context))))

         ;; Get mandatory title with validation
         (title (let ((input ""))
                  (while (string-empty-p (string-trim input))
                    (setq input (read-string "Area Title: ")))
                  input))

         ;; Generate automatic data
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (slug (replace-regexp-in-string
                "[^a-z0-9-]" ""
                (replace-regexp-in-string
                 "\\s-+" "-"
                 (downcase title))))
         (filename (concat slug ".org"))
         (full-file-path (file-name-concat areas-dir filename))
         (org-id (org-id-new))

         ;; Read template from file and populate with data
         (template-content (with-temp-buffer
                             (insert-file-contents (my-org-capture-template-path "gtd-area"))
                             (buffer-string)))
         (template (format template-content title org-id timestamp)))

    ;; Check for file existence and write file
    (when (and (file-exists-p full-file-path)
               (not (y-or-n-p (format "File %s exists. Overwrite? " filename))))
      (user-error "File creation cancelled"))

    ;; Write template to file
    (write-region template nil full-file-path)

    ;; Open file and register org-id location
    (find-file full-file-path)
    (goto-char (point-min))
    (when (re-search-forward ":ID:" nil t)
      (org-id-get-create))  ; This registers the ID location

    ;; Final confirmation
    (message "Area file created: %s" full-file-path)))

(defun my-gtd-archive-project ()
  "Archive a GTD project file by moving it to the archive subdirectory."
  (interactive)
  ;; Step 1: Safety checks
  (unless (eq major-mode 'org-mode)
    (error "This is not an Org project file"))

  ;; Check if this is a project file
  (save-excursion
    (goto-char (point-min))
    (unless (and (org-at-heading-p)
                 (string= "project" (org-entry-get (point) "GTD_TYPE")))
      (error "This is not an Org project file")))

  ;; Confirm with user
  (when (y-or-n-p "Archive this project? ")
    ;; Step 2: Determine and prepare archive file path
    (let* ((current-file-path (buffer-file-name))
           (current-dir (file-name-directory current-file-path))
           (filename (file-name-nondirectory current-file-path))
           (archive-dir (file-name-concat current-dir "archive"))
           (new-file-path (file-name-concat archive-dir filename)))

      ;; Create archive directory if it doesn't exist
      (make-directory archive-dir t)

      ;; Step 3: Remove priority cookie from headline
      (save-excursion
        (goto-char (point-min))
        (org-entry-put (point) "PRIORITY" nil))

      ;; Step 4: Set ARCHIVED property
      (save-excursion
        (goto-char (point-min))
        (org-entry-put (point) "ARCHIVED" (format-time-string "[%Y-%m-%d %a %H:%M]")))

      ;; Step 5: Move file and update Org ID locations
      (save-buffer)
      (rename-file current-file-path new-file-path)
      (org-id-update-id-locations (list new-file-path))
      (set-visited-file-name new-file-path)
      (set-buffer-modified-p nil)

      ;; Final confirmation
      (message "Project archived to %s" new-file-path))))

(defun my-gtd-copy-heading-to-area (&optional prefix)
  "Copy the current heading's subtree to its linked area file and tag as :COPIED:.
With prefix argument, or when no AREA link exists, prompt to select an area file."
  (interactive "P")
  (save-excursion
    ;; Step 1: Initial Context Validation
    (unless (org-at-heading-p)
      (error "Not at a heading"))

    (when (member "COPIED" (org-get-tags))
      (error "Heading has already been copied"))

    ;; Step 2: Determine if we need to select an area
    (let* ((area-link nil)
           (project-title nil)
           (project-id nil)
           (area-file-path nil)
           (current-file-path (buffer-file-name))
           (current-dir (file-name-directory current-file-path))
           (is-personal-project (string-match-p "/org-personal/" current-file-path))
           (is-open-project (string-match-p "/org-open/" current-file-path))
           (is-local-project (string-match-p "/org-local/" current-file-path))
           (is-shared-project (string-match-p "/org-shared/" current-file-path))
           (areas-dir (cond (is-personal-project my-gtd-personal-areas)
                            (is-open-project my-gtd-open-areas)
                            (is-local-project my-gtd-local-areas)
                            (is-shared-project my-gtd-shared-areas)
                            (t (error "Project file is not in a recognized GTD directory"))))
           (force-select-area prefix))

      ;; Try to find existing AREA link if not forcing selection
      (unless force-select-area
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^AREA: *\\(\\[\\[id:[^]]+\\]\\(?:\\[.*?\\]\\)?\\]\\)" nil t)
            (setq area-link (match-string 1)))))

      ;; If no area link found or prefix used, prompt for area selection
      (when (or force-select-area (not area-link))
        (let* ((area-files (directory-files areas-dir t "\\.org$"))
               (area-choices (mapcar (lambda (file)
                                       (cons (file-name-base file) file))
                                     area-files))
               (selected-area (completing-read "Select area: " area-choices nil t)))
          (setq area-file-path (cdr (assoc selected-area area-choices)))))

      ;; Find Project Info
      (save-excursion
        (goto-char (point-min))
        (setq project-title (org-get-heading t t t t))  ; Remove priority and tags
        (setq project-id (org-id-get))
        (unless project-id
          (error "Project's top-level heading has no ID")))

      ;; Step 3: Copy Subtree and Prepare for Destination
      (org-copy-subtree 1)

      ;; If we have an area-link, resolve it to file path
      (when (and area-link (not area-file-path))
        (let ((id-match (string-match "id:\\([^]]+\\)" area-link)))
          (unless id-match
            (error "Could not extract ID from area link: %s" area-link))
          (let ((area-id (match-string 1 area-link)))
            (setq area-file-path (org-id-find-id-file area-id))
            (unless area-file-path
              (error "Could not find file for area link: %s" area-link)))))

      ;; Step 4: Modify the Area File
      (with-current-buffer (find-file-noselect area-file-path)
        (goto-char (point-min))
        (if (re-search-forward "^\\* Moved project items$" nil t)
            ;; Heading found - move to end of its subtree
            (org-end-of-subtree t)
          ;; Heading not found - create it at end of buffer
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert "* Moved project items\n"))

        ;; Insert spacing and paste the subtree
        (insert "\n")
        (yank)

        ;; Move to the pasted heading and add traceability link
        (org-back-to-heading t)
        (org-end-of-meta-data t)
        (insert (format "COPIED_FROM: [[id:%s][%s]]\n" project-id project-title))

        ;; Save the area file
        (save-buffer))

      ;; Step 5: Finalize the Source Project File
      (org-set-tags (append (org-get-tags) '("COPIED")))

      ;; Mark TODO as DONE if it's not already done
      (let ((todo-state (org-get-todo-state)))
        (when (and todo-state (not (member todo-state org-done-keywords)))
          (org-todo 'done)))

      (save-buffer)

      ;; Step 6: Display Confirmation
      (message "Copied heading to '%s' and tagged as :COPIED:" (file-name-nondirectory area-file-path)))))

(defun my-org-capture-template-path (name)
  (expand-file-name (concat "capture-templates/" name ".txt") user-emacs-directory))

(defconst my-gtd-personal-inbox-target `(file+headline ,my-gtd-personal-inbox "Inbox items"))

(defconst my-gtd-capture-templates
  `(("i" "Inbox")
    ("ii" "note" entry ,my-gtd-personal-inbox-target (file ,(my-org-capture-template-path "gtd-note")))
    ("il" "note link" entry ,my-gtd-personal-inbox-target (file ,(my-org-capture-template-path "gtd-note-link")))
    ("iI" "todo" entry ,my-gtd-personal-inbox-target (file ,(my-org-capture-template-path "gtd-todo")))
    ("iL" "todo link" entry ,my-gtd-personal-inbox-target (file ,(my-org-capture-template-path "gtd-todo-link")))))

(defun my-gtd-capture-note (&optional prefix)
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "ii"))
        ((equal prefix '(4)) (org-capture nil "il"))
        (t (error "Invalid prefix argument: %s" prefix))))

(defun my-gtd-capture-todo (&optional prefix)
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "iI"))
        ((equal prefix '(4)) (org-capture nil "iL"))
        (t (error "Invalid prefix argument: %s" prefix))))

(defun my-gtd-day-agenda-skip-todo-p ()
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (scheduled (org-get-scheduled-time (point)))
        (deadline (org-get-deadline-time (point)))
        (priority (org-get-priority (thing-at-point 'line t))))
    (unless (or scheduled
                deadline
                (< priority 1000)
                (and (< priority 3000)
                     (string= (org-entry-get (point) "GTD_TYPE") "project"))
                (and (< priority 3000)
                     (string= (org-entry-get (point) "GTD_TYPE") "area")))
      subtree-end)))

(defun my-gtd-day-agenda-skip-project-p ()
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (priority (org-get-priority (thing-at-point 'line t))))
    (when (< priority 1000)
      subtree-end)))

(defconst my-gtd-day-agenda
  `("d" "Day" ((agenda "" ((org-agenda-span 1)
                           (org-agenda-skip-scheduled-if-done t)
                           (org-agenda-skip-deadline-if-done t)
                           (org-agenda-skip-timestamp-if-done t)))
               (todo "TODO" ((org-agenda-overriding-header "Ad-hoc and high-prio project tasks")
                             (org-agenda-skip-function 'my-gtd-day-agenda-skip-todo-p)
                             (org-agenda-files ',my-gtd-personal-dirs)))
               (tags "GTD_TYPE=\"project\"" ((org-agenda-overriding-header "Projects")
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-sorting-strategy '(priority-down))
                                 (org-agenda-skip-function 'my-gtd-day-agenda-skip-project-p)
                                 (org-agenda-files '(,my-gtd-personal-projects))))
               (todo "TODO" ((org-agenda-overriding-header "Open ad-hoc and high-prio project tasks")
                             (org-agenda-skip-function 'my-gtd-day-agenda-skip-todo-p)
                             (org-agenda-files ',my-gtd-open-dirs)))
               (tags "GTD_TYPE=\"project\"" ((org-agenda-overriding-header "Open projects")
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-sorting-strategy '(priority-down))
                                 (org-agenda-skip-function 'my-gtd-day-agenda-skip-project-p)
                                 (org-agenda-files '(,my-gtd-open-projects))))
               (todo "TODO" ((org-agenda-overriding-header "Local ad-hoc and high-prio project tasks")
                             (org-agenda-skip-function 'my-gtd-day-agenda-skip-todo-p)
                             (org-agenda-files ',my-gtd-local-dirs)))
               (tags "GTD_TYPE=\"project\"" ((org-agenda-overriding-header "Local projects")
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-sorting-strategy '(priority-down))
                                 (org-agenda-skip-function 'my-gtd-day-agenda-skip-project-p)
                                 (org-agenda-files '(,my-gtd-local-projects))))
               (todo "TODO" ((org-agenda-overriding-header "Shared ad-hoc and high-prio project tasks")
                             (org-agenda-skip-function 'my-gtd-day-agenda-skip-todo-p)
                             (org-agenda-files ',my-gtd-shared-dirs)))
               (tags "GTD_TYPE=\"project\"" ((org-agenda-overriding-header "Shared projects")
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-sorting-strategy '(priority-down))
                                 (org-agenda-skip-function 'my-gtd-day-agenda-skip-project-p)
                                 (org-agenda-files '(,my-gtd-shared-projects)))))))

(defun my-org-agenda-category-short ()
  "Return a shortened category name, stripping timestamp from filename if present."
  (let* ((file-name (file-name-base (buffer-file-name)))
         ;; Strip timestamp pattern YYYY-MM-DD- from beginning
         (cleaned-name (replace-regexp-in-string "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-" "" file-name))
         ;; Limit to 19 characters and capitalize first letter
         (short-name (if (> (length cleaned-name) 19)
                         (substring cleaned-name 0 19)
                       cleaned-name)))
    (if (string-empty-p short-name) 
        (substring file-name 0 (min 19 (length file-name)))
      (capitalize short-name))))

(defun my-org-setup ()
  (setq-local fill-column 120)
  (auto-fill-mode 1))

(use-package org
  :ensure
  :custom
  (org-startup-folded 'showall)
  (org-hide-drawer-startup nil)
  (org-startup-indented nil)
  (org-startup-with-inline-images t)
  (org-confirm-babel-evaluate nil)
  (org-use-sub-superscripts '{})
  (org-src-window-setup 'split-window-below)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-use-speed-commands t)
  (org-log-done 'time)
  (org-priority-lowest 69)
  (org-priority-default 68)
  (org-capture-templates my-gtd-capture-templates)
  (org-agenda-files my-gtd-all-dirs)
  (org-agenda-custom-commands `(,my-gtd-day-agenda))
  (org-agenda-prefix-format '((agenda . " %i %-20(my-org-agenda-category-short) %?-12t% s")
                               (todo . " %i %-20(my-org-agenda-category-short) ")
                               (tags . " %i %-20(my-org-agenda-category-short) ")
                               (search . " %i %-20(my-org-agenda-category-short) ")))
  (org-refile-targets '((org-agenda-files :level . 2)))
  (org-attach-id-dir (expand-file-name "attachments" my-org-personal-dir))
  (org-attach-use-inheritance t)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-habit-graph-column 80)
  (org-habit-show-done-always-green t)
  :config
  (require 'org-attach)
  (require 'org-id)
  (require 'org-habit)
  (add-hook 'org-mode-hook 'my-org-setup)
  (add-hook 'org-after-todo-state-change-hook 'my-org-remove-priority-when-done)
  (add-hook 'org-after-todo-state-change-hook 'my-gtd-checklist-auto-advance)
  (add-hook 'org-after-refile-insert-hook 'my-gtd-sort-entries)
  (add-hook 'before-save-hook 'my-gtd-auto-update-attachments-on-save)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t) (clojure . t)))
  :bind
  (("C-c c" . org-capture)
   ("C-c i" . my-gtd-capture-note)
   ("C-c I" . my-gtd-capture-todo)
   ("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c o p" . my-gtd-create-project)
   ("C-c o A" . my-gtd-create-area)
   :map org-mode-map
   ("C-c o s" . my-gtd-sort-entries)
   ("C-c o S" . my-gtd-sort-todos)
   ("C-c o r" . my-gtd-reset-checklist)
   ("C-c o i" . my-gtd-insert-note)
   ("C-c o I" . my-gtd-insert-todo)
   ("C-c o d" . my-org-duplicate-subtree)
   ("C-c o x" . my-gtd-complete-as-wont-do)
   ("C-c o Z" . my-gtd-archive-project)
   ("C-c o c" . my-gtd-copy-heading-to-area)
   ("C-c o C-i"  . org-id-get-create)
   ("C-c o a" . my-gtd-update-attachments-heading)))

(use-package modus-themes
  :ensure
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-headings '((0 . (ultrabold 1.7))
                           (1 . (ultrabold 1.7))
                           (2 . (extrabold 1.5))
                           (3 . (extrabold 1.3))
                           (4 . (extrabold 1.1))
                           (t . (extrabold))))
  (modus-themes-variable-pitch-ui t)
  :config
  (modus-themes-load-theme 'modus-vivendi))

(set-face-attribute 'default nil :font my-font :height my-font-height)
(set-frame-font my-font nil t)

(when (my-windows-p)
  (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'append))

(use-package ace-window
  :ensure
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (aw-scope 'frame)
  :bind
  (("M-o" . ace-window)))

(use-package vertico
  :ensure
  :demand
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))
  (add-to-list 'vertico-multiform-commands
               '(consult-ripgrep buffer indexed))
  :bind
  (:map vertico-map
        ("C-;" . vertico-quick-insert)))

(use-package savehist
  :config
  (savehist-mode))

(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure
  :config
  ;; Don't live preview buffers to avoid triggering package loading (esp. Org).
  ;; See https://github.com/minad/consult#live-previews
  (consult-customize consult-buffer :preview-key "M-.")
  :bind
  (("C-x b" . consult-buffer)
   ("C-x B" . consult-buffer-other-tab)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-s l" . consult-line)
   ("M-s k" . consult-keep-lines)
   ("M-s f" . consult-focus-lines)
   ("M-s r" . consult-ripgrep)
   :map prog-mode-map
   ("C-c ! !" . consult-flymake)))

(use-package corfu
  :ensure
  :demand
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  :config
  (global-corfu-mode)
  (corfu-history-mode)
  :bind
  (:map corfu-map
        ("C-SPC" . corfu-insert-separator)
        ("C-;" . corfu-quick-insert)))

(use-package super-save
  :ensure
  :custom
  (super-save-all-buffers t)
  (super-save-auto-save-when-idle t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-silent t)
  (super-save-exclude '(".sbt" "project/"
                        ".gpg"))
  (auto-save-default nil)
  :config
  (super-save-mode))

(defun my-easysession-visible-buffer-list ()
  (let ((visible-buffers '()))
    (dolist (buffer (buffer-list))
      (when (or (get-buffer-window buffer 'visible)
                (and (bound-and-true-p tab-bar-mode)
                     (fboundp 'tab-bar-get-buffer-tab)
                     (tab-bar-get-buffer-tab buffer t nil)))
        (push buffer visible-buffers)))
    visible-buffers))

(defun my-easysession-setup-minimal ()
  (when (and (boundp 'tab-bar-mode) tab-bar-mode)
    (tab-bar-close-other-tabs)
    (tab-bar-rename-tab ""))
  (delete-other-windows)
  (scratch-buffer))

(defun my-easysession-reset-session ()
  (interactive)
  (when (yes-or-no-p "Reset session?")
    (my-easysession-setup-minimal)))

(use-package easysession
  :ensure
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)
  :custom
  (easysession-mode-line-misc-info t)
  (easysession-save-interval (* 5 60))
  (easysession-buffer-list-function 'my-easysession-visible-buffer-list)
  (easysession-switch-to-exclude-current t)
  :init
  (add-hook 'emacs-startup-hook 'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook 'easysession-save-mode 103)
  :config
  (add-hook 'easysession-new-session-hook 'my-easysession-setup-minimal)
  (add-hook 'easysession-before-load-hook 'easysession-save)
  :bind
  (("<f12> <f12>" . easysession-switch-to)
   ("<f12> s" . easysession-save)
   ("<f12> k" . easysession-delete)
   ("<f12> r" . my-easysession-reset-session)))

(use-package link-hint
  :ensure
  :bind
  (("C-c f" . link-hint-open-link)
   ("C-c y" . link-hint-copy-link)))

(use-package embark
  :ensure
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

(use-package embark-consult
  :ensure
  :after (embark consult))

(use-package smartparens
  :ensure
  :config
  (require 'smartparens-config)
  :hook
  (prog-mode . smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("C-<right>" . sp-forward-slurp-sexp)
        ("C-<left>" . sp-backward-slurp-sexp)
        ("M-<right>" . sp-forward-barf-sexp)
        ("M-<left>" . sp-backward-barf-sexp)
        ("M-a" . sp-beginning-of-sexp)
        ("M-e" . sp-end-of-sexp)
        ("C-M-u" . sp-up-sexp)
        ("C-M-S-u" . sp-backward-up-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-S-d" . sp-backward-down-sexp)
        ("C-c p u" . sp-unwrap-sexp)
        ("C-c p (" . sp-wrap-round)
        ("C-c p [" . sp-wrap-square)
        ("C-c p {" . sp-wrap-curly)
        ("C-c p r" . sp-rewrap-sexp)))

(use-package crux
  :ensure
  :bind
  (("C-o" . crux-smart-open-line)
   ("C-S-o" . crux-smart-open-line-above)
   ("C-^" . crux-top-join-line)
   ("C-M-; D" . crux-duplicate-and-comment-current-line-or-region)))

(use-package whole-line-or-region
  :ensure
  :demand
  :config
  (whole-line-or-region-global-mode)
  :bind
  (("M-/" . whole-line-or-region-comment-dwim)))

(defun my-avy-embark-with-save-excursion (f)
  (let ((point-before (point)))
    (goto-char target-point)
    (let ((buffer-before-embark (buffer-name (current-buffer)))
          (point-before-embark (point)))
      (funcall f)
      (when (and (string= buffer-before-embark (buffer-name (current-buffer)))
                 (= point-before-embark (point)))
        (goto-char point-before)))))

(defun my-avy-embark-act (target-point)
  (my-avy-embark-with-save-excursion 'embark-act))

(defun my-avy-embark-dwim (target-point)
  (my-avy-embark-with-save-excursion 'embark-dwim))

(use-package avy
  :custom
  (avy-single-candidate-jump t)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'my-avy-embark-act)
  (setf (alist-get ?\; avy-dispatch-alist) 'my-avy-embark-dwim)
  :bind
  (("C-;" . avy-goto-char-timer)
   ("M-;" . avy-pop-mark)
   ("M-g g" . avy-goto-line)
   ("M-g G" . avy-goto-end-of-line)
   ("M-g h" . avy-org-goto-heading-timer)
   ("M-g s" . avy-goto-word-1)
   ("C-M-; c" . avy-copy-line)
   ("C-M-; C" . avy-copy-region)
   ("C-M-; m" . avy-move-line)
   ("C-M-; M" . avy-move-region)
   ("C-M-; k" . avy-kill-whole-line)
   ("C-M-; K" . avy-kill-region)
   ("C-M-; s" . avy-kill-ring-save-whole-line)
   ("C-M-; S" . avy-kill-ring-save-region)
   :map isearch-mode-map
   ("C-;" . avy-isearch)))

(use-package multiple-cursors
  :ensure
  :bind
  (("C-+" . mc/mark-next-like-this)
   ("C-c k l" . mc/edit-lines)
   ("C-c k m" . mc/mark-all-dwim)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-<return>" . set-rectangular-region-anchor)))

(use-package expand-region
  :ensure
  :bind
  (("C-=" . er/expand-region)))

(use-package iy-go-to-char
  :ensure (:host github :repo "arttsu/iy-go-to-char")
  :bind
  (("M-g f" . iy-go-to-char)
   ("M-g F" . iy-go-to-char-backward)
   ("M-g t" . iy-go-up-to-char)
   ("M-g T" . iy-go-up-to-char-backward)
   ("M-g ;" . iy-go-to-or-up-to-continue)
   ("M-g ," . iy-go-to-or-up-to-continue-backward)))

(defun tempel-include (elt)
  (when (eq (car-safe elt) 'i)
    (if-let (template (alist-get (cadr elt) (tempel--templates)))
        (cons 'l template)
      (message "Template %s not found" (cadr elt))
      nil)))

(use-package tempel
  :ensure
  :demand
  :custom
  (tempel-trigger-prefix "<")
  :init
  (defun my-tempel-setup-capf()
    (setq-local completion-at-point-functions (cons 'tempel-complete completion-at-point-functions)))
  (add-hook 'text-mode-hook 'my-tempel-setup-capf)
  (add-hook 'conf-mode-hook 'my-tempel-setup-capf)
  (add-hook 'prog-mode-hook 'my-tempel-setup-capf)
  :config
  (tempel-key "C-c t f" fun emacs-lisp-mode-map)
  (tempel-key "C-c t t" today)
  (tempel-key "C-c t T" now)
  (add-to-list 'tempel-user-elements #'tempel-include)
  :bind
  (("M-+" . tempel-insert)))

(use-package org-auto-tangle
  :ensure
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-modern
  :ensure
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  (org-modern-priority-faces '((?A :background "Firebrick3" :foreground "White")
                               (?B :background "Goldenrod" :foreground "White")
                               (?C :background "SeaGreen" :foreground "White")
                               (?D :background "MediumOrchid" :foreground "White")
                               (?E :background "Seashell3" :foreground "Black")))
  (org-modern-table nil)
  :custom-face
  (org-modern-tag ((t (:foreground "White" :background "CornflowerBlue" :slant italic))))
  :config
  (global-org-modern-mode))

(use-package ob-restclient
  :ensure
  :after org)

(use-package org-node
  :ensure
  :custom
  (org-mem-do-sync-with-org-id t)
  (org-mem-watch-dirs (list my-org-personal-dir my-org-open-dir my-org-local-dir my-org-shared-dir))
  :config
  (org-mem-updater-mode)
  (org-node-cache-mode)
  (org-node-backlink-mode)
  :bind
  (("M-s M-f" . org-node-find)
   ("M-s M-i" . org-node-insert-link)))

(use-package ox-gfm
  :ensure
  :after org)

(defun my-vterm-unbind-keys ()
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
  (local-unset-key (kbd "<f12>")))

(defun my-vterm--new-buffer-and-switch (path buffer-name)
  (let ((default-directory path))
    (vterm buffer-name)
    (switch-to-buffer buffer-name)))

(defun my-vterm-project (&optional prefix)
  (interactive "P")
  (if-let ((project-root-path (when (project-current) (project-root (project-current)))))
      (let ((target-buffer-name (format "*%s: vterm*" (project-name (project-current)))))
        (if (get-buffer target-buffer-name)
            (if prefix
                (let ((new-buffer-name (generate-new-buffer-name target-buffer-name)))
                  (my-vterm--new-buffer-and-switch project-root-path new-buffer-name))
              (switch-to-buffer target-buffer-name))
          (my-vterm--new-buffer-and-switch project-root-path target-buffer-name)))))

(use-package vterm
  :when my-vterm-shell
  :ensure
  :demand
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-shell my-vterm-shell)
  (vterm-max-scrollback 50000)
  (vterm-clear-scrollback-when-clearing t)
  :config
  (add-hook 'vterm-mode-hook 'my-vterm-unbind-keys)
  (add-to-list 'project-switch-commands '(my-vterm-project "Vterm" "V") t)
  :bind
  (("C-x v" . vterm)
   ("C-x 4 v" . vterm-other-window)
   ("C-x p v" . my-vterm-project)))

(use-package eat
  :ensure
  :custom
  (eat-kill-buffer-on-exit t)
  :bind
  (("C-x e" . eat)
   ("C-x 4 e" . eat-other-window)
   ("C-x p e" . eat-project)
   ("C-x p 4 e" . eat-project-other-window)))

(use-package rg
  :when my-use-ripgrep
  :after project
  :ensure
  :bind
  (("M-s R" . rg)
   ("C-x p g" . rg-project)))

(defun my-gptel-auto-fill-response (begin end)
  (when (and (not (eq begin end)) (eq major-mode 'org-mode))
    (save-excursion
      (goto-char begin)
      (while (not (>= (point) end))
        (org-forward-sentence)
        (let ((elem (org-element-at-point)))
          (when (member (org-element-type elem) '(paragraph item))
            (save-excursion
              (goto-char (org-element-property :begin elem))
              (fill-paragraph))))))))

(defun my-gptel-clear-buffer ()
  (interactive)
  (erase-buffer)
  (insert "*** "))

(defun my-gptel-send ()
  (interactive)
  (goto-char (point-max))
  (gptel-send)
  (org-back-to-heading)
  (recenter-top-bottom 0))

(define-derived-mode my-gptel-mode org-mode "GPTel")

(defun my-gptel-mode-setup ()
  (interactive)
  (org-mode)
  (gptel-mode))

(use-package gptel
  :ensure
  :demand
  :custom
  (gptel-model 'gpt-4o)
  (gptel-default-mode 'org-mode)
  :config
  (add-hook 'gptel-mode-hook 'toggle-truncate-lines)
  (add-hook 'gptel-post-response-functions 'my-gptel-auto-fill-response)
  (add-hook 'my-gptel-mode-hook 'my-gptel-mode-setup)
  (add-to-list 'auto-mode-alist '("\\.gptel\\'" . my-gptel-mode))
  :bind
  (("C-c SPC" . gptel)
   :map gptel-mode-map
   ("C-c C-c" . my-gptel-send)
   ("C-c k" . gptel-abort)
   ("C-c d h" . my-gptel-clear-buffer)))

(use-package emms
  :ensure
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-player-mpv-update-metadata t)
  (emms-streams-file (expand-file-name "streams.emms" user-emacs-directory))
  :config
  (emms-all)
  :bind
  (("C-c r r" . emms-streams)
   ("C-c r p" . emms-pause)
   ("C-c r k" . emms-stop)))

(defun my-ledger--bal-period (period)
  (format "%%(binary) -f %%(ledger-file) --invert --period \"%s\" -S amount bal ^Income ^Expenses" period))

(use-package ledger-mode
  :ensure
  :custom
  (ledger-default-date-format "%Y-%m-%d")
  :config
  (ledger-reports-add "bal-this-month" (my-ledger--bal-period "this month"))
  (ledger-reports-add "bal-last-month" (my-ledger--bal-period "last month")))

(use-package kubel
  :ensure
  :unless (my-windows-p)
  :after vterm
  :config
  (add-to-list 'vterm-tramp-shells '("kubectl" "/bin/bash"))
  :bind
  (("C-c K" . kubel)
   :map kubel-mode-map
   ("n" . next-line)
   ("p" . previous-line)
   ("N" . kubel-set-namespace)
   ("v" . kubel-exec-vterm-pod)
   ("P" . kubel-port-forward-pod)))

(use-package jinx
  :ensure
  :when my-use-jinx
  :custom (jinx-languages "en_US de_DE ru")
  :hook (emacs-startup . global-jinx-mode)
  :bind
  (("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages)))

(use-package envrc
  :ensure
  :hook (elpaca-after-init . envrc-global-mode))

(use-package tree-sitter
  :ensure
  :unless (my-windows-p)
  :custom
  (treesit-font-lock-level 4))

(defun lsp-corfu-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))

(use-package lsp-mode
  :ensure
  :custom
  (lsp-keymap-prefix "<f5>")
  (lsp-pylsp-plugins-black-enabled t)
  (lsp-completion-provider :none)
  :hook
  (python-mode . lsp)
  (python-ts-mode . lsp)
  (lsp-mode . lsp-corfu-setup)
  :commands lsp
  :bind
  (:map
   lsp-mode-map
   ([M-down-mouse-1] . mouse-set-point)
   ([M-mouse-1] . lsp-find-definition)
   ([M-mouse-3] . xref-go-back)))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode)

(use-package pet
  :ensure
  :after envrc
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  (require 'envrc)
  (add-hook 'change-major-mode-after-body-hook 'envrc-mode))

(use-package jarchive
  :ensure
  :unless (my-windows-p)
  :config
  (jarchive-setup))

(use-package copilot
  :ensure
  :when my-use-copilot
  :after hydra
  :custom
  (copilot-idle-delay 0.3)
  :custom-face
  (copilot-overlay-face ((t (:foreground "DarkOrchid1" :slant italic))))
  :config
  (add-to-list 'warning-suppress-log-types '(copilot copilot-no-mode-indent))
  (defhydra my-copilot-accept-completion (copilot-mode-map "C-M-<tab>")
    "Accept Copilot completion"
    ("C-M-<tab>" copilot-accept-completion "Accept" :color blue)
    ("C-M-f" copilot-accept-completion-by-word "By word")
    ("C-M-e" copilot-accept-completion-by-line "By line"))
  :hook
  (prog-mode . copilot-mode))

(use-package scala-ts-mode
  :ensure
  :interpreter "scala"
  :mode "\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'"
  :hook (scala-ts-mode . lsp))

(use-package lsp-metals
  :ensure
  :after (lsp-mode scala-ts-mode)
  :bind
  (:map scala-ts-mode-map
        ("<f5> I" . lsp-metals-build-import)))

(use-package graphql-mode
  :ensure)

(use-package dockerfile-mode
  :ensure)

(use-package aider
  :ensure
  :when my-use-aider
  :config
  (setq aider-args '("--model" "litellm_proxy/gemini-25-pro"))
  (global-set-key (kbd "C-c b") 'aider-transient-menu))

(use-package clojure-mode
  :ensure)

(use-package cider
  :ensure
  :custom
  (cider-jack-in-default 'babashka))

(use-package fish-mode
  :ensure)

(use-package markdown-mode
  :ensure
  :interpreter "markdown")

(use-package yaml-mode
  :ensure)

(use-package just-mode
  :ensure)
