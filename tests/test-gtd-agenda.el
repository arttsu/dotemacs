;;; test-gtd-agenda.el --- Tests for GTD day agenda functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for the GTD day agenda system
;; Tests filtering, organization, and display of agenda items

;;; Code:

(require 'ert)
(require 'org)
(require 'org-agenda)
(require 'test-gtd-utils)

;;; Test Fixtures

(defvar gtd-test-agenda-temp-dir nil
  "Temporary directory for agenda test files.")

(defvar gtd-test-agenda-orig-vars nil
  "Original values of GTD directory variables.")

;; Define the agenda builder function for testing
;; (This mirrors the one in config.org but is available for tests)
(defun my-gtd-build-day-agenda (local-dir local-areas local-projects shared-dir shared-areas shared-projects)
  "Build day agenda command with specified directories."
  `("d" "Day" ((agenda "" ((org-agenda-span 1)
                           (org-agenda-skip-scheduled-if-done t)
                           (org-agenda-skip-deadline-if-done t)
                           (org-agenda-skip-timestamp-if-done t)))
               (todo "TODO" ((org-agenda-overriding-header "Local ad-hoc and high-prio project tasks")
                             (org-agenda-skip-function 'my-gtd-day-agenda-skip-todo-p)
                             (org-agenda-files '(,local-dir ,local-areas ,local-projects))))
               (tags "GTD_TYPE=\"project\"" ((org-agenda-overriding-header "Local projects")
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-sorting-strategy '(priority-down))
                                 (org-agenda-skip-function 'my-gtd-day-agenda-skip-project-p)
                                 (org-agenda-files '(,local-projects))))
               (todo "TODO" ((org-agenda-overriding-header "Shared ad-hoc and high-prio project tasks")
                             (org-agenda-skip-function 'my-gtd-day-agenda-skip-todo-p)
                             (org-agenda-files '(,shared-dir ,shared-areas ,shared-projects))))
               (tags "GTD_TYPE=\"project\"" ((org-agenda-overriding-header "Shared projects")
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-sorting-strategy '(priority-down))
                                 (org-agenda-skip-function 'my-gtd-day-agenda-skip-project-p)
                                 (org-agenda-files '(,shared-projects)))))))

(defun gtd-test-agenda-setup-directories ()
  "Set up temporary directories and files for agenda testing."
  (setq gtd-test-agenda-temp-dir (make-temp-file "gtd-agenda-test" t))

  ;; Store original values
  (setq gtd-test-agenda-orig-vars
        `((my-gtd-local-dir . ,my-gtd-local-dir)
          (my-gtd-local-projects . ,my-gtd-local-projects)
          (my-gtd-local-areas . ,my-gtd-local-areas)
          (my-gtd-local-inbox . ,my-gtd-local-inbox)
          (my-gtd-shared-dir . ,my-gtd-shared-dir)
          (my-gtd-shared-projects . ,my-gtd-shared-projects)
          (my-gtd-shared-areas . ,my-gtd-shared-areas)
          (my-gtd-all-dirs . ,my-gtd-all-dirs)
          (org-agenda-files . ,org-agenda-files)
          (org-agenda-custom-commands . ,org-agenda-custom-commands)))

  ;; Override GTD directory constants for testing
  (setq my-gtd-local-dir (expand-file-name "local" gtd-test-agenda-temp-dir))
  (setq my-gtd-local-projects (expand-file-name "projects" my-gtd-local-dir))
  (setq my-gtd-local-areas (expand-file-name "areas" my-gtd-local-dir))
  (setq my-gtd-local-inbox (expand-file-name "inbox.org" my-gtd-local-dir))

  (setq my-gtd-shared-dir (expand-file-name "shared" gtd-test-agenda-temp-dir))
  (setq my-gtd-shared-projects (expand-file-name "projects" my-gtd-shared-dir))
  (setq my-gtd-shared-areas (expand-file-name "areas" my-gtd-shared-dir))

  (setq my-gtd-all-dirs (list my-gtd-local-dir my-gtd-local-areas my-gtd-local-projects
                              my-gtd-shared-dir my-gtd-shared-areas my-gtd-shared-projects))

  ;; Override agenda configuration to use only test files
  (setq org-agenda-files my-gtd-all-dirs)

  ;; Use the builder function to create test agenda command with test directories
  (setq my-gtd-day-agenda
        (my-gtd-build-day-agenda my-gtd-local-dir my-gtd-local-areas my-gtd-local-projects
                                 my-gtd-shared-dir my-gtd-shared-areas my-gtd-shared-projects))

  (setq org-agenda-custom-commands `(,my-gtd-day-agenda))

  ;; Create directories
  (make-directory my-gtd-local-projects t)
  (make-directory my-gtd-local-areas t)
  (make-directory my-gtd-shared-projects t)
  (make-directory my-gtd-shared-areas t))

(defun gtd-test-agenda-restore-directories ()
  "Restore original GTD directory variables."
  (when gtd-test-agenda-orig-vars
    (dolist (var-pair gtd-test-agenda-orig-vars)
      (set (car var-pair) (cdr var-pair)))
    (setq gtd-test-agenda-orig-vars nil)))

(defun gtd-test-agenda-cleanup ()
  "Clean up temporary directories and restore original state."
  (gtd-test-agenda-restore-directories)
  (when gtd-test-agenda-temp-dir
    (delete-directory gtd-test-agenda-temp-dir t)
    (setq gtd-test-agenda-temp-dir nil)))

(defun gtd-test-create-project-file (context title priority tasks &optional scheduled)
  "Create a test project file with specified CONTEXT, TITLE, PRIORITY, and TASKS.
CONTEXT should be 'local' or 'shared'.
TASKS is a list of plists with :title, :priority, :state."
  (let* ((projects-dir (if (string= context "local") my-gtd-local-projects my-gtd-shared-projects))
         (slug (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" (downcase title)))
         (filename (format "2024-01-15-%s.org" slug))
         (filepath (expand-file-name filename projects-dir))
         (org-id (org-id-new)))

    (with-temp-file filepath
      (insert (format "* [#%s] %s\n" priority title))
      (insert ":PROPERTIES:\n")
      (insert (format ":ID: %s\n" org-id))
      (insert ":GTD_TYPE: project\n")
      (insert ":CREATED: [2024-01-15 Mon 10:00]\n")
      (insert ":END:\n\n")

      (when scheduled
        (insert (format "SCHEDULED: %s\n\n" scheduled)))

      (insert "** Project Overview\n\n")
      (insert "** Tasks\n")
      (insert ":PROPERTIES:\n")
      (insert ":STYLE: checklist\n")
      (insert ":END:\n\n")

      (dolist (task tasks)
        (insert (format "*** %s%s %s\n"
                        (or (plist-get task :state) "TODO")
                        (if-let ((prio (plist-get task :priority)))
                            (format " [#%s]" prio)
                          "")
                        (plist-get task :title)))
        (insert ":PROPERTIES:\n")
        (insert ":CREATED: [2024-01-15 Mon 11:00]\n")
        (insert ":END:\n\n")))

    filepath))

(defun gtd-test-create-area-file (context title tasks)
  "Create a test area file with specified CONTEXT, TITLE, and TASKS."
  (let* ((areas-dir (if (string= context "local") my-gtd-local-areas my-gtd-shared-areas))
         (slug (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" (downcase title)))
         (filename (format "%s.org" slug))
         (filepath (expand-file-name filename areas-dir))
         (org-id (org-id-new)))

    (with-temp-file filepath
      (insert (format "* %s\n" title))
      (insert ":PROPERTIES:\n")
      (insert (format ":ID: %s\n" org-id))
      (insert ":GTD_TYPE: area\n")
      (insert ":CREATED: [2024-01-15 Mon 09:00]\n")
      (insert ":END:\n\n")

      (insert "** Active Items\n")
      (insert ":PROPERTIES:\n")
      (insert ":STYLE: checklist\n")
      (insert ":END:\n\n")

      (dolist (task tasks)
        (insert (format "*** %s%s %s\n"
                        (or (plist-get task :state) "TODO")
                        (if-let ((prio (plist-get task :priority)))
                            (format " [#%s]" prio)
                          "")
                        (plist-get task :title)))
        (insert ":PROPERTIES:\n")
        (insert ":CREATED: [2024-01-15 Mon 12:00]\n")
        (insert ":END:\n\n")))

    filepath))

(defun gtd-test-create-inbox-file ()
  "Create test inbox file with various items."
  (with-temp-file my-gtd-local-inbox
    (insert "* Items\n\n")

    ;; Ad-hoc tasks (not in projects/areas)
    (insert "** TODO [#A] Important ad-hoc task\n")
    (insert ":PROPERTIES:\n")
    (insert ":CREATED: [2024-01-15 Mon 08:00]\n")
    (insert ":END:\n\n")

    (insert "** TODO [#C] Low priority ad-hoc task\n")
    (insert ":PROPERTIES:\n")
    (insert ":CREATED: [2024-01-15 Mon 08:30]\n")
    (insert ":END:\n\n")

    (insert "** TODO [#E] Very low priority task\n")
    (insert ":PROPERTIES:\n")
    (insert ":CREATED: [2024-01-15 Mon 09:00]\n")
    (insert ":END:\n\n")

    ;; Scheduled item (should appear in calendar, not todo sections)
    (insert "** TODO Scheduled meeting\n")
    (insert "SCHEDULED: <2024-01-15 Mon 14:00>\n")
    (insert ":PROPERTIES:\n")
    (insert ":CREATED: [2024-01-15 Mon 07:00]\n")
    (insert ":END:\n\n")))

(defun gtd-test-create-test-data ()
  "Create comprehensive test data for agenda testing."
  ;; Create inbox
  (gtd-test-create-inbox-file)

  ;; Create local high-priority project
  (gtd-test-create-project-file
   "local" "Critical Local Project" "A"
   '((:title "High priority task" :priority "A" :state "TODO")
     (:title "Medium priority task" :priority "B" :state "TODO")
     (:title "Low priority task" :priority "E" :state "TODO")))

  ;; Create local low-priority project
  (gtd-test-create-project-file
   "local" "Routine Local Project" "D"
   '((:title "Routine task" :priority "C" :state "TODO")))

  ;; Create shared high-priority project
  (gtd-test-create-project-file
   "shared" "Important Shared Project" "B"
   '((:title "Shared high priority task" :priority "A" :state "TODO")
     (:title "Shared medium task" :priority "B" :state "TODO")))

  ;; Create shared project with scheduled item
  (gtd-test-create-project-file
   "shared" "Scheduled Shared Project" "A"
   '((:title "Regular task" :priority "A" :state "TODO"))
   "<2024-01-15 Mon 10:00>")

  ;; Create local area
  (gtd-test-create-area-file
   "local" "Personal Development"
   '((:title "Area high priority task" :priority "A" :state "TODO")
     (:title "Area low priority task" :priority "E" :state "TODO")))

  ;; Create shared area
  (gtd-test-create-area-file
   "shared" "Team Management"
   '((:title "Team task" :priority "B" :state "TODO"))))

;;; Agenda Content Validation

(defun gtd-test-get-agenda-content ()
  "Get the content of the agenda buffer as a string."
  (with-current-buffer "*Org Agenda*"
    (buffer-string)))

(defun gtd-test-agenda-contains-section-p (section-header)
  "Check if agenda contains a specific section header."
  (string-match-p (regexp-quote section-header) (gtd-test-get-agenda-content)))

(defun gtd-test-agenda-contains-task-p (task-title)
  "Check if agenda contains a specific task."
  (string-match-p (regexp-quote task-title) (gtd-test-get-agenda-content)))

(defun gtd-test-count-task-occurrences (task-title)
  "Count how many times a task appears in the agenda."
  (let ((content (gtd-test-get-agenda-content))
        (count 0)
        (start 0))
    (while (string-match (regexp-quote task-title) content start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

;;; Debug Test

(ert-deftest test-gtd-day-agenda-debug ()
  "Debug test to isolate agenda generation issues."
  (gtd-test-agenda-setup-directories)
  (unwind-protect
      (progn
        ;; Create minimal test data
        (gtd-test-create-inbox-file)

        ;; Try to generate agenda and catch detailed error
        (condition-case err
            (progn
              (org-agenda nil "d")
              (message "✅ Agenda generated successfully")
              ;; Just check that buffer exists
              (should (get-buffer "*Org Agenda*")))
          (error
           (message "❌ Agenda generation failed with error: %S" err)
           (ert-fail (format "Agenda generation failed: %S" err)))))

    ;; Cleanup
    (when (get-buffer "*Org Agenda*")
      (kill-buffer "*Org Agenda*"))
    (gtd-test-agenda-cleanup)))

;;; Simplified Test

(ert-deftest test-gtd-day-agenda-simple ()
  "Simplified test of GTD day agenda with basic validation."
  (gtd-test-agenda-setup-directories)
  (unwind-protect
      (progn
        ;; Create just one project file for testing
        (gtd-test-create-project-file
         "local" "Test Project" "A"
         '((:title "High priority task" :priority "A" :state "TODO")
           (:title "Low priority task" :priority "E" :state "TODO")))

        ;; Create inbox
        (gtd-test-create-inbox-file)

        ;; Generate agenda
        (condition-case err
            (org-agenda nil "d")
          (error (ert-fail (format "Simple agenda generation failed: %s" err))))

        ;; Basic validation
        (should (get-buffer "*Org Agenda*"))

        ;; Debug: Check test files and agenda configuration
        (message "=== TEST FILES DEBUG ===")
        (message "Test temp dir: %s" gtd-test-agenda-temp-dir)
        (message "Local projects dir: %s" my-gtd-local-projects)
        (message "Inbox file: %s" my-gtd-local-inbox)
        (message "Inbox exists: %s" (file-exists-p my-gtd-local-inbox))
        (message "Project files: %s" (directory-files my-gtd-local-projects))
        (message "org-agenda-files: %S" org-agenda-files)
        (message "=== AGENDA CONTENT ===")
        (let ((content (gtd-test-get-agenda-content)))
          (message "%s" content))
        (message "=== END DEBUG ===")

        ;; Check for actual content (relax the "Day:" requirement for now)
        (should (gtd-test-agenda-contains-task-p "Important ad-hoc task"))
        (should (gtd-test-agenda-contains-task-p "High priority task"))
        (should-not (gtd-test-agenda-contains-task-p "Low priority task"))

        (message "✅ Simple day agenda test passed"))

    ;; Cleanup
    (when (get-buffer "*Org Agenda*")
      (kill-buffer "*Org Agenda*"))
    (gtd-test-agenda-cleanup)))

;;; Main Test

(ert-deftest test-gtd-day-agenda-comprehensive ()
  "Comprehensive test of GTD day agenda filtering and organization."

  ;; Set up test environment
  (gtd-test-agenda-setup-directories)
  (unwind-protect
      (progn
        ;; Create test data
        (gtd-test-create-test-data)

        ;; Update org-agenda-files to include our test files
        (setq org-agenda-files my-gtd-all-dirs)

        ;; Generate the day agenda
        (let ((org-agenda-span 1)
              (org-agenda-start-day "2024-01-15"))
          ;; Run the custom day agenda command
          (condition-case err
              (org-agenda nil "d")
            (error (ert-fail (format "Day agenda generation failed: %s" err)))))

        ;; Validate agenda content
        (should (gtd-test-agenda-contains-section-p "Day:"))

        ;; Check calendar section exists (for scheduled items)
        (should (gtd-test-agenda-contains-task-p "Scheduled meeting"))
        (should (gtd-test-agenda-contains-task-p "Scheduled Shared Project"))

        ;; Check local tasks section
        (should (gtd-test-agenda-contains-section-p "Local ad-hoc and high-prio project tasks"))
        (should (gtd-test-agenda-contains-task-p "Important ad-hoc task"))
        (should (gtd-test-agenda-contains-task-p "High priority task"))
        (should (gtd-test-agenda-contains-task-p "Medium priority task"))
        (should (gtd-test-agenda-contains-task-p "Area high priority task"))

        ;; Check that low priority tasks are filtered out
        (should-not (gtd-test-agenda-contains-task-p "Low priority task"))
        (should-not (gtd-test-agenda-contains-task-p "Very low priority task"))
        (should-not (gtd-test-agenda-contains-task-p "Area low priority task"))

        ;; Check local projects section
        (should (gtd-test-agenda-contains-section-p "Local projects"))
        (should (gtd-test-agenda-contains-task-p "Critical Local Project"))
        (should-not (gtd-test-agenda-contains-task-p "Routine Local Project")) ; Priority D, should be filtered

        ;; Check shared tasks section
        (should (gtd-test-agenda-contains-section-p "Shared ad-hoc and high-prio project tasks"))
        (should (gtd-test-agenda-contains-task-p "Shared high priority task"))
        (should (gtd-test-agenda-contains-task-p "Team task"))

        ;; Check shared projects section
        (should (gtd-test-agenda-contains-section-p "Shared projects"))
        (should (gtd-test-agenda-contains-task-p "Important Shared Project"))
        (should (gtd-test-agenda-contains-task-p "Scheduled Shared Project"))

        ;; Validate that scheduled items don't appear in TODO sections
        ;; (they should only appear in calendar section)
        (let ((scheduled-in-todos (gtd-test-count-task-occurrences "Scheduled meeting")))
          (should (= scheduled-in-todos 1))) ; Should appear only once (in calendar)

        (message "✅ Day agenda test passed - all sections and filtering working correctly"))

    ;; Cleanup
    (when (get-buffer "*Org Agenda*")
      (kill-buffer "*Org Agenda*"))
    (gtd-test-agenda-cleanup)))

;;; Quick Smoke Test

(ert-deftest test-gtd-day-agenda-smoke ()
  "Quick smoke test to ensure day agenda can be generated without errors."
  (gtd-test-agenda-setup-directories)
  (unwind-protect
      (progn
        ;; Create minimal test data
        (gtd-test-create-inbox-file)
        (setq org-agenda-files my-gtd-all-dirs)

        ;; This should not error
        (condition-case err
            (org-agenda nil "d")
          (error (ert-fail (format "Agenda generation failed: %s" err))))

        ;; Should create agenda buffer
        (should (get-buffer "*Org Agenda*"))

        (message "✅ Day agenda smoke test passed"))

    ;; Cleanup
    (when (get-buffer "*Org Agenda*")
      (kill-buffer "*Org Agenda*"))
    (gtd-test-agenda-cleanup)))

(provide 'test-gtd-agenda)
;;; test-gtd-agenda.el ends here