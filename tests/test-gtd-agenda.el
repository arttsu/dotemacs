;;; test-gtd-agenda.el --- Tests for GTD day agenda functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for the GTD day agenda system using golden file testing
;; Tests complete agenda output against expected snapshots

;;; Code:

(require 'ert)
(require 'org)
(require 'org-agenda)
(require 'test-gtd-utils)
(require 'test-gtd-golden)

;;; Test Fixtures

(defvar gtd-test-agenda-temp-dir nil
  "Temporary directory for agenda test files.")

(defvar gtd-test-agenda-orig-vars nil
  "Original values of GTD directory variables.")


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
          (my-gtd-shared-inbox . ,my-gtd-shared-inbox)
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
  (setq my-gtd-shared-inbox (expand-file-name "inbox.org" my-gtd-shared-dir))
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

(defun gtd-test-create-shared-inbox-file ()
  "Create test shared inbox file with items."
  (with-temp-file my-gtd-shared-inbox
    (insert "* Shared inbox\n\n")

    ;; Item that appears in the golden file
    (insert "** TODO Look into Claude Code hooks\n")
    (insert ":PROPERTIES:\n")
    (insert ":CREATED: [2024-01-15 Mon 10:00]\n")
    (insert ":END:\n\n")))

(defun gtd-test-create-test-data ()
  "Create comprehensive test data for agenda testing."
  ;; Create inbox files
  (gtd-test-create-inbox-file)
  (gtd-test-create-shared-inbox-file)

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

;;; Golden File Test Creation

(defun gtd-test-create-comprehensive-test-data ()
  "Create comprehensive, realistic test data for golden file generation."
  ;; Create inbox files with variety of priorities and types
  (gtd-test-create-inbox-file)
  (gtd-test-create-shared-inbox-file)

  ;; Local high-priority project with mixed task priorities
  (gtd-test-create-project-file
   "local" "Website Redesign" "A"
   '((:title "Design mockups" :priority "A" :state "TODO")
     (:title "Code landing page" :priority "B" :state "TODO")
     (:title "Write copy" :priority "B" :state "TODO")
     (:title "Setup analytics" :priority "C" :state "TODO")
     (:title "SEO optimization" :priority "E" :state "TODO")))

  ;; Local medium-priority project
  (gtd-test-create-project-file
   "local" "Home Organization" "B"
   '((:title "Declutter office" :priority "A" :state "TODO")
     (:title "File documents" :priority "B" :state "TODO")))

  ;; Local low-priority project (should be filtered out)
  (gtd-test-create-project-file
   "local" "Learn Guitar" "D"
   '((:title "Practice scales" :priority "C" :state "TODO")))

  ;; Shared high-priority project
  (gtd-test-create-project-file
   "shared" "Product Launch" "A"
   '((:title "Finalize features" :priority "A" :state "TODO")
     (:title "Marketing plan" :priority "A" :state "TODO")
     (:title "Beta testing" :priority "B" :state "TODO")))

  ;; Shared project with scheduling
  (gtd-test-create-project-file
   "shared" "Team Quarterly Review" "B"
   '((:title "Prepare slides" :priority "A" :state "TODO"))
   "<2024-01-15 Mon 10:00>")

  ;; Local area with high-priority items
  (gtd-test-create-area-file
   "local" "Health & Fitness"
   '((:title "Schedule dentist appointment" :priority "A" :state "TODO")
     (:title "Research nutrition plan" :priority "B" :state "TODO")
     (:title "Buy new running shoes" :priority "E" :state "TODO")))

  ;; Shared area
  (gtd-test-create-area-file
   "shared" "Team Development"
   '((:title "Plan code review process" :priority "B" :state "TODO")
     (:title "Update documentation" :priority "C" :state "TODO"))))

;;; Golden File Tests

(ert-deftest test-gtd-day-agenda-golden ()
  "Golden file test for GTD day agenda - comprehensive validation."
  (gtd-test-agenda-setup-directories)
  (unwind-protect
      (progn
        ;; Create comprehensive test data
        (gtd-test-create-comprehensive-test-data)

        ;; Set specific date for consistent agenda output
        (let ((org-agenda-span 1)
              (org-agenda-start-day "2024-01-15"))

          ;; Generate the day agenda
          (condition-case err
              (org-agenda nil "d")
            (error (ert-fail (format "Agenda generation failed: %s" err))))

          ;; Test against golden file
          (should (gtd-golden-test-agenda "day-agenda-comprehensive"))))

    ;; Cleanup
    (when (get-buffer "*Org Agenda*")
      (kill-buffer "*Org Agenda*"))
    (gtd-test-agenda-cleanup)))

;;; Minimal Golden File Test

(ert-deftest test-gtd-day-agenda-golden-minimal ()
  "Minimal golden file test for GTD day agenda - basic structure validation."
  (gtd-test-agenda-setup-directories)
  (unwind-protect
      (progn
        ;; Create minimal but realistic test data
        (gtd-test-create-inbox-file)
        (gtd-test-create-shared-inbox-file)

        ;; One local project with mixed priorities
        (gtd-test-create-project-file
         "local" "Test Project" "A"
         '((:title "High priority task" :priority "A" :state "TODO")
           (:title "Low priority task" :priority "E" :state "TODO")))

        ;; One shared area
        (gtd-test-create-area-file
         "shared" "Test Area"
         '((:title "Area task" :priority "B" :state "TODO")))

        ;; Set specific date for consistent output
        (let ((org-agenda-span 1)
              (org-agenda-start-day "2024-01-15"))

          ;; Generate agenda
          (condition-case err
              (org-agenda nil "d")
            (error (ert-fail (format "Minimal agenda generation failed: %s" err))))

          ;; Test against golden file
          (should (gtd-golden-test-agenda "day-agenda-minimal"))))

    ;; Cleanup
    (when (get-buffer "*Org Agenda*")
      (kill-buffer "*Org Agenda*"))
    (gtd-test-agenda-cleanup)))


;;; Golden File Creation Utility

(defun gtd-create-golden-files-manually ()
  "Utility function to create golden files manually.
This should only be run when setting up golden files for the first time
or when agenda format changes intentionally."
  (interactive)
  (gtd-test-agenda-setup-directories)
  (unwind-protect
      (progn
        ;; Create comprehensive test data
        (gtd-test-create-comprehensive-test-data)

        (let ((org-agenda-span 1)
              (org-agenda-start-day "2024-01-15"))

          ;; Generate agenda
          (org-agenda nil "d")

          ;; Create golden files
          (gtd-golden-create-golden-file "day-agenda-comprehensive" t)
          (message "Golden file created for comprehensive test")

          ;; Kill and regenerate for minimal test
          (kill-buffer "*Org Agenda*")
          (gtd-test-agenda-cleanup)
          (gtd-test-agenda-setup-directories)

          ;; Create minimal data
          (gtd-test-create-inbox-file)
          (gtd-test-create-shared-inbox-file)
          (gtd-test-create-project-file
           "local" "Test Project" "A"
           '((:title "High priority task" :priority "A" :state "TODO")
             (:title "Low priority task" :priority "E" :state "TODO")))
          (gtd-test-create-area-file
           "shared" "Test Area"
           '((:title "Area task" :priority "B" :state "TODO")))

          ;; Generate minimal agenda
          (org-agenda nil "d")
          (gtd-golden-create-golden-file "day-agenda-minimal" t)
          (message "Golden file created for minimal test"))

        (message "✅ Golden files created successfully!"))

    ;; Cleanup
    (when (get-buffer "*Org Agenda*")
      (kill-buffer "*Org Agenda*"))
    (gtd-test-agenda-cleanup)))

;;; Debug Utilities

(defun gtd-test-debug-agenda-output ()
  "Interactive function to debug agenda output."
  (interactive)
  (when (get-buffer "*Org Agenda*")
    (gtd-golden-debug-content "debug")))

(provide 'test-gtd-agenda)
;;; test-gtd-agenda.el ends here