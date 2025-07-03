;;; test-gtd-project-lifecycle.el --- Tests for GTD project lifecycle functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for project and area creation and archiving functionality

;;; Code:

(require 'ert)
(require 'org)
(require 'test-gtd-utils)

;;; Test Fixtures

(defvar gtd-test-temp-dir nil
  "Temporary directory for test files.")

(defun gtd-test-setup-temp-directories ()
  "Set up temporary directories for testing project lifecycle functions."
  (setq gtd-test-temp-dir (make-temp-file "gtd-test" t))

  ;; Override GTD directory constants for testing
  (setq my-gtd-local-projects (expand-file-name "local/projects" gtd-test-temp-dir))
  (setq my-gtd-local-areas (expand-file-name "local/areas" gtd-test-temp-dir))
  (setq my-gtd-shared-projects (expand-file-name "shared/projects" gtd-test-temp-dir))
  (setq my-gtd-shared-areas (expand-file-name "shared/areas" gtd-test-temp-dir))

  ;; Create directories
  (make-directory my-gtd-local-projects t)
  (make-directory my-gtd-local-areas t)
  (make-directory my-gtd-shared-projects t)
  (make-directory my-gtd-shared-areas t))

(defun gtd-test-cleanup-temp-directories ()
  "Clean up temporary directories after testing."
  (when gtd-test-temp-dir
    (delete-directory gtd-test-temp-dir t)))

(defun gtd-test-with-temp-dirs (body-fn)
  "Execute BODY-FN with temporary GTD directories set up."
  (gtd-test-setup-temp-directories)
  (unwind-protect
      (funcall body-fn)
    (gtd-test-cleanup-temp-directories)))

;;; Template Path Tests

(ert-deftest test-gtd-template-path-resolver ()
  "Test that template path resolver constructs correct paths."
  (let ((expected-project (expand-file-name "capture-templates/gtd-project.txt" user-emacs-directory))
        (expected-area (expand-file-name "capture-templates/gtd-area.txt" user-emacs-directory)))
    (should (string= (my-org-capture-template-path "gtd-project") expected-project))
    (should (string= (my-org-capture-template-path "gtd-area") expected-area))))

;;; Project Creation Tests

(ert-deftest test-gtd-create-project-local ()
  "Test creating a project in local context."
  (gtd-test-with-temp-dirs
   (lambda ()
     ;; Mock user input
     (let ((title "Test Project")
           (context "Local")
           (priority ?A))

       ;; Mock completing-read and read functions
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) context))
                 ((symbol-function 'read-string)
                  (lambda (&rest _) title))
                 ((symbol-function 'read-char-choice)
                  (lambda (&rest _) priority))
                 ((symbol-function 'find-file)
                  (lambda (file)
                    ;; Just set the buffer file name without actually opening
                    (setq buffer-file-name file)))
                 ((symbol-function 'y-or-n-p)
                  (lambda (&rest _) nil)))

         ;; Call the function
         (my-gtd-create-project)

         ;; Check that file was created in the right location
         (let ((expected-pattern (concat my-gtd-local-projects "/[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-test-project\\.org")))
           (should (directory-files my-gtd-local-projects nil ".*test-project.*\\.org$"))

           ;; Check file content
           (let ((created-file (car (directory-files my-gtd-local-projects t ".*test-project.*\\.org$"))))
             (when created-file
               (with-temp-buffer
                 (insert-file-contents created-file)
                 (should (string-match-p "\\* \\[#A\\] Test Project" (buffer-string)))
                 (should (string-match-p ":GTD_TYPE: project" (buffer-string)))
                 (should (string-match-p ":ID:" (buffer-string)))
                 (should (string-match-p "\\*\\* Resources" (buffer-string)))
                 (should (string-match-p "\\*\\* To-dos" (buffer-string)))
                 (should (string-match-p ":STYLE: checklist" (buffer-string)))
                 (should (string-match-p "\\*\\* Log" (buffer-string)))
                 (should (string-match-p ":STYLE: log" (buffer-string))))))))))))

(ert-deftest test-gtd-create-project-shared ()
  "Test creating a project in shared context."
  (gtd-test-with-temp-dirs
   (lambda ()
     (let ((title "Shared Project")
           (context "Shared")
           (priority ?B))

       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) context))
                 ((symbol-function 'read-string)
                  (lambda (&rest _) title))
                 ((symbol-function 'read-char-choice)
                  (lambda (&rest _) priority))
                 ((symbol-function 'find-file)
                  (lambda (file)
                    (setq buffer-file-name file)))
                 ((symbol-function 'y-or-n-p)
                  (lambda (&rest _) nil)))

         (my-gtd-create-project)

         ;; Check that file was created in shared projects directory
         (should (directory-files my-gtd-shared-projects nil ".*shared-project.*\\.org$"))

         ;; Check file content has correct priority
         (let ((created-file (car (directory-files my-gtd-shared-projects t ".*shared-project.*\\.org$"))))
           (when created-file
             (with-temp-buffer
               (insert-file-contents created-file)
               (should (string-match-p "\\* \\[#B\\] Shared Project" (buffer-string)))))))))))

(ert-deftest test-gtd-create-project-lowercase-priority ()
  "Test creating a project with lowercase priority input."
  (gtd-test-with-temp-dirs
   (lambda ()
     (let ((title "Test Project")
           (context "Local")
           (priority ?b))  ; lowercase b

       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) context))
                 ((symbol-function 'read-string)
                  (lambda (&rest _) title))
                 ((symbol-function 'read-char-choice)
                  (lambda (&rest _) priority))
                 ((symbol-function 'find-file)
                  (lambda (file)
                    (setq buffer-file-name file)))
                 ((symbol-function 'y-or-n-p)
                  (lambda (&rest _) nil)))

         (my-gtd-create-project)

         ;; Check file content has uppercase priority
         (let ((created-file (car (directory-files my-gtd-local-projects t ".*test-project.*\\.org$"))))
           (when created-file
             (with-temp-buffer
               (insert-file-contents created-file)
               (should (string-match-p "\\* \\[#B\\] Test Project" (buffer-string)))))))))))

(ert-deftest test-gtd-create-project-default-priority ()
  "Test creating a project with default priority (Enter pressed)."
  (gtd-test-with-temp-dirs
   (lambda ()
     (let ((title "Test Project")
           (context "Local")
           (priority ?\r))  ; Enter key

       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) context))
                 ((symbol-function 'read-string)
                  (lambda (&rest _) title))
                 ((symbol-function 'read-char-choice)
                  (lambda (&rest _) priority))
                 ((symbol-function 'find-file)
                  (lambda (file)
                    (setq buffer-file-name file)))
                 ((symbol-function 'y-or-n-p)
                  (lambda (&rest _) nil)))

         (my-gtd-create-project)

         ;; Check file content has default priority D
         (let ((created-file (car (directory-files my-gtd-local-projects t ".*test-project.*\\.org$"))))
           (when created-file
             (with-temp-buffer
               (insert-file-contents created-file)
               (should (string-match-p "\\* \\[#D\\] Test Project" (buffer-string)))))))))))

(ert-deftest test-gtd-create-project-special-characters ()
  "Test creating a project with special characters in title."
  (gtd-test-with-temp-dirs
   (lambda ()
     (let ((title "Test! Project# & More$")
           (context "Local")
           (priority ?A))

       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) context))
                 ((symbol-function 'read-string)
                  (lambda (&rest _) title))
                 ((symbol-function 'read-char-choice)
                  (lambda (&rest _) priority))
                 ((symbol-function 'find-file)
                  (lambda (file)
                    (setq buffer-file-name file)))
                 ((symbol-function 'y-or-n-p)
                  (lambda (&rest _) nil)))

         (my-gtd-create-project)

         ;; Check that file was created with special characters removed from slug
         (should (directory-files my-gtd-local-projects nil ".*test-project--more.*\\.org$")))))))

(ert-deftest test-gtd-create-project-empty-title ()
  "Test that empty title throws an error."
  (gtd-test-with-temp-dirs
   (lambda ()
     (cl-letf (((symbol-function 'completing-read)
                (lambda (&rest _) "Local"))
               ((symbol-function 'read-string)
                (lambda (&rest _) ""))
               ((symbol-function 'read-char-choice)
                (lambda (&rest _) ?A))
               ((symbol-function 'y-or-n-p)
                (lambda (&rest _) nil)))

       (should-error (my-gtd-create-project) :type 'user-error)))))

;;; Area Creation Tests

(ert-deftest test-gtd-create-area-local ()
  "Test creating an area in local context."
  (gtd-test-with-temp-dirs
   (lambda ()
     (let ((title "Test Area")
           (context "Local"))

       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) context))
                 ((symbol-function 'read-string)
                  (lambda (&rest _) title))
                 ((symbol-function 'find-file)
                  (lambda (file)
                    (setq buffer-file-name file))))

         (my-gtd-create-area)

         ;; Check that file was created in local areas directory
         (should (directory-files my-gtd-local-areas nil "test-area\\.org$"))

         ;; Check file content
         (let ((created-file (expand-file-name "test-area.org" my-gtd-local-areas)))
           (when (file-exists-p created-file)
             (with-temp-buffer
               (insert-file-contents created-file)
               (should (string-match-p "\\* Test Area" (buffer-string)))
               (should (string-match-p ":GTD_TYPE: area" (buffer-string)))
               (should (string-match-p ":ID:" (buffer-string)))))))))))

(ert-deftest test-gtd-create-area-shared ()
  "Test creating an area in shared context."
  (gtd-test-with-temp-dirs
   (lambda ()
     (let ((title "Shared Area")
           (context "Shared"))

       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) context))
                 ((symbol-function 'read-string)
                  (lambda (&rest _) title))
                 ((symbol-function 'find-file)
                  (lambda (file)
                    (setq buffer-file-name file))))

         (my-gtd-create-area)

         ;; Check that file was created in shared areas directory
         (should (directory-files my-gtd-shared-areas nil "shared-area\\.org$")))))))

(ert-deftest test-gtd-create-area-empty-title ()
  "Test that empty area title throws an error."
  (gtd-test-with-temp-dirs
   (lambda ()
     (cl-letf (((symbol-function 'completing-read)
                (lambda (&rest _) "Local"))
               ((symbol-function 'read-string)
                (lambda (&rest _) "")))

       (should-error (my-gtd-create-area) :type 'user-error)))))

;;; Project Archiving Tests

(ert-deftest test-gtd-archive-project-basic ()
  "Test basic project archiving functionality."
  (gtd-test-with-temp-dirs
   (lambda ()
     ;; Create a test project file
     (let ((project-file (expand-file-name "test-project.org" my-gtd-local-projects))
           (project-content "#+TITLE: Test Project\n\n* [#A] Test Project\n:PROPERTIES:\n:ID: test-id-123\n:GTD_TYPE: project\n:CREATED: [2024-01-01 Mon 10:00]\n:END:\n\n** TODO First task\n"))

       ;; Create the project file
       (with-temp-buffer
         (insert project-content)
         (write-file project-file))

       ;; Open the file and archive it
       (with-current-buffer (find-file-noselect project-file)
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (&rest _) t)))

           (my-gtd-archive-project)

           ;; Check that file was moved to archive directory
           (let ((archive-dir (expand-file-name "archive" my-gtd-local-projects))
                 (archived-file (expand-file-name "archive/test-project.org" my-gtd-local-projects)))

             (should (file-exists-p archive-dir))
             (should (file-exists-p archived-file))
             (should-not (file-exists-p project-file))

             ;; Check that priority was removed and ARCHIVED property was added
             (with-temp-buffer
               (insert-file-contents archived-file)
               (should-not (string-match-p "\\[#A\\]" (buffer-string)))
               (should (string-match-p ":ARCHIVED:" (buffer-string)))))))))))

(ert-deftest test-gtd-archive-project-not-gtd-project ()
  "Test that archiving fails for non-GTD project files."
  (gtd-test-with-temp-dirs
   (lambda ()
     ;; Create a non-GTD project file
     (let ((regular-file (expand-file-name "regular.org" my-gtd-local-projects))
           (regular-content "#+TITLE: Regular File\n\n* Some heading\n"))

       (with-temp-buffer
         (insert regular-content)
         (write-file regular-file))

       ;; Try to archive it
       (with-current-buffer (find-file-noselect regular-file)
         (should-error (my-gtd-archive-project) :type 'user-error))))))

(ert-deftest test-gtd-archive-project-user-cancellation ()
  "Test that archiving can be cancelled by user."
  (gtd-test-with-temp-dirs
   (lambda ()
     ;; Create a test project file
     (let ((project-file (expand-file-name "test-project.org" my-gtd-local-projects))
           (project-content "#+TITLE: Test Project\n\n* [#A] Test Project\n:PROPERTIES:\n:GTD_TYPE: project\n:END:\n"))

       (with-temp-buffer
         (insert project-content)
         (write-file project-file))

       ;; Open file and cancel archiving
       (with-current-buffer (find-file-noselect project-file)
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (&rest _) nil)))  ; User says no

           (my-gtd-archive-project)

           ;; Check that file was not moved
           (should (file-exists-p project-file))
           (should-not (file-exists-p (expand-file-name "archive" my-gtd-local-projects)))))))))

(ert-deftest test-gtd-archive-project-not-org-mode ()
  "Test that archiving fails in non-org-mode buffers."
  (with-temp-buffer
    (text-mode)  ; Not org-mode
    (should-error (my-gtd-archive-project) :type 'user-error)))

(ert-deftest test-gtd-archive-project-no-file ()
  "Test that archiving fails for buffers not visiting files."
  (with-temp-buffer
    (org-mode)
    ;; Buffer not visiting a file
    (should-error (my-gtd-archive-project) :type 'user-error)))

;;; Update test integration file

(provide 'test-gtd-project-lifecycle)
;;; test-gtd-project-lifecycle.el ends here
