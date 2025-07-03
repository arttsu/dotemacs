;;; test-gtd-integration.el --- Integration tests for GTD workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for the GTD workflow functions

;;; Code:

(require 'ert)
(require 'org)

;; Load the configuration
(let ((config-dir (expand-file-name ".." (file-name-directory load-file-name))))
  (load (expand-file-name "init.el" config-dir)))

;;; Test Utilities

(defmacro with-gtd-test-buffer (content &rest body)
  "Create a temporary org buffer with CONTENT and execute BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defun gtd-test-buffer-lines ()
  "Get all non-empty lines from current buffer."
  (split-string (buffer-string) "\n" t))

(defun gtd-test-get-todo-states ()
  "Get list of TODO states in order from current buffer."
  (let (states)
    (org-map-entries
     (lambda ()
       (push (org-get-todo-state) states))
     nil nil)
    (nreverse states)))

(defun gtd-test-get-headings ()
  "Get list of headings in order from current buffer."
  (let (headings)
    (org-map-entries
     (lambda ()
       (push (org-get-heading t t t t) headings))
     nil nil)
    (nreverse headings)))

;;; Tests

(ert-deftest test-gtd-sort-checklist ()
  "Test sorting a checklist with mixed TODO and DONE items."
  (with-gtd-test-buffer
      "* My Checklist
:PROPERTIES:
:STYLE: checklist
:END:

** TODO [#B] Second priority task
:PROPERTIES:
:CREATED: [2024-01-02 Tue 10:00]
:END:

** DONE Completed task
CLOSED: [2024-01-03 Wed 15:00]
:PROPERTIES:
:CREATED: [2024-01-01 Mon 09:00]
:END:

** TODO [#A] First priority task
:PROPERTIES:
:CREATED: [2024-01-01 Mon 08:00]
:END:

** TODO [#B] First by timestamp
:PROPERTIES:
:CREATED: [2024-01-01 Mon 07:00]
:END:

** DONE Another completed task
CLOSED: [2024-01-04 Thu 16:00]
:PROPERTIES:
:CREATED: [2024-01-02 Tue 11:00]
:END:
"
    ;; Move to the checklist heading
    (goto-char (point-min))
    
    ;; Run the sort function
    (my-gtd-sort-by-style)
    
    ;; Check the resulting order
    (let ((headings (gtd-test-get-headings))
          (states (gtd-test-get-todo-states)))
      
      ;; First should be TODO items sorted by priority then timestamp
      (should (equal (nth 1 headings) "First priority task"))  ; [#A]
      (should (equal (nth 2 headings) "First by timestamp"))   ; [#B] earlier
      (should (equal (nth 3 headings) "Second priority task")) ; [#B] later
      
      ;; Then DONE items sorted by closed timestamp
      (should (equal (nth 4 headings) "Completed task"))       ; Closed earlier
      (should (equal (nth 5 headings) "Another completed task")) ; Closed later
      
      ;; Verify TODO states are in correct order
      (should (equal states '(nil "TODO" "TODO" "TODO" "DONE" "DONE"))))))

(ert-deftest test-gtd-sort-log ()
  "Test sorting a log with entries in reverse chronological order."
  (with-gtd-test-buffer
      "* My Log
:PROPERTIES:
:STYLE: log
:END:

** Entry from yesterday
:PROPERTIES:
:CREATED: [2024-01-01 Mon 14:00]
:END:

** Entry from today
:PROPERTIES:
:CREATED: [2024-01-02 Tue 10:00]
:END:

** Entry from last week
:PROPERTIES:
:CREATED: [2023-12-28 Thu 09:00]
:END:
"
    ;; Move to the log heading
    (goto-char (point-min))
    
    ;; Run the sort function
    (my-gtd-sort-by-style)
    
    ;; Check the resulting order (newest first)
    (let ((headings (gtd-test-get-headings)))
      (should (equal (nth 1 headings) "Entry from today"))
      (should (equal (nth 2 headings) "Entry from yesterday"))
      (should (equal (nth 3 headings) "Entry from last week")))))

;;; Test Runner

(defun run-gtd-tests ()
  "Run all GTD integration tests."
  (interactive)
  (ert-run-tests-batch-and-exit "test-gtd-"))

(provide 'test-gtd-integration)
;;; test-gtd-integration.el ends here