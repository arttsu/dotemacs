;;; test-gtd-sorting.el --- Tests for GTD sorting functionality -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for GTD sorting functionality including checklists and logs

;;; Code:

(require 'ert)
(require 'test-gtd-utils)

;; Load the configuration
(let ((config-dir (expand-file-name ".." (file-name-directory load-file-name))))
  (load (expand-file-name "init.el" config-dir)))

;;; Checklist Sorting Tests

(ert-deftest test-gtd-sort-checklist-basic ()
  "Test sorting a checklist with mixed TODO and DONE items."
  (with-gtd-test-buffer
      (gtd-test-create-checklist
       "My Checklist"
       '((:state "TODO" :priority "B" :heading "Second priority task" :created "[2024-01-02 Tue 10:00]")
         (:state "DONE" :heading "Completed task" :created "[2024-01-01 Mon 09:00]" :closed "[2024-01-03 Wed 15:00]")
         (:state "TODO" :priority "A" :heading "First priority task" :created "[2024-01-01 Mon 08:00]")
         (:state "TODO" :priority "B" :heading "First by timestamp" :created "[2024-01-01 Mon 07:00]")
         (:state "DONE" :heading "Another completed task" :created "[2024-01-02 Tue 11:00]" :closed "[2024-01-04 Thu 16:00]")))

    ;; Run the sort function
    (my-gtd-sort-by-style)

    ;; Check the resulting order
    (let ((headings (gtd-test-get-headings))
          (states (gtd-test-get-todo-states)))

      ;; First should be TODO items sorted by priority then timestamp
      (should (equal (nth 1 headings) "First priority task"))  ; [#A]
      (should (equal (nth 2 headings) "First by timestamp"))   ; [#B] earlier
      (should (equal (nth 3 headings) "Second priority task")) ; [#B] later

      ;; Then DONE items sorted by closed timestamp (newest first)
      (should (equal (nth 4 headings) "Another completed task")) ; Closed later (2024-01-04)
      (should (equal (nth 5 headings) "Completed task"))       ; Closed earlier (2024-01-03)

      ;; Verify TODO states are in correct order
      (should (equal states '(nil "TODO" "TODO" "TODO" "DONE" "DONE"))))))

(ert-deftest test-gtd-sort-checklist-empty ()
  "Test sorting an empty checklist gives appropriate error."
  (with-gtd-test-buffer
      (gtd-test-create-checklist "Empty Checklist" '())

    ;; Empty checklist should give "Nothing to sort" error
    (should-error (my-gtd-sort-by-style) :type 'user-error)))

(ert-deftest test-gtd-sort-checklist-only-todos ()
  "Test sorting checklist with only TODO items."
  (with-gtd-test-buffer
      (gtd-test-create-checklist
       "TODO Only"
       '((:state "TODO" :priority "C" :heading "Low priority" :created "[2024-01-01 Mon 10:00]")
         (:state "TODO" :priority "A" :heading "High priority" :created "[2024-01-01 Mon 09:00]")
         (:state "TODO" :priority "B" :heading "Medium priority" :created "[2024-01-01 Mon 08:00]")))

    (my-gtd-sort-by-style)

    (let ((headings (gtd-test-get-headings)))
      (should (equal (nth 1 headings) "High priority"))
      (should (equal (nth 2 headings) "Medium priority"))
      (should (equal (nth 3 headings) "Low priority")))))

(ert-deftest test-gtd-sort-from-child-entry ()
  "Test that sorting from child entry navigates to parent and sorts."
  (with-gtd-test-buffer
      (gtd-test-create-checklist
       "Parent Checklist"
       '((:state "TODO" :heading "Second" :created "[2024-01-02 Mon 10:00]")
         (:state "TODO" :heading "First" :created "[2024-01-01 Mon 10:00]")))

    ;; Debug: show the buffer content
    ;; (message "Buffer before sort:\n%s" (buffer-string))

    ;; Move to child entry
    (search-forward "Second")
    (beginning-of-line)

    ;; Sorting from child should work by navigating to parent
    (my-gtd-sort-entries)

    ;; Check that items were sorted correctly
    (goto-char (point-min))  ; Reset position for consistent results
    (let ((headings (gtd-test-get-headings)))
      (should (equal (nth 1 headings) "First"))
      (should (equal (nth 2 headings) "Second")))))

(ert-deftest test-gtd-sort-no-style-property ()
  "Test that sorting without STYLE property gives appropriate error."
  (with-gtd-test-buffer
      "* Regular Heading

** Child 1

** Child 2

"
    ;; Try to sort from parent without STYLE
    (goto-char (point-min))
    (should-error (my-gtd-sort-entries) :type 'user-error)

    ;; Try to sort from child without STYLE
    (search-forward "Child 1")
    (beginning-of-line)
    (should-error (my-gtd-sort-entries) :type 'user-error)))

;;; Log Sorting Tests

(ert-deftest test-gtd-sort-log-basic ()
  "Test sorting a log with entries in reverse chronological order."
  (with-gtd-test-buffer
      (gtd-test-create-log
       "My Log"
       '((:heading "Yesterday's entry" :created "[2024-01-01 Mon 14:00]")
         (:heading "Today's entry" :created "[2024-01-02 Tue 10:00]")
         (:heading "Last week's entry" :created "[2023-12-28 Thu 09:00]")))

    (my-gtd-sort-by-style)

    ;; Check the resulting order (newest first)
    (let ((headings (gtd-test-get-headings)))
      (should (equal (nth 1 headings) "Today's entry"))
      (should (equal (nth 2 headings) "Yesterday's entry"))
      (should (equal (nth 3 headings) "Last week's entry")))))

(ert-deftest test-gtd-sort-log-missing-timestamps ()
  "Test sorting log with some entries missing CREATED property."
  (with-gtd-test-buffer
      "* My Log
:PROPERTIES:
:STYLE: log
:END:

** Entry with timestamp
:PROPERTIES:
:CREATED: [2024-01-02 Tue 10:00]
:END:

** Entry without timestamp

Some content here

** Another timestamped entry
:PROPERTIES:
:CREATED: [2024-01-01 Mon 10:00]
:END:

"
    (my-gtd-sort-by-style)

    ;; Entries without timestamps should sort to the end (oldest)
    (let ((headings (gtd-test-get-headings)))
      (should (equal (nth 1 headings) "Entry with timestamp"))
      (should (equal (nth 2 headings) "Another timestamped entry"))
      (should (equal (nth 3 headings) "Entry without timestamp")))))

(provide 'test-gtd-sorting)
;;; test-gtd-sorting.el ends here
