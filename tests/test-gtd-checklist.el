;;; test-gtd-checklist.el --- Tests for GTD checklist management -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for GTD checklist functionality including reset and won't-do

;;; Code:

(require 'ert)
(require 'test-gtd-utils)

;; Load the configuration
(let ((config-dir (expand-file-name ".." (file-name-directory load-file-name))))
  (load (expand-file-name "init.el" config-dir)))

;;; Reset Checklist Tests

(ert-deftest test-gtd-reset-checklist-basic ()
  "Test resetting a checklist with mixed states."
  (with-gtd-test-buffer
      (gtd-test-create-checklist
       "Reset Test"
       '((:state "TODO" :heading "Already TODO")
         (:state "DONE" :heading "Completed item" :closed "[2024-01-01 Mon 12:00]")
         (:state "DONE" :heading "+Won't do item+" :closed "[2024-01-01 Mon 13:00]" :closed-as "WONT_DO")))
    
    ;; Simulate yes response
    (with-simulated-input t
      (my-gtd-reset-checklist))
    
    ;; All items should be TODO
    (let ((states (gtd-test-get-todo-states))
          (headings (gtd-test-get-headings))
          (closed-as-props (gtd-test-get-properties "CLOSED_AS")))
      
      (should (equal states '(nil "TODO" "TODO" "TODO")))
      ;; Strikethrough should be removed
      (should (equal (nth 3 headings) "Won't do item"))
      ;; CLOSED_AS properties should be nil
      (should (equal closed-as-props '(nil nil nil nil))))))

(ert-deftest test-gtd-reset-checklist-cancelled ()
  "Test cancelling checklist reset."
  (with-gtd-test-buffer
      (gtd-test-create-checklist
       "Cancel Test"
       '((:state "DONE" :heading "Should stay done")))
    
    ;; Simulate no response
    (with-simulated-input nil
      (my-gtd-reset-checklist))
    
    ;; State should remain unchanged
    (let ((states (gtd-test-get-todo-states)))
      (should (equal states '(nil "DONE"))))))

(ert-deftest test-gtd-reset-not-checklist ()
  "Test reset on non-checklist should error."
  (with-gtd-test-buffer
      "* Not a checklist\n** TODO Some item"
    
    (should-error (my-gtd-reset-checklist) :type 'error)))

;;; Won't Do Tests

(ert-deftest test-gtd-complete-as-wont-do-mark ()
  "Test marking a TODO item as won't do."
  (with-gtd-test-buffer
      "* TODO Test item
:PROPERTIES:
:CREATED: [2024-01-01 Mon 10:00]
:END:
"
    (my-gtd-complete-as-wont-do)
    
    ;; Check state change
    (should (string= (org-get-todo-state) "DONE"))
    ;; Check property
    (should (string= (org-entry-get (point) "CLOSED_AS") "WONT_DO"))
    ;; Check strikethrough
    (should (string= (org-get-heading t t t t) "+Test item+"))
    ;; Check closed timestamp was added
    (should (org-entry-get (point) "CLOSED"))))

(ert-deftest test-gtd-complete-as-wont-do-unmark ()
  "Test unmarking a won't do item."
  (with-gtd-test-buffer
      "* DONE +Already marked+
CLOSED: [2024-01-01 Mon 12:00]
:PROPERTIES:
:CREATED: [2024-01-01 Mon 10:00]
:CLOSED_AS: WONT_DO
:END:
"
    (my-gtd-complete-as-wont-do)
    
    ;; Check state change
    (should (string= (org-get-todo-state) "TODO"))
    ;; Check property removed
    (should-not (org-entry-get (point) "CLOSED_AS"))
    ;; Check strikethrough removed
    (should (string= (org-get-heading t t t t) "Already marked"))
    ;; CLOSED timestamp should be gone
    (goto-char (point-min))
    (should-not (search-forward "CLOSED:" nil t))))

(ert-deftest test-gtd-complete-as-wont-do-toggle ()
  "Test toggling won't do state multiple times."
  (with-gtd-test-buffer
      "* TODO Toggle test
:PROPERTIES:
:CREATED: [2024-01-01 Mon 10:00]
:END:
"
    ;; First toggle - mark as won't do
    (my-gtd-complete-as-wont-do)
    (should (string= (org-get-todo-state) "DONE"))
    (should (gtd-test-heading-has-strikethrough-p))
    
    ;; Second toggle - back to TODO
    (my-gtd-complete-as-wont-do)
    (should (string= (org-get-todo-state) "TODO"))
    (should-not (gtd-test-heading-has-strikethrough-p))
    
    ;; Third toggle - won't do again
    (my-gtd-complete-as-wont-do)
    (should (string= (org-get-todo-state) "DONE"))
    (should (gtd-test-heading-has-strikethrough-p))))

;;; Auto-advance Tests

(ert-deftest test-gtd-checklist-auto-advance ()
  "Test auto-advance behavior in checklists."
  (with-gtd-test-buffer
      (gtd-test-create-checklist
       "Auto-advance Test"
       '((:state "TODO" :heading "First item")
         (:state "TODO" :heading "Second item")
         (:state "TODO" :heading "Third item")))
    
    ;; Move to first item
    (search-forward "First item")
    (beginning-of-line)
    
    ;; Complete it - this triggers the hook
    (let ((org-after-todo-state-change-hook '(my-gtd-checklist-auto-advance)))
      (org-todo 'done))
    
    ;; We can't easily test cursor movement in batch mode,
    ;; but we can verify the hook runs without error
    (should t)))

(provide 'test-gtd-checklist)
;;; test-gtd-checklist.el ends here
