;;; test-gtd-capture.el --- Tests for GTD capture system -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for GTD capture templates and functions

;;; Code:

(require 'ert)
(require 'org)
(load-file "tests/test-gtd-utils.el")

;;; Template Path Resolution Tests

(ert-deftest test-gtd-capture-template-path-resolver ()
  "Test that template path resolver works correctly."
  (let ((expected-path (expand-file-name "capture-templates/gtd-note.txt" user-emacs-directory)))
    (should (string= (my-org-capture-template-path "gtd-note") expected-path))))

(ert-deftest test-gtd-capture-template-files-exist ()
  "Test that all capture template files exist."
  (let ((templates '("gtd-note" "gtd-note-link" "gtd-todo" "gtd-todo-link"
                     "gtd-project" "gtd-area")))
    (dolist (template templates)
      (let ((path (my-org-capture-template-path template)))
        (should (file-exists-p path))))))

;;; Template Content Tests

(ert-deftest test-gtd-capture-template-content-note ()
  "Test that note template has correct content structure."
  (let ((template-path (my-org-capture-template-path "gtd-note")))
    (with-temp-buffer
      (insert-file-contents template-path)
      (let ((content (buffer-string)))
        (should (string-match-p "\\* %\\?" content))
        (should (string-match-p ":PROPERTIES:" content))
        (should (string-match-p ":CREATED: %U" content))
        (should (string-match-p ":END:" content))))))

(ert-deftest test-gtd-capture-template-content-todo ()
  "Test that todo template has correct TODO keyword."
  (let ((template-path (my-org-capture-template-path "gtd-todo")))
    (with-temp-buffer
      (insert-file-contents template-path)
      (let ((content (buffer-string)))
        (should (string-match-p "\\* TODO %\\?" content))
        (should (string-match-p ":CREATED: %U" content))))))

(ert-deftest test-gtd-capture-template-content-link-variants ()
  "Test that link templates include link placeholders."
  (let ((note-link-path (my-org-capture-template-path "gtd-note-link"))
        (todo-link-path (my-org-capture-template-path "gtd-todo-link")))
    ;; Note link template
    (with-temp-buffer
      (insert-file-contents note-link-path)
      (let ((content (buffer-string)))
        (should (string-match-p "\\[\\[%c\\]\\[%\\?\\]\\]" content))))
    ;; Todo link template
    (with-temp-buffer
      (insert-file-contents todo-link-path)
      (let ((content (buffer-string)))
        (should (string-match-p "\\* TODO \\[\\[%c\\]\\[%\\?\\]\\]" content))))))

;;; Capture Configuration Tests

(ert-deftest test-gtd-capture-templates-defined ()
  "Test that capture templates are properly defined."
  (should (boundp 'my-gtd-capture-templates))
  (should (listp my-gtd-capture-templates))
  (should (> (length my-gtd-capture-templates) 0)))

(ert-deftest test-gtd-capture-templates-structure ()
  "Test that capture templates have expected structure."
  (let ((templates my-gtd-capture-templates))
    ;; Should have inbox section
    (should (member '("i" "Inbox") templates))
    ;; Should have note templates
    (should (cl-some (lambda (template) 
                       (and (listp template) (string= (car template) "ii")))
                     templates))
    ;; Should have todo templates
    (should (cl-some (lambda (template) 
                       (and (listp template) (string= (car template) "iI")))
                     templates))))

(ert-deftest test-gtd-local-inbox-target-defined ()
  "Test that local inbox target is properly defined."
  (should (boundp 'my-gtd-local-inbox-target))
  (should (listp my-gtd-local-inbox-target))
  (should (eq (car my-gtd-local-inbox-target) 'file+headline)))

;;; Capture Function Tests

(ert-deftest test-gtd-capture-functions-defined ()
  "Test that capture wrapper functions are defined."
  (should (fboundp 'my-gtd-capture-note))
  (should (fboundp 'my-gtd-capture-todo)))

(ert-deftest test-gtd-capture-function-interactive ()
  "Test that capture functions are interactive."
  (should (commandp 'my-gtd-capture-note))
  (should (commandp 'my-gtd-capture-todo)))

;;; Mock Capture Tests (testing function logic without actual capture)

(ert-deftest test-gtd-capture-note-prefix-handling ()
  "Test that capture note function handles prefix arguments correctly."
  ;; Mock org-capture to avoid actual capture during tests
  (cl-letf (((symbol-function 'org-capture) 
             (lambda (goto keys)
               (list 'captured goto keys))))
    ;; Test no prefix (should use "ii")
    (let ((current-prefix-arg nil))
      (should (equal (my-gtd-capture-note) '(captured nil "ii"))))
    ;; Test C-u prefix (should use "il")  
    (let ((current-prefix-arg '(4)))
      (should (equal (my-gtd-capture-note '(4)) '(captured nil "il"))))))

(ert-deftest test-gtd-capture-todo-prefix-handling ()
  "Test that capture todo function handles prefix arguments correctly."
  ;; Mock org-capture to avoid actual capture during tests
  (cl-letf (((symbol-function 'org-capture) 
             (lambda (goto keys)
               (list 'captured goto keys))))
    ;; Test no prefix (should use "iI")
    (let ((current-prefix-arg nil))
      (should (equal (my-gtd-capture-todo) '(captured nil "iI"))))
    ;; Test C-u prefix (should use "iL")
    (let ((current-prefix-arg '(4)))
      (should (equal (my-gtd-capture-todo '(4)) '(captured nil "iL"))))))

(ert-deftest test-gtd-capture-invalid-prefix ()
  "Test that capture functions handle invalid prefix arguments."
  (cl-letf (((symbol-function 'org-capture) 
             (lambda (goto keys)
               (list 'captured goto keys))))
    ;; Test invalid prefix for note
    (should-error (my-gtd-capture-note '(16)) :type 'error)
    ;; Test invalid prefix for todo  
    (should-error (my-gtd-capture-todo '(16)) :type 'error)))

;;; Integration Tests (require careful setup)

(ert-deftest test-gtd-capture-template-resolution-in-config ()
  "Test that templates resolve correctly when used in capture config."
  ;; This tests that the template files can be found when referenced in config
  (let ((templates my-gtd-capture-templates))
    (dolist (template templates)
      (when (and (listp template) (> (length template) 4) 
                 (eq (car (nth 4 template)) 'file))
        ;; Extract file path from template definition
        (let ((file-expr (cadr (nth 4 template))))
          (when (and (listp file-expr) 
                     (eq (car file-expr) 'my-org-capture-template-path))
            (let ((template-name (cadr file-expr)))
              (should (file-exists-p (my-org-capture-template-path template-name))))))))))

(provide 'test-gtd-capture)
;;; test-gtd-capture.el ends here