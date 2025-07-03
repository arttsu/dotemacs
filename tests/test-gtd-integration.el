;;; test-gtd-integration.el --- Integration tests for GTD workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Master file that loads all GTD tests
;; This file is kept for backwards compatibility

;;; Code:

(require 'test-gtd-sorting)
(require 'test-gtd-checklist)
(require 'test-gtd-project-lifecycle)
(require 'test-gtd-capture)
(require 'test-gtd-agenda)

;;; Test Runner

(defun run-gtd-tests ()
  "Run all GTD integration tests."
  (interactive)
  (let ((result (ert-run-tests-batch "test-gtd-")))
    (princ "\n")
    (if (and result (= (ert--stats-failed-unexpected result) 0))
        (princ "✅ All tests passed!\n")
      (princ "❌ Some tests failed!\n"))))

(provide 'test-gtd-integration)
;;; test-gtd-integration.el ends here
