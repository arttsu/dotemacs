;;; test-gtd-integration.el --- Integration tests for GTD workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Master file that loads all GTD tests
;; This file is kept for backwards compatibility

;;; Code:

(require 'test-gtd-sorting)
(require 'test-gtd-checklist)

;;; Test Runner

(defun run-gtd-tests ()
  "Run all GTD integration tests."
  (interactive)
  (ert-run-tests-batch-and-exit "test-gtd-"))

(provide 'test-gtd-integration)
;;; test-gtd-integration.el ends here