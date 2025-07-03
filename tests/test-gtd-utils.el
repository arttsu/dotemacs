;;; test-gtd-utils.el --- Shared utilities for GTD tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Common utilities and helpers for GTD integration tests

;;; Code:

(require 'ert)
(require 'org)

;;; Test Buffer Creation

(defmacro with-gtd-test-buffer (content &rest body)
  "Create a temporary org buffer with CONTENT and execute BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

;;; Content Extraction Helpers

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

(defun gtd-test-get-properties (property)
  "Get list of PROPERTY values for all entries."
  (let (values)
    (org-map-entries
     (lambda ()
       (push (org-entry-get (point) property) values))
     nil nil)
    (nreverse values)))

;;; Test Data Builders

(defun gtd-test-create-checklist (title items)
  "Create a checklist with TITLE and ITEMS.
ITEMS should be a list of plists with :state, :priority, :heading, :created, etc."
  (concat "* " title "\n"
          ":PROPERTIES:\n"
          ":STYLE: checklist\n"
          ":END:\n"
          "\n"
          (mapconcat
           (lambda (item)
             (concat "** " (or (plist-get item :state) "TODO")
                     (if-let ((priority (plist-get item :priority)))
                         (format " [#%s]" priority)
                       "")
                     " " (plist-get item :heading) "\n"
                     (when (string= (plist-get item :state) "DONE")
                       (format "CLOSED: %s\n" (or (plist-get item :closed)
                                                   "[2024-01-01 Mon 12:00]")))
                     ":PROPERTIES:\n"
                     (format ":CREATED: %s\n" (or (plist-get item :created)
                                                   "[2024-01-01 Mon 10:00]"))
                     (when-let ((closed-as (plist-get item :closed-as)))
                       (format ":CLOSED_AS: %s\n" closed-as))
                     ":END:\n"
                     "\n"))
           items
           "\n")))

(defun gtd-test-create-log (title entries)
  "Create a log with TITLE and ENTRIES.
ENTRIES should be a list of plists with :heading and :created."
  (concat "* " title "\n"
          ":PROPERTIES:\n"
          ":STYLE: log\n"
          ":END:\n"
          "\n"
          (mapconcat
           (lambda (entry)
             (concat "** " (plist-get entry :heading) "\n"
                     ":PROPERTIES:\n"
                     (format ":CREATED: %s\n" (plist-get entry :created))
                     ":END:\n"
                     "\n"))
           entries
           "\n")))

;;; Assertion Helpers

(defun gtd-test-has-property-p (property value)
  "Check if current entry has PROPERTY with VALUE."
  (string= (org-entry-get (point) property) value))

(defun gtd-test-heading-has-strikethrough-p ()
  "Check if current heading has strikethrough formatting."
  (let ((heading (org-get-heading t t t t)))
    (string-match "^\\+.*\\+$" heading)))

;;; User Input Simulation

(defmacro with-simulated-input (input &rest body)
  "Execute BODY with INPUT as simulated user input."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'yes-or-no-p)
              (lambda (_prompt) ,input))
             ((symbol-function 'y-or-n-p)
              (lambda (_prompt) ,input)))
     ,@body))

(provide 'test-gtd-utils)
;;; test-gtd-utils.el ends here