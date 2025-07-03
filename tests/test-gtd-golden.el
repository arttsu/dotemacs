;;; test-gtd-golden.el --- Golden file testing utilities for GTD agenda -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for golden file testing of GTD agenda output
;; Provides snapshot testing capabilities with normalization and comparison

;;; Code:

(require 'ert)
(require 'org)
(require 'org-agenda)

;;; Golden File Infrastructure

(defvar gtd-golden-test-data-dir
  (expand-file-name "test-data" (file-name-directory load-file-name))
  "Directory for storing golden files and test data.")

(defun gtd-golden-ensure-test-data-dir ()
  "Ensure test data directory exists."
  (unless (file-exists-p gtd-golden-test-data-dir)
    (make-directory gtd-golden-test-data-dir t))
  gtd-golden-test-data-dir)

(defun gtd-golden-get-file-path (test-name &optional suffix)
  "Get path for golden file for TEST-NAME with optional SUFFIX."
  (let ((dir (gtd-golden-ensure-test-data-dir)))
    (expand-file-name 
     (format "%s%s.txt" test-name (or suffix ""))
     dir)))

;;; Content Normalization

(defun gtd-golden-normalize-agenda-content (content)
  "Normalize agenda CONTENT for consistent comparison.
Removes or standardizes dynamic elements like dates, paths, and timestamps."
  (let ((normalized content))
    ;; Normalize file paths - replace with placeholder
    (setq normalized (replace-regexp-in-string
                     "/tmp/[^/]+/gtd-agenda-test[^/]*"
                     "/test-temp"
                     normalized))
    
    ;; Normalize actual dates to test date
    (setq normalized (replace-regexp-in-string
                     "\\[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\}"
                     "2024-01-15 Mon"
                     normalized))
    
    ;; Normalize "Day-agenda (W03):" style headers to just "Day-agenda:"
    (setq normalized (replace-regexp-in-string
                     "Day-agenda (W[0-9]+):"
                     "Day-agenda:"
                     normalized))
    
    ;; Normalize time stamps in agenda
    (setq normalized (replace-regexp-in-string
                     "[0-9]\\{1,2\\}:[0-9]\\{2\\}"
                     "HH:MM"
                     normalized))
    
    ;; Remove trailing whitespace from lines
    (setq normalized (replace-regexp-in-string
                     "[ \t]+$"
                     ""
                     normalized))
    
    ;; Normalize multiple consecutive newlines
    (setq normalized (replace-regexp-in-string
                     "\n\n\n+"
                     "\n\n"
                     normalized))
    
    ;; Trim final newlines for consistent comparison
    (string-trim normalized)))

(defun gtd-golden-capture-agenda-buffer ()
  "Capture and normalize agenda buffer content."
  (unless (get-buffer "*Org Agenda*")
    (error "No agenda buffer found"))
  
  (with-current-buffer "*Org Agenda*"
    (let ((raw-content (buffer-string)))
      (gtd-golden-normalize-agenda-content raw-content))))

;;; Golden File Operations

(defun gtd-golden-read-file (filepath)
  "Read golden file at FILEPATH, return nil if file doesn't exist."
  (when (file-exists-p filepath)
    (with-temp-buffer
      (insert-file-contents filepath)
      (buffer-string))))

(defun gtd-golden-write-file (filepath content)
  "Write CONTENT to file at FILEPATH."
  (with-temp-file filepath
    (insert content)))

(defun gtd-golden-compare-content (expected actual)
  "Compare EXPECTED and ACTUAL content, return diff info or nil if same."
  (if (string= expected actual)
      nil
    (let ((expected-lines (split-string expected "\n"))
          (actual-lines (split-string actual "\n"))
          (diffs '()))
      
      ;; Simple line-by-line diff
      (let ((max-lines (max (length expected-lines) (length actual-lines))))
        (dotimes (i max-lines)
          (let ((exp-line (nth i expected-lines))
                (act-line (nth i actual-lines)))
            (unless (string= (or exp-line "") (or act-line ""))
              (push (list :line (1+ i)
                         :expected (or exp-line "")
                         :actual (or act-line ""))
                    diffs)))))
      
      (nreverse diffs))))

(defun gtd-golden-format-diff (diffs)
  "Format DIFFS into human-readable string."
  (if (null diffs)
      "No differences found"
    (with-temp-buffer
      (insert (format "Found %d line differences:\n\n" (length diffs)))
      (dolist (diff diffs)
        (let ((line (plist-get diff :line))
              (expected (plist-get diff :expected))
              (actual (plist-get diff :actual)))
          (insert (format "Line %d:\n" line))
          (insert (format "  Expected: %S\n" expected))
          (insert (format "  Actual:   %S\n\n" actual))))
      (buffer-string))))

;;; Main Golden File Testing Function

(defun gtd-golden-test-agenda (test-name)
  "Test agenda output against golden file for TEST-NAME.
Returns t if test passes, otherwise signals test failure with diff info."
  (let* ((golden-file (gtd-golden-get-file-path test-name))
         (failed-file (gtd-golden-get-file-path test-name ".FAILED"))
         (actual-content (gtd-golden-capture-agenda-buffer))
         (expected-content (gtd-golden-read-file golden-file)))
    
    ;; Write actual content to .FAILED file for inspection
    (gtd-golden-write-file failed-file actual-content)
    
    (if (null expected-content)
        (error "Golden file not found: %s\n\nActual output written to: %s\n\nTo create golden file, review the output and run:\ncp %s %s"
               golden-file failed-file failed-file golden-file)
      
      (let ((diffs (gtd-golden-compare-content expected-content actual-content)))
        (if diffs
            (let ((diff-msg (gtd-golden-format-diff diffs)))
              (error "Golden file test failed for %s\n\n%s\nExpected: %s\nActual: %s\n\nTo update golden file if changes are intentional:\ncp %s %s"
                     test-name diff-msg golden-file failed-file failed-file golden-file))
          
          ;; Test passed - clean up .FAILED file
          (when (file-exists-p failed-file)
            (delete-file failed-file))
          t)))))

;;; Golden File Creation Helper

(defun gtd-golden-create-golden-file (test-name &optional force)
  "Create golden file for TEST-NAME from current agenda buffer.
If FORCE is non-nil, overwrite existing golden file."
  (let* ((golden-file (gtd-golden-get-file-path test-name))
         (content (gtd-golden-capture-agenda-buffer)))
    
    (when (or force 
              (not (file-exists-p golden-file))
              (y-or-n-p (format "Golden file %s exists. Overwrite? " golden-file)))
      
      (gtd-golden-write-file golden-file content)
      (message "Golden file created: %s" golden-file)
      golden-file)))

;;; Debug Utilities

(defun gtd-golden-debug-content (test-name)
  "Debug utility to show raw and normalized content for TEST-NAME."
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (let ((raw (buffer-string))
            (normalized (gtd-golden-normalize-agenda-content (buffer-string))))
        
        (with-current-buffer (get-buffer-create "*GTD Golden Debug*")
          (erase-buffer)
          (insert (format "=== RAW AGENDA CONTENT ===\n%s\n\n" raw))
          (insert (format "=== NORMALIZED CONTENT ===\n%s\n\n" normalized))
          (insert (format "=== GOLDEN FILE PATH ===\n%s\n" (gtd-golden-get-file-path test-name)))
          (goto-char (point-min)))
        
        (display-buffer "*GTD Golden Debug*")))))

(provide 'test-gtd-golden)
;;; test-gtd-golden.el ends here