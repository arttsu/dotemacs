(defface my-org-checkbox-done-text
  '((t (:inherit org-done)))
  "Face for the text part of a checked Org Mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'my-org-checkbox-done-text prepend))
 'append)

(provide 'my-org-looks)
