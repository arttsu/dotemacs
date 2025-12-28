;;; my-anki.el --- Custom Anki Mode and Helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(define-derived-mode my-anki-mode org-mode "Anki")

(add-to-list 'auto-mode-alist '("\\.anki\\'" . my-anki-mode))

(defun my-anki-mode-init ()
  "Initialize my-anki-mode in a new buffer."
  ;; Anki Editor requires Org mode.
  (let ((major-mode 'org-mode))
    (anki-editor-mode +1)))

(provide 'my-anki)

;;; my-anki.el ends here.
