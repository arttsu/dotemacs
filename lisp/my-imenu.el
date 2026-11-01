;;; my-imenu.el --- Imenu helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun my-imenu-elisp-index ()
  "Return an Imenu index for Emacs Lisp buffers."
  (let* ((section-regex (rx line-start (>= 3 ";") (+ blank) (group (* not-newline)) line-end))
         (section-rules `(("Sections" ,section-regex 1))))
    (append (imenu--generic-function section-rules)
            (imenu-default-create-index-function))))

(defun my-imenu-setup-elisp ()
  "Use custom Imenu index in Emacs Lisp Mode."
  (setq-local imenu-create-index-function #'my-imenu-elisp-index))

(provide 'my-imenu)

;;; my-imenu.el ends here
