;;; my-emacs.el --- Helpers for vanilla Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun my-jump-home ()
  "Find the home directory."
  (interactive)
  (find-file "~/"))

(defun my-pop-mark ()
  "Pop the last mark."
  (interactive)
  (set-mark-command '(4)))

(provide 'my-emacs)

;;; my-emacs.el ends here
