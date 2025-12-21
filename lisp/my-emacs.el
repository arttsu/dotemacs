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

(defun my-jump-to-custom ()
  "Find the custom file."
  (interactive)
  (find-file my-custom-file))

(defvar-keymap tab-bar-history-repeat-map
  :repeat t
  "<left>" #'tab-bar-history-back
  "<right>" #'tab-bar-history-forward)

(provide 'my-emacs)

;;; my-emacs.el ends here
