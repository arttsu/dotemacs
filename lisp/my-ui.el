;;; my-ui.el --- UI helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun my-ui-set-theme-and-font ()
  "Set Modus theme and font on startup."
  (and my-ui-default-modus-theme (modus-themes-load-theme my-ui-default-modus-theme))
  (when (and my-ui-default-font my-ui-default-font-height)
    (set-face-attribute 'default nil :font my-ui-default-font :height my-ui-default-font-height)
    (set-frame-font my-ui-default-font nil t))
  (when my-ui-default-emoji-font
    (set-fontset-font t 'unicode my-ui-default-emoji-font nil 'append)))

(provide 'my-ui)

;;; my-ui.el ends here
