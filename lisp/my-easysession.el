;;; my-easysession.el --- Easysession Helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun my-easysession-visible-buffer-list ()
  "Return a list of all buffers considered visible in the current session.

A buffer is included if it satisfies any of the following:
- It is currently displayed in a visible window.
- It is associated with a visible tab in `tab-bar-mode', if enabled.
- It is the *scratch* buffer (included as a special case).

The returned list contains live buffers only."
  (let ((visible-buffers '()))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (or ;; Exception: The scratch buffer
                  (string= (buffer-name buffer) "*scratch*")
                  ;; Windows
                  (get-buffer-window buffer 'visible)
                  ;; Tab-bar windows
                  (and (bound-and-true-p tab-bar-mode)
                       (fboundp 'tab-bar-get-buffer-tab)
                       (tab-bar-get-buffer-tab buffer t nil))))
        (push buffer visible-buffers)))
    visible-buffers))

(defun my-easysession-do-reset ()
  "Reset the current session to the default state after Emacs startup."
  (when (and (boundp 'tab-bar-mode) tab-bar-mode)
    (tab-bar-close-other-tabs)
    (tab-bar-rename-tab ""))
  (delete-other-windows)
  (scratch-buffer))

(defun my-easysession-reset ()
  "Ask for confirmation and reset the current session."
  (interactive)
  (when (yes-or-no-p "Reset the current session?")
    (my-easysession-do-reset)))

(defun my-easysession-switch-to-main ()
  "Switch to the session 'main'."
  (interactive)
  (easysession-switch-to-and-restore-geometry "main"))

(provide 'my-easysession)

;;; my-easysession.el ends here.
