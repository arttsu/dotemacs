(require 'gptel)
(require 'my-org-helpers)

(define-derived-mode my-gptel-mode org-mode "GPTel")

(defun my-gptel-mode-setup ()
  (interactive)
  (org-mode)
  (gptel-mode))

(add-hook 'my-gptel-mode-hook 'my-gptel-mode-setup)

(defun my-gptel-clear-buffer ()
  (interactive)
  (erase-buffer)
  (insert "*** "))

(defun my-gptel-send ()
  (interactive)
  (goto-char (point-max))
  (gptel-send)
  (org-back-to-heading)
  (recenter-top-bottom 0))

(defun my-gptel-post-response (&rest args)
  (my-org-auto-fill-buffer))

(add-hook 'gptel-post-response-hook 'my-gptel-post-response)

(provide 'my-gptel)
