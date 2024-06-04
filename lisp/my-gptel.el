(require 'gptel)

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

(provide 'my-gptel)
