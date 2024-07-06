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

(defun my-gptel-post-response (&rest args)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (org-forward-paragraph)
      (let ((elem (org-element-at-point)))
        (when (member (org-element-type elem) '(paragraph item))
          (save-excursion
            (goto-char (org-element-property :begin elem))
            (fill-paragraph)))
        (when (string= (org-element-type elem) 'plain-list)
          (save-excursion
            (org-backward-paragraph)
            (fill-paragraph)))))))

(add-hook 'gptel-post-response-hook 'my-gptel-post-response)

(provide 'my-gptel)
