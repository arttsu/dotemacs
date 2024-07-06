(defun my-org-auto-fill-buffer ()
  (interactive)
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

(provide 'my-org-helpers)
