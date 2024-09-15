(defun my-avy-embark (point)
  (goto-char point)
  (embark-act))

(defun my-avy-embark-dwim (point)
  (goto-char point)
  (embark-dwim))

(provide 'my-avy-embark)
