(defun string-in-list-p (str lst)
  (require 'cl-lib)
  (cl-find-if (lambda (item) (string-equal str item)) lst))

(provide 'my-elisp-helpers)
