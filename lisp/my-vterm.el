;;; my-vterm.el --- Vterm helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun my-vterm-unbind-keys ()
  "Remove some of the self-insert bindings that conflict with useful global bindings."
  (local-unset-key (kbd "<f1>"))
  (local-unset-key (kbd "<f2>"))
  (local-unset-key (kbd "<f3>"))
  (local-unset-key (kbd "<f4>"))
  (local-unset-key (kbd "<f5>"))
  (local-unset-key (kbd "<f6>"))
  (local-unset-key (kbd "<f7>"))
  (local-unset-key (kbd "<f8>"))
  (local-unset-key (kbd "<f9>"))
  (local-unset-key (kbd "<f10>"))
  (local-unset-key (kbd "<f11>"))
  (local-unset-key (kbd "<f12>"))
  (local-unset-key (kbd "M-s")))

(provide 'my-vterm)

;;; my-vterm.el ends here.
