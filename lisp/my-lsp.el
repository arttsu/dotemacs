;;; my-lsp.el --- LSP Mode Helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun my-lsp-setup-corfu ()
  "Setup Corfu completions in LSP mode."
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))

(provide 'my-lsp)

;;; my-lsp.el ends here
