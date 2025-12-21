(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq package-enable-at-startup nil)

(load (expand-file-name "elpaca-bootstrap.el" user-emacs-directory) nil 'nomessage)

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(defun my-load-local-init ()
  "Load local init file if exists."
  (let ((file (expand-file-name "local.el" user-emacs-directory)))
    (when (file-readable-p file)
      (load file nil 'nomessage))))

(add-hook 'elpaca-after-init-hook #'my-load-local-init)
