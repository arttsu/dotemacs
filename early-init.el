(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq package-enable-at-startup nil)

(load (expand-file-name "elpaca-bootstrap.el" user-emacs-directory) nil 'nomessage)

(elpaca elpaca-use-package
  (elpaca-use-package-mode))
