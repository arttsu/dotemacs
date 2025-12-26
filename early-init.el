(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq package-enable-at-startup nil)

(load (expand-file-name "elpaca-bootstrap.el" user-emacs-directory) nil 'nomessage)

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;; Variables

;;;; Org Variables

(defvar my-org-dir nil)
(defvar my-org-day-agenda-include-shared-by-default nil)

;;;; UI Variables

(defvar my-ui-default-modus-theme nil)
(defvar my-ui-default-font nil)
(defvar my-ui-default-font-height nil)
(defvar my-ui-default-emoji-font nil)

;;;; Functionality Variables

(defvar my-jinx-languages nil)

;;;; Load Local Overrides

(let ((file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-readable-p file)
    (load file nil 'nomessage)))

;;; Load Custom File

(defconst my-custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-readable-p my-custom-file)
  (load-file my-custom-file))
