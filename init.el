(use-package emacs
  :custom
  (create-lockfiles nil)
  (make-backup-files nil)
  (inhibit-splash-screen t)
  (initial-scratch-message "All systems go! 🚀")
  (initial-major-mode 'text-mode))

(defvar bootstrap-version)
(let ((bootstrap-file
	 (expand-file-name
	  "straight/repos/straight.el/bootstrap.el"
	  (or (bound-and-true-p straight-base-dir)
	      user-emacs-directory)))
	(bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(use-package org
  :defer t
  :custom
  (org-confirm-babel-evaluate nil))

(use-package magit
  :bind
  (("C-c g" . magit-file-dispatch)))

(use-package vertico
  :config
  (vertico-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s k" . consult-keep-lines)
   ("M-s f" . consult-focus-lines)))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (aw-scope 'global)
  :bind
  (("M-o" . ace-window)))

(use-package avy
  :custom
  (avy-single-candidate-jump t)
  :bind
  (("C-;" . avy-goto-char-timer)
   ("M-;" . avy-pop-mark)
   ("M-g g" . avy-goto-line)
   ("M-g G" . avy-goto-end-of-line)
   ("M-g h" . avy-org-goto-heading-timer)
   ("M-g w" . avy-goto-word-1)
   :map isearch-mode-map
   ("C-;" . avy-isearch)))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))
