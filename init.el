(load (expand-file-name "local.el" user-emacs-directory))

(setq straight-fix-flycheck t)

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

(straight-use-package 'org)

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-headings '((0 . (ultrabold 1.3))
			   (1 . (ultrabold 1.2))
			   (2 . (extrabold 1.1))
			   (t . (extrabold))))
  (modus-themes-variable-pitch-ui t)
  :config
  (modus-themes-load-theme 'modus-vivendi))

(defun my/kill-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))

(defun my/kill-to-beginning-of-buffer ()
  (interactive)
  (kill-region (point-min) (point)))

(defun my/kill-to-word (&optional n)
  (interactive "p")
  (kill-region (point) (save-excursion (forward-to-word (or n 1)) (point))))

(use-package emacs
  :custom
  (create-lockfiles nil)
  (make-backup-files nil)
  (disabled-command-function nil)
  (inhibit-splash-screen t)
  (initial-scratch-message "All systems go! 🚀")
  (initial-major-mode 'text-mode)
  :bind
  (("C-c x x" . erase-buffer)
   ("C-c x >" . my/kill-to-end-of-buffer)
   ("C-c x <" . my/kill-to-beginning-of-buffer)
   ("C-c x w" . my/kill-to-word)))

(use-package super-save
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-exclude '(".sbt" "project/"))
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-all-buffers t)
  (auto-save-default nil)
  :config
  (super-save-mode))

(use-package whole-line-or-region
  :demand
  :config
  (whole-line-or-region-global-mode)
  :bind
  (("M-/" . whole-line-or-region-comment-dwim)))

(use-package vertico
  :config
  (vertico-mode))

(use-package vertico-posframe
  :after vertico
  :config
  (vertico-posframe-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (aw-scope 'frame)
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
   ("C-M-; c" . avy-copy-line)
   ("C-M-; C" . avy-copy-region)
   ("C-M-; m" . avy-move-line)
   ("C-M-; M" . avy-move-region)
   ("C-M-; k" . avy-kill-whole-line)
   ("C-M-; K" . avy-kill-region)
   ("C-M-; s" . avy-kill-ring-save-whole-line)
   ("C-M-; S" . avy-kill-ring-save-region)
   :map isearch-mode-map
   ("C-;" . avy-isearch)))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  :bind
  (:map smartparens-mode-map
	("C-<right>" . sp-forward-slurp-sexp)
	("C-<left>" . sp-backward-slurp-sexp)
	("M-<right>" . sp-forward-barf-sexp)
	("M-<left>" . sp-backward-barf-sexp)
	("M-a" . sp-beginning-of-sexp)
	("M-e" . sp-end-of-sexp)
	("C-M-u" . sp-up-sexp)
	("C-M-S-u" . sp-backward-up-sexp)
	("C-M-d" . sp-down-sexp)
	("C-M-S-d" . sp-backward-down-sexp)
	("C-c p u" . sp-unwrap-sexp)
	("C-c p r" . sp-rewrap-sexp)
	("C-c p [" . sp-wrap-square)
	("C-c p {" . sp-wrap-curly)
	("C-c p (" . sp-wrap-round)))

(use-package multiple-cursors
  :bind
  (("C-+" . mc/mark-next-like-this)
   ("C--" . mc/mark-previous-like-this)
   ("C-c k l" . mc/edit-lines)
   ("C-c k m" . mc/mark-all-dwim)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-<return>" . set-rectangular-region-anchor)))

(use-package crux
  :bind
  (("C-o" . crux-smart-open-line)
   ("C-S-o" . crux-smart-open-line-above)
   ("C-^" . crux-top-join-line)
   ("C-M-; D" . crux-duplicate-and-comment-current-line-or-region)))

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g M" . consult-global-mark)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s f" . consult-focus-lines)))

(use-package magit
  :bind
  (("C-c g" . magit-file-dispatch)))

(use-package org
  :defer t
  :custom
  (org-confirm-babel-evaluate nil)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-use-sub-superscripts '{})
  (org-attach-directory "~/org/attach")
  (org-attach-use-inheritance t)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  :config
  (require 'org-attach)
  (add-to-list 'org-export-backends 'md)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)))

(use-package org-modern
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-agenda-tags-column 0)
  :config
  (global-org-modern-mode))
