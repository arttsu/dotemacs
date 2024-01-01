(defun my-mac-p ()
  (equal system-type 'darwin))

(defun my-windows-p ()
  (equal system-type 'windows-nt))

(load (expand-file-name "local.el" user-emacs-directory))

(use-package emacs
  :custom
  (inhibit-splash-screen t)
  (initial-scratch-message "Ready to go!")
  (initial-major-mode 'text-mode)
  (create-lockfiles nil)
  (make-backup-files nil)
  (global-auto-revert-non-file-buffers t)
  (disabled-command-function nil)
  (save-interprogram-paste-before-kill t)
  (gc-cons-threshold 100000000) ;; 100mb
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (visible-bell t)
  (require-final-newline t)
  (indent-tabs-mode nil)
  :config
  (global-auto-revert-mode)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (global-subword-mode)
  :bind
  (("C-c s" . scratch-buffer)
   ("C-x C-b" . ibuffer-other-window)))

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

(use-package magit
  :bind
  (("C-c g" . magit-file-dispatch)))

(unless (my-windows-p)
  (set-face-attribute 'default nil :font "Iosevka Comfy Wide Fixed" :height my-default-font-height)
  (set-frame-font "Iosevka Comfy Wide Fixed" nil t))

(use-package modus-themes
  :custom
  (modus-themes-bold-constructs nil)
  (modus-themes-italic-constructs t)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-headings '((0 . (ultrabold 1.3))
			   (1 . (ultrabold 1.2))
			   (2 . (extrabold 1.1))
			   (t . (extrabold))))
  (modus-themes-variable-pitch-ui t)
  :config
  (modus-themes-load-theme 'modus-vivendi))

(defun my-capture-template (name)
  (expand-file-name (format "capture_templates/%s.txt" name) user-emacs-directory))

(defvar my-capture-prompt-history nil)

(defun my-capture-prompt (prompt var)
  (make-local-variable var)
  (set var (read-string (concat prompt ": ") nil my-capture-prompt-history)))

(defun my-gtd-file (name)
  (format "~/org/gtd/%s.org" name))

(defun my-capture-inbox ()
  (interactive)
  (org-capture nil "i"))

(setq my-inbox-file "~/org/inbox.org")

(setq my-calendar-file (my-gtd-file "calendar")
      my-tasks-file (my-gtd-file "tasks")
      my-projects-file (my-gtd-file "projects")
      my-someday-file (my-gtd-file "someday")
      my-gtd-files `(,my-calendar-file ,my-tasks-file ,my-projects-file ,my-someday-file))

(setq my-gtd-capture-templates
      (list
       `("i" "Inbox" entry (file ,my-inbox-file) (file ,(my-capture-template "inbox")))
       `("p" "Project" entry (file ,my-projects-file) (file ,(my-capture-template "project")))
       `("s" "Someday folder" entry (file ,my-someday-file) (file ,(my-capture-template "someday_folder")))))

(setq my-gtd-refile-targets `(,my-gtd-files :level . 1))

(setq my-day-agenda
      `("d"
	"Day"
	((agenda "" ((org-agenda-span 1)
		     (org-agenda-skip-scheduled-if-done t)
		     (org-agenda-skip-deadline-if-done t)
		     (org-agenda-skip-timestamp-if-done t)
		     (org-agenda-files '(,my-calendar-file ,my-tasks-file ,my-projects-file ,my-someday-file))))
	 (todo "TODO" ((org-agenda-overriding-header "Not-scheduled Tasks")
		       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
		       (org-agenda-files '(,my-calendar-file ,my-tasks-file))))
	 (tags "NOW+LEVEL=1" ((org-agenda-overriding-header "Projects")
			      (org-agenda-files '(,my-projects-file)))))))

(use-package org
  :defer t
  :custom
  (org-confirm-babel-evaluate nil)
  (org-startup-indented t)
  (org-agenda-files my-gtd-files)
  (org-agenda-custom-commands (list my-day-agenda))
  (org-capture-templates my-gtd-capture-templates)
  (org-refile-targets (list my-gtd-refile-targets))
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c i" . my-capture-inbox)
   ("C-c l" . org-store-link)))

(use-package org-modern
  :after org
  :config
  (global-org-modern-mode))

(use-package smartparens
  :hook (emacs-lisp-mode . smartparens-mode)
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
	("C-c p {" . sp-wrap-curly)
	("C-c p [" . sp-wrap-square)
	("C-c p (" . sp-wrap-round)
	("C-c p r" . sp-rewrap-sexp)))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  :bind
  (("M-o" . ace-window)))

(use-package vertico
  :config
  (vertico-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-complition)))))

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
   ("M-s k" . consult-keep-lines)))

(use-package whole-line-or-region
  :demand
  :config
  (whole-line-or-region-global-mode)
  :bind
  (("M-/" . whole-line-or-region-comment-dwim)))

(use-package crux
  :bind
  (("C-o" . crux-smart-open-line)
   ("C-S-o" . crux-smart-open-line-above)
   ("C-^" . crux-top-join-line)
   ("C-M-; D" . crux-duplicate-and-comment-current-line-or-region)))

(use-package multiple-cursors
  :bind
  (("C-+" . mc/mark-next-like-this)
   ("C-c k l" . mc/edit-lines)
   ("C-c k m" . mc/mark-all-dwim)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-<return>" . set-rectangular-region-anchor)))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

(use-package iy-go-to-char
  :bind
  (("M-g f" . iy-go-to-char)
   ("M-g F" . iy-go-to-char-backward)
   ("M-g t" . iy-go-up-to-char)
   ("M-g T" . iy-go-up-to-char-backward)
   ("M-g ;" . iy-go-to-or-up-to-continue)
   ("M-g ," . iy-go-to-or-up-to-continue-backward)))

(use-package company
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.2)
  (company-selection-wrap-around t)
  (company-dabbrev-downcase nil)
  (company-show-numbers t)
  :config
  (global-company-mode))

(use-package super-save
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-exclude '(".sbt" "project/"))
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-all-buffers t)
  (auto-save-default nil)
  :config
  (super-save-mode)
  (add-to-list 'super-save-triggers 'ace-window))
