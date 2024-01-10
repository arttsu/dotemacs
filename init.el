(defun my-mac-p ()
  (equal system-type 'darwin))

(defun my-windows-p ()
  (equal system-type 'windows-nt))

(load (expand-file-name "local.el" user-emacs-directory))

(use-package emacs
  :custom
  (create-lockfiles nil)
  (make-backup-files nil)
  (inhibit-splash-screen t)
  (initial-scratch-message "All systems go!")
  (initial-major-mode 'text-mode)
  (global-auto-revert-non-file-buffers t)
  (disabled-command-function nil)
  (save-interprogram-paste-before-kill t)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024))
  (visible-bell t)
  (require-final-newline t)
  (indent-tabs-mode nil)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (global-auto-revert-mode)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (global-subword-mode)
  (when (file-exists-p custom-file)
    (load custom-file))
  :bind
  (("C-x C-M-b" . scratch-buffer)
   ("C-x C-b" . ibuffer-other-window)
   ("C-M-; d" . duplicate-dwim)
   ("M-g w" . forward-to-word)
   ("M-g W" . backward-to-word)
   ("C-c x x" . erase-buffer)))

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

(use-package exec-path-from-shell
  :if (my-mac-p)
  :config
  (exec-path-from-shell-initialize))

(use-package magit
  :bind
  (("C-c g" . magit-file-dispatch)))

(defun my-capture-template (name)
  (expand-file-name (format "capture_templates/%s.txt" name) user-emacs-directory))

(defvar my-capture-prompt-history nil)

(defun my-capture-prompt (prompt var)
  (make-local-variable var)
  (set var (read-string (concat prompt ": ") nil my-capture-prompt-history)))

(defun my-gtd-file (name)
  (format "~/ordo/gtd/%s.org" name))

(defconst my-inbox-file (my-gtd-file "inbox"))
(defconst my-calendar-file (my-gtd-file "calendar"))
(defconst my-tasks-file (my-gtd-file "tasks"))
(defconst my-projects-file (my-gtd-file "projects"))
(defconst my-someday-file (my-gtd-file "someday"))

(defconst my-shared-inbox-file "~/kunhavigi/inbox.org")

(defconst my-gtd-agenda-files (list my-calendar-file
                                    my-tasks-file
                                    my-projects-file))

(defconst my-inbox-files (list my-inbox-file my-shared-inbox-file))

(defconst my-gtd-capture-templates
  `(("i" "Inbox" entry (file ,my-inbox-file) (file ,(my-capture-template "inbox")))
    ("l" "Link" entry (file ,my-inbox-file) (file ,(my-capture-template "link")))
    ("p" "Project" entry (file ,my-projects-file) (file ,(my-capture-template "project")))
    ("s" "Someday Area" entry (file ,my-someday-file) (file ,(my-capture-template "someday_area")))))

(defun my-capture-to-inbox ()
  (interactive)
  (org-capture nil "i"))

(defconst my-day-agenda
  `("d"
    "Day"
    ((agenda "" ((org-agenda-span 1)
                 (org-agenda-skip-scheduled-if-done t)
                 (org-agenda-skip-deadline-if-done t)
                 (org-agenda-skip-timestamp-if-done t)
                 (org-agenda-files ',my-gtd-agenda-files)))
     (todo "TODO" ((org-agenda-overriding-header "Not-scheduled Tasks")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
                   (org-agenda-files '(,my-tasks-file))))
     (tags "NOW+LEVEL=1" ((org-agenda-overriding-header "Projects")
                          (org-agenda-files '(,my-projects-file)))))))

(defconst my-gtd-refile-targets
  `(((,my-calendar-file
      ,my-tasks-file
      ,my-projects-file
      ,my-someday-file
      ,my-inbox-file
      ,my-shared-inbox-file)
     :level . 1)))

(straight-use-package 'org)

(use-package org
  :defer t
  :custom
  (org-agenda-files my-gtd-agenda-files)
  (org-confirm-babel-evaluate nil)
  (org-startup-indented t)
  (org-use-sub-superscripts '{})
  (org-capture-templates my-gtd-capture-templates)
  (org-agenda-custom-commands (list my-day-agenda))
  (org-refile-targets my-gtd-refile-targets)
  (org-attach-directory "~/ordo/attach")
  :bind
  (("C-c c" . org-capture)
   ("C-c i" . my-capture-to-inbox)
   ("C-c a" . org-agenda)))

(use-package org-modern
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")
  (org-agenda-tags-column 0)
  :config
  (global-org-modern-mode))

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

(use-package dired
  :straight nil
  :custom
  (dired-dwim-target t)
  :bind
  (("<f7>" . dired-jump)
   :map dired-mode-map
   ("o" . crux-open-with)
   ("<tab>" . dired-find-file-other-window)))

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

(unless (my-windows-p)
  (set-face-attribute 'default nil :font "Iosevka Comfy Wide Fixed" :height my-default-font-height)
  (set-frame-font "Iosevka Comfy Wide Fixed" nil t))

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
        ("C-c p {" . sp-wrap-curly)
        ("C-c p [" . sp-wrap-square)
        ("C-c p (" . sp-wrap-round)
        ("C-c p r" . sp-rewrap-sexp)))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
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
  (completion-category-overrides '((file (styles partial-completion)))))

(defun my-avy-action-embark (point)
  (unwind-protect
      (save-excursion
        (goto-char point)
        (embark-act))
    (avy--done)))

(defun my-avy-action-embark-dwim (point)
  (unwind-protect
      (save-excursion
        (goto-char point)
        (embark-dwim))
    (avy--done)))

(use-package avy
  :custom
  (avy-single-candidate-jump t)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'my-avy-action-embark)
  (setf (alist-get ?\; avy-dispatch-alist) 'my-avy-action-embark-dwim)
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
  :custom
  (consult-buffer-sources '(consult--source-hidden-buffer consult--source-modified-buffer consult--source-buffer consult--source-recent-file consult--source-file-register consult--source-project-buffer-hidden consult--source-project-recent-file-hidden))
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)
   ("C-x B" . consult-bookmark)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g M" . consult-global-mark)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

(use-package embark-consult
  :after (embark consult))

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

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "<f5>")
  :hook
  (scala-mode . lsp)
  :commands lsp
  :bind
  (:map lsp-mode-map
        ([M-down-mouse-1] . mouse-set-point)
        ([M-mouse-1] . lsp-find-definition)
        ([M-mouse-3] . xref-go-back)))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-metals
  :bind
  (:map scala-mode-map
        ("<f5> I" . lsp-metals-build-import)))

(use-package flycheck
  :custom
  (flycheck-global-modes '(not org-mode))
  :config
  (global-flycheck-mode))

(use-package rg
  :bind
  (("M-s R" . rg-project)))

(defun my-vterm-project-root ()
  (interactive)
  (let* ((project-root (when-let ((project (project-current)))
                         (project-root project)))
         (project-name (when project-root
                          (file-name-nondirectory
                           (directory-file-name project-root))))
         (vterm-buffer-name (when project-name
                              (format "*%s: vterm*" project-name))))
    (if project-root
        (progn
          (unless (get-buffer vterm-buffer-name)
            (let ((default-directory project-root))
              (vterm vterm-buffer-name)))
          (switch-to-buffer vterm-buffer-name))
      (message "Not in a project"))))

(use-package vterm
  :when (not (my-windows-p))
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-shell my-fish-path)
  (vterm-max-scrollback 50000)
  :bind
  (("C-x v" . vterm)
   ("C-x 4 v" . vterm-other-window)
   ("C-x p v" . my-vterm-project-root)
   :map vterm-mode-map
   ("<f4>" . rename-buffer)))

(defun my-gptel-clear ()
  (interactive)
  (erase-buffer)
  (insert "*** "))

(use-package gptel
  :custom
  (gptel-model "gpt-4-1106-preview")
  (gptel-default-mode 'org-mode)
  :bind
  (("C-c SPC" . gptel)
   :map gptel-mode-map
   ("C-c x x" . my-gptel-clear)
   ("C-c x a" . gptel-abort)))

(use-package ob-restclient)

(use-package hydra)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :custom-face (copilot-overlay-face ((t (:background "DarkGreen" :foreground "White"))))
  :config
  (add-to-list 'warning-suppress-log-types '(copilot copilot-no-mode-indent))
  (defhydra my-copilot-hydra (copilot-mode-map "M-TAB")
    "Copilot"
    ("M-TAB" copilot-accept-completion "Accept" :color blue)
    ("M-f" copilot-accept-completion-by-word "Word")
    ("M-e" copilot-accept-completion-by-line "Line")))

(use-package js
  :bind
  (:map js-json-mode-map
        ("<f5> = =" . json-pretty-print-buffer)))

(define-derived-mode anki-mode org-mode "Anki")

(add-to-list 'auto-mode-alist '("\\.anki\\'" . anki-mode))

(use-package anki-editor
  :hook ((anki-mode . anki-editor-mode)))

(use-package kubel
  :bind
  (("C-c K" . kubel)
   :map kubel-mode-map
   ("n" . next-line)
   ("p" . previous-line)
   ("N" . kubel-set-namespace)))
