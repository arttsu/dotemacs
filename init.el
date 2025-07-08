(defun my-linux-p ()
  (eq system-type 'gnu/linux))

(defun my-macos-p ()
  (eq system-type 'darwin))

(defun my-windows-p ()
  (eq system-type 'windows-nt))

(setq my-font (cond ((my-linux-p) "Liberation Mono")
                    ((my-macos-p) "Menlo")
                    ((my-windows-p) "Cascadia Code")))
(setq my-font-height 125)

(setq my-vterm-shell nil)
(setq my-use-ripgrep nil)
(setq my-use-copilot nil)
(setq my-use-jinx nil)

(let ((path-to-local-config (expand-file-name "local.el" user-emacs-directory)))
  (if (file-exists-p path-to-local-config)
      (progn
        (load path-to-local-config)
        (message "Loaded local config."))
    (message "No local config.")))

(setq custom-file-path (expand-file-name "custom.el" user-emacs-directory))
(setq custom-file custom-file-path)

(when (file-exists-p custom-file-path)
  (load custom-file-path))

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(use-package exec-path-from-shell
  :ensure
  :when (my-macos-p)
  :config
  (exec-path-from-shell-initialize))

(use-package emacs
  :custom
  ;; UI and startup
  (inhibit-startup-message t)
  (initial-scratch-message "✅ All systems go! 🚀🪐✨")
  (initial-major-mode 'text-mode)
  (visible-bell t)

  ;; Editing behavior
  (indent-tabs-mode nil)
  (tab-width 4)
  (require-final-newline t)
  (save-interprogram-paste-before-kill t)

  ;; File handling
  (create-lockfiles nil)
  (make-backup-files nil)
  (global-auto-revert-non-file-buffers t)

  ;; Security and permissions
  (epg-pinentry-mode 'loopback)
  (disabled-command-function nil)

  :config
  ;; UI cleanup
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)

  ;; Font configuration
  (set-face-attribute 'default nil :font my-font :height my-font-height)
  (set-frame-font my-font nil t)

  ;; Better emoji support on Windows
  (when (my-windows-p)
    (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'append))

  ;; Essential modes
  (global-auto-revert-mode)
  (global-subword-mode)
  (tab-bar-mode)
  (tab-bar-history-mode)
  (repeat-mode)

  ;; Simplify prompts
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Only in programming buffers to avoid clutter in text modes
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  :bind
  (;; Navigation
   ("C-c j x" . scratch-buffer)
   ("C-c j h" . my-jump-home)
   ("<f8>" . my-pop-mark)
   ("C-M-<return>" . tab-switch)

   ;; Text navigation
   ("M-g w" . forward-to-word)
   ("M-g W" . backward-to-word)

   ;; Editing
   ("C-M-; d" . duplicate-dwim)
   ("C-c d h" . erase-buffer)
   ("M-z" . zap-up-to-char)
   ("M-Z" . zap-to-char)

   ;; Mouse
   ([down-mouse-2] . mouse-set-point)
   ([mouse-2] . delete-window)))

(defun my-pop-mark ()
  "Jump back to previous mark position"
  (interactive)
  (set-mark-command '(4)))

(defun my-jump-home ()
  "Open home directory in dired"
  (interactive)
  (find-file "~/"))

(defun my-add-super-save-advice (command)
  "Add advice to COMMAND to auto-save before execution."
  (advice-add command :before
              (lambda (&rest _)
                (when (bound-and-true-p super-save-mode)
                  (super-save-command)))))

(use-package dired
  :custom
  (dired-dwim-target t)
  (insert-directory-program (cond ((my-windows-p) insert-directory-program)
                                  ((my-macos-p) "gls")
                                  (t "ls")))
  (dired-listing-switches (cond ((my-windows-p) dired-listing-switches)
                                (t "-alh --group-directories-first")))
  :bind
  (("<f7>" . dired-jump)
   :map dired-mode-map
   ("o" . crux-open-with)
   ("<tab>" . dired-find-file-other-window)))

(use-package dired-du
  :ensure)

(use-package project
  :config
  (add-to-list 'project-switch-commands '(project-dired "Dired" "<return>") t))

(use-package flymake
  :bind
  (:map prog-mode-map
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)))

(use-package savehist
  :config
  (savehist-mode))

(use-package modus-themes
  :ensure
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-headings '((0 . (ultrabold 1.9))
                           (1 . (ultrabold 1.7))
                           (2 . (extrabold 1.5))
                           (3 . (extrabold 1.3))
                           (4 . (extrabold 1.1))
                           (t . (extrabold))))
  (modus-themes-variable-pitch-ui t)
  :config
  (modus-themes-load-theme 'modus-operandi))

(use-package vertico
  :ensure
  :demand
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  ;; Configure multiform for specific commands
  (add-to-list 'vertico-multiform-commands
               '(consult-ripgrep buffer indexed))
  :bind
  (:map vertico-map
        ("C-;" . vertico-quick-insert)))

(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :ensure
  :demand
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  :config
  (global-corfu-mode)
  (corfu-history-mode)
  :bind
  (:map corfu-map
        ("C-SPC" . corfu-insert-separator)
        ("C-;" . corfu-quick-insert)))

(use-package consult
  :ensure
  :config
  ;; Don't live preview buffers to avoid triggering package loading (esp. Org)
  (consult-customize consult-buffer :preview-key "M-.")
  :bind
  (;; Buffer and project navigation
   ("C-x b" . consult-buffer)
   ("C-x B" . consult-buffer-other-tab)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)

   ;; Navigation
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)

   ;; Search
   ("M-s l" . consult-line)
   ("M-s k" . consult-keep-lines)
   ("M-s f" . consult-focus-lines)
   ("M-s r" . consult-ripgrep)

   :map prog-mode-map
   ("C-c ! !" . consult-flymake)))

(defun my-avy-embark-act (target-point)
  "Jump to avy target and run embark-act."
  (goto-char target-point)
  (embark-act))

(defun my-avy-embark-dwim (target-point)
  "Jump to avy target and run embark-dwim."
  (goto-char target-point)
  (embark-dwim))

(use-package avy
  :ensure
  :custom
  (avy-single-candidate-jump t)
  :config
  ;; Embark integration - use . for embark-act, ; for embark-dwim
  (setf (alist-get ?. avy-dispatch-alist) 'my-avy-embark-act)
  (setf (alist-get ?\; avy-dispatch-alist) 'my-avy-embark-dwim)
  :bind
  (;; Core avy commands
   ("C-;" . avy-goto-char-timer)
   ("M-;" . avy-pop-mark)
   ("M-g g" . avy-goto-line)
   ("M-g G" . avy-goto-end-of-line)
   ("M-g h" . avy-org-goto-heading-timer)
   ("M-g s" . avy-goto-word-1)

   ;; Avy actions
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

(use-package embark
  :ensure
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

(use-package embark-consult
  :ensure
  :after (embark consult))

(use-package link-hint
  :ensure
  :bind
  (("C-c f" . link-hint-open-link)
   ("C-c y" . link-hint-copy-link)))

(use-package smartparens
  :ensure
  :config
  (require 'smartparens-config)
  :hook
  (prog-mode . smartparens-mode)
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
        ("C-c p (" . sp-wrap-round)
        ("C-c p [" . sp-wrap-square)
        ("C-c p {" . sp-wrap-curly)
        ("C-c p r" . sp-rewrap-sexp)))

(use-package crux
  :ensure
  :bind
  (("C-o" . crux-smart-open-line)
   ("C-S-o" . crux-smart-open-line-above)
   ("C-^" . crux-top-join-line)
   ("C-M-; D" . crux-duplicate-and-comment-current-line-or-region)))

(use-package expand-region
  :ensure
  :bind
  ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure
  :bind
  (("C-+" . mc/mark-next-like-this)
   ("C-c k l" . mc/edit-lines)
   ("C-c k m" . mc/mark-all-dwim)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-<return>" . set-rectangular-region-anchor)))

(use-package iy-go-to-char
  :ensure (:host github :repo "arttsu/iy-go-to-char")
  :bind
  (("M-g f" . iy-go-to-char)
   ("M-g F" . iy-go-to-char-backward)
   ("M-g t" . iy-go-up-to-char)
   ("M-g T" . iy-go-up-to-char-backward)
   ("M-g ;" . iy-go-to-or-up-to-continue)
   ("M-g ," . iy-go-to-or-up-to-continue-backward)))

(use-package super-save
  :ensure
  :custom
  (super-save-all-buffers t)
  (super-save-auto-save-when-idle t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-silent t)
  (super-save-exclude '(".sbt" "project/"
                        ".gpg"))
  (auto-save-default nil)
  :config
  (super-save-mode))

(use-package ace-window
  :ensure
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (aw-scope 'frame)
  :config
  ;; Manual integration with super-save since triggers don't work reliably
  (my-add-super-save-advice 'ace-window)
  :bind
  ("M-o" . ace-window))

(use-package whole-line-or-region
  :ensure
  :demand
  :config
  (whole-line-or-region-global-mode)
  :bind
  ("M-/" . whole-line-or-region-comment-dwim))

(defun tempel-include (elt)
  "Include template from another template."
  (when (eq (car-safe elt) 'i)
    (if-let (template (alist-get (cadr elt) (tempel--templates)))
        (cons 'l template)
      (message "Template %s not found" (cadr elt))
      nil)))

(use-package tempel
  :ensure
  :demand
  :custom
  (tempel-trigger-prefix "<")
  :init
  (defun my-tempel-setup-capf()
    (setq-local completion-at-point-functions
                (cons 'tempel-complete completion-at-point-functions)))
  (add-hook 'text-mode-hook 'my-tempel-setup-capf)
  (add-hook 'conf-mode-hook 'my-tempel-setup-capf)
  (add-hook 'prog-mode-hook 'my-tempel-setup-capf)
  :config
  (tempel-key "C-c t f" fun emacs-lisp-mode-map)
  (tempel-key "C-c t t" today)
  (tempel-key "C-c t T" now)
  (add-to-list 'tempel-user-elements #'tempel-include)
  :bind
  ("M-+" . tempel-insert))

(defun my-easysession-visible-buffer-list ()
  "Return list of buffers visible in windows or tab bar tabs."
  (let ((visible-buffers '()))
    (dolist (buffer (buffer-list))
      (when (or (get-buffer-window buffer 'visible)
                (and (bound-and-true-p tab-bar-mode)
                     (fboundp 'tab-bar-get-buffer-tab)
                     (tab-bar-get-buffer-tab buffer t nil)))
        (push buffer visible-buffers)))
    visible-buffers))

(defun my-easysession-setup-minimal ()
  "Set up a minimal session environment for new sessions."
  (when (and (boundp 'tab-bar-mode) tab-bar-mode)
    (tab-bar-close-other-tabs)
    (tab-bar-rename-tab ""))
  (delete-other-windows)
  (scratch-buffer))

(defun my-easysession-reset-session ()
  "Reset the current session to a minimal state."
  (interactive)
  (when (yes-or-no-p "Reset session?")
    (my-easysession-setup-minimal)))

(use-package easysession
  :ensure
  :custom
  (easysession-mode-line-misc-info t)
  (easysession-save-interval (* 5 60))  ; Auto-save every 5 minutes
  (easysession-buffer-list-function 'my-easysession-visible-buffer-list)
  (easysession-switch-to-exclude-current t)
  :init
  ;; Load last session and enable auto-save on startup (only in interactive mode)
  (unless noninteractive
    (add-hook 'emacs-startup-hook 'easysession-load-including-geometry 102)
    (add-hook 'emacs-startup-hook 'easysession-save-mode 103))
  :config
  ;; Set up clean environment for new sessions
  (add-hook 'easysession-new-session-hook 'my-easysession-setup-minimal)
  ;; Save before switching sessions
  (add-hook 'easysession-before-load-hook 'easysession-save)
  :bind
  (("<f12> <f12>" . easysession-switch-to)
   ("<f12> s" . easysession-save)
   ("<f12> S" . easysession-save-as)
   ("<f12> k" . easysession-delete)
   ("<f12> r" . my-easysession-reset-session)))

(use-package hydra
  :ensure)

(use-package transient
  :ensure)

(use-package magit
  :ensure
  :bind
  ("C-c g" . magit-file-dispatch))

(defun my-vterm-unbind-keys ()
  "Unbind function keys in vterm to allow global Emacs bindings."
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
  (local-unset-key (kbd "<f12>")))

(defun my-vterm--new-buffer-and-switch (path buffer-name)
  "Create new vterm buffer in PATH with BUFFER-NAME and switch to it."
  (let ((default-directory path))
    (vterm buffer-name)
    (switch-to-buffer buffer-name)))

(defun my-vterm-project (&optional prefix)
  "Open vterm in project root. With PREFIX, create new vterm buffer."
  (interactive "P")
  (if-let ((project-root-path (when (project-current) (project-root (project-current)))))
      (let ((target-buffer-name (format "*%s: vterm*" (project-name (project-current)))))
        (if (get-buffer target-buffer-name)
            (if prefix
                (let ((new-buffer-name (generate-new-buffer-name target-buffer-name)))
                  (my-vterm--new-buffer-and-switch project-root-path new-buffer-name))
              (switch-to-buffer target-buffer-name))
          (my-vterm--new-buffer-and-switch project-root-path target-buffer-name)))))

(use-package vterm
  :when (bound-and-true-p my-vterm-shell)
  :ensure
  :demand
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-shell my-vterm-shell)
  (vterm-max-scrollback 50000)
  (vterm-clear-scrollback-when-clearing t)
  :config
  (require 'project)
  (add-hook 'vterm-mode-hook 'my-vterm-unbind-keys)
  (add-to-list 'project-switch-commands '(my-vterm-project "Vterm" "V") t)
  :bind
  (("C-x v" . vterm)
   ("C-x 4 v" . vterm-other-window)
   ("C-x p v" . my-vterm-project)))

(use-package kubel
  :ensure
  :unless (my-windows-p)
  :after vterm
  :config
  (add-to-list 'vterm-tramp-shells '("kubectl" "/bin/bash"))
  :bind
  (("C-c K" . kubel)
   :map kubel-mode-map
   ("n" . next-line)
   ("p" . previous-line)
   ("N" . kubel-set-namespace)
   ("v" . kubel-exec-vterm-pod)
   ("P" . kubel-port-forward-pod)))

(use-package rg
  :when my-use-ripgrep
  :ensure
  :bind
  (("M-s R" . rg)
   ("C-x p g" . rg-project)))

(use-package jinx
  :ensure
  :when my-use-jinx
  :custom
  (jinx-languages "en_US de_DE ru")
  :hook
  (emacs-startup . global-jinx-mode)
  :bind
  (("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages)))

(defun my-gptel-auto-fill-response (begin end)
  "Auto-fill GPTel responses in org-mode."
  (when (and (not (eq begin end)) (eq major-mode 'org-mode))
    (save-excursion
      (goto-char begin)
      (while (not (>= (point) end))
        (org-forward-sentence)
        (let ((elem (org-element-at-point)))
          (when (member (org-element-type elem) '(paragraph item))
            (save-excursion
              (goto-char (org-element-property :begin elem))
              (fill-paragraph))))))))

(defun my-gptel-clear-buffer ()
  "Clear GPTel buffer and insert starter text."
  (interactive)
  (erase-buffer)
  (insert "*** "))

(defun my-gptel-send ()
  "Send GPTel request and position cursor."
  (interactive)
  (goto-char (point-max))
  (gptel-send)
  (org-back-to-heading)
  (recenter-top-bottom 0))

(define-derived-mode my-gptel-mode org-mode "GPTel")

(defun my-gptel-mode-setup ()
  "Setup GPTel mode."
  (interactive)
  (org-mode)
  (gptel-mode))

(use-package gptel
  :ensure
  :demand
  :custom
  (gptel-model 'gpt-4o)
  (gptel-default-mode 'org-mode)
  :config
  (add-hook 'gptel-mode-hook 'toggle-truncate-lines)
  (add-hook 'gptel-post-response-functions 'my-gptel-auto-fill-response)
  (add-hook 'my-gptel-mode-hook 'my-gptel-mode-setup)
  (add-to-list 'auto-mode-alist '("\\.gptel\\'" . my-gptel-mode))
  :bind
  (("C-c SPC" . gptel)
   :map gptel-mode-map
   ("C-c C-c" . my-gptel-send)
   ("C-c k" . gptel-abort)
   ("C-c d h" . my-gptel-clear-buffer)))

(use-package emms
  :ensure
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-player-mpv-update-metadata t)
  (emms-streams-file (expand-file-name "streams.emms" user-emacs-directory))
  :config
  (emms-all)
  :bind
  (("C-c r r" . emms-streams)
   ("C-c r p" . emms-pause)
   ("C-c r k" . emms-stop)))

(defun my-ledger--bal-period (period)
  "Generate ledger balance command for PERIOD."
  (format "%%(binary) -f %%(ledger-file) --invert --period \"%s\" -S amount bal ^Income ^Expenses" period))

(use-package ledger-mode
  :ensure
  :custom
  (ledger-default-date-format "%Y-%m-%d")
  :config
  (ledger-reports-add "bal-this-month" (my-ledger--bal-period "this month"))
  (ledger-reports-add "bal-last-month" (my-ledger--bal-period "last month")))

(use-package org
  :ensure
  :custom
  ;; Startup and display
  (org-startup-folded 'showall)
  (org-hide-drawer-startup nil)
  (org-startup-indented nil)
  (org-startup-with-inline-images t)
  (org-cycle-separator-lines 1)

  ;; Editing behavior
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-use-speed-commands t)
  (org-use-sub-superscripts '{})
  (org-src-window-setup 'split-window-below)
  (org-confirm-babel-evaluate nil)

  ;; Todo and priority settings
  (org-log-done 'time)
  (org-priority-lowest 69)
  (org-priority-default 68)

  ;; Linking and ID settings
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-refile-targets my-gtd-refile-targets)
  (org-refile-target-verify-function 'my-gtd-refile-verify-target)
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)

  ;; Habit settings
  (org-habit-graph-column 80)
  (org-habit-show-done-always-green t)

  ;; Capture templates
  (org-capture-templates my-gtd-capture-templates)

  ;; Agenda configuration
  (org-agenda-files my-gtd-all-dirs)
  (org-agenda-custom-commands `(,my-gtd-day-agenda))
  (org-agenda-prefix-format '((agenda . " %i %-20(my-org-agenda-category-short) %?-12t% s")
                               (todo . " %i %-20(my-org-agenda-category-short) ")
                               (tags . " %i %-20(my-org-agenda-category-short) ")
                               (search . " %i %-20(my-org-agenda-category-short) ")))

  :config
  (require 'org-attach)
  (require 'org-id)
  (require 'org-habit)

  ;; Suppress spurious org-element warnings during agenda generation
  (add-to-list 'warning-suppress-types '(org-element))
  (add-to-list 'warning-suppress-log-types '(org-element))

  ;; GTD hooks
  (add-hook 'org-after-todo-state-change-hook 'my-org-remove-priority-when-done)
  (add-hook 'org-after-todo-state-change-hook 'my-gtd-checklist-auto-advance)
  ;; Refile hooks in order: blank line, format log entry, then sort
  (add-hook 'org-after-refile-insert-hook 'my-gtd-add-blank-line-after-refile -10)
  (add-hook 'org-after-refile-insert-hook 'my-gtd-format-log-entry-after-refile 50)
  (add-hook 'org-after-refile-insert-hook 'my-gtd-sort-entries-hook 90)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))

  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ;; GTD capture
   ("C-c i" . my-gtd-capture-note)
   ("C-c I" . my-gtd-capture-todo)
   ;; GTD project lifecycle
   ("C-c o p" . my-gtd-create-project)
   ("C-c o A" . my-gtd-create-area)
   :map org-mode-map
   ("C-c o C-i" . org-id-get-create)
   ;; GTD workflow
   ("C-c o s" . my-gtd-sort-entries)
   ("C-c o r" . my-gtd-reset-checklist)
   ("C-c o x" . my-gtd-complete-as-wont-do)
   ;; GTD project lifecycle
   ("C-c o Z" . my-gtd-archive-project)))

(use-package org-modern
  :ensure
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  (org-modern-priority-faces '((?A :background "Firebrick3" :foreground "White")
                               (?B :background "Goldenrod" :foreground "White")
                               (?C :background "SeaGreen" :foreground "White")
                               (?D :background "MediumOrchid" :foreground "White")
                               (?E :background "Seashell3" :foreground "Black")))
  (org-modern-table nil)
  :custom-face
  (org-modern-tag ((t (:foreground "White" :background "CornflowerBlue" :slant italic))))
  :config
  (global-org-modern-mode))

(use-package org-auto-tangle
  :ensure
  :hook
  (org-mode . org-auto-tangle-mode))

(use-package ob-restclient
  :ensure
  :after org)

(use-package ox-gfm
  :ensure
  :after org)

;; GTD directory structure with local/shared naming
(defconst my-org-local-dir (expand-file-name "~/org-local"))
(defconst my-gtd-local-dir (expand-file-name "gtd" my-org-local-dir))
(defconst my-gtd-local-inbox (expand-file-name "inbox.org" my-gtd-local-dir))
(defconst my-gtd-local-areas (expand-file-name "areas" my-gtd-local-dir))
(defconst my-gtd-local-projects (expand-file-name "projects" my-gtd-local-dir))

(defconst my-org-shared-dir (expand-file-name "~/org-shared"))
(defconst my-gtd-shared-dir (expand-file-name "gtd" my-org-shared-dir))
(defconst my-gtd-shared-inbox (expand-file-name "inbox.org" my-gtd-shared-dir))
(defconst my-gtd-shared-projects (expand-file-name "projects" my-gtd-shared-dir))
(defconst my-gtd-shared-areas (expand-file-name "areas" my-gtd-shared-dir))

;; Directory lists for org-node watching
(defconst my-gtd-all-dirs
  (list my-gtd-local-dir my-gtd-local-areas my-gtd-local-projects
        my-gtd-shared-dir my-gtd-shared-areas my-gtd-shared-projects))

;; Helper function for validating cursor position
  (defun my-org-require-at-heading ()
    "Ensure point is at an org heading."
    (unless (org-at-heading-p)
      (user-error "Not at a heading")))

  ;; Priority management
  (defun my-org-remove-priority-when-done ()
    "Remove priority when todo is marked DONE."
    (when (string= org-state "DONE")
      (ignore-errors (org-entry-put (point) "PRIORITY" nil))))

  ;; Timestamp extraction functions
  (defun my-gtd-extract-created-timestamp ()
    "Extract CREATED timestamp from current entry."
    (or (org-entry-get (point) "CREATED")
        "[1900-01-01 Mon 00:00]"))

  (defun my-gtd-extract-closed-timestamp ()
    "Extract CLOSED timestamp from current entry."
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (if (re-search-forward "CLOSED: " nil t)
            (buffer-substring-no-properties (point) (line-end-position))
          "[1900-01-01 Mon 00:00]"))))

  (defun my-gtd-invert-timestamp-for-reverse-sort (timestamp &optional default)
    "Invert a timestamp for newest-first sorting.
DEFAULT is returned if timestamp doesn't match expected format."
    (if (string-match "\\[\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\).*\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\]" timestamp)
        (let ((year (string-to-number (match-string 1 timestamp)))
              (month (string-to-number (match-string 2 timestamp)))
              (day (string-to-number (match-string 3 timestamp)))
              (hour (string-to-number (match-string 4 timestamp)))
              (minute (string-to-number (match-string 5 timestamp))))
          (format "[%04d-%02d-%02d %02d:%02d"
                  (- 9999 year) (- 99 month) (- 99 day)
                  (- 99 hour) (- 99 minute)))
      (or default "[0000-00-00 00:00]")))

  (defun my-gtd-extract-created-timestamp-for-reverse-sort ()
    "Extract created timestamp for newest-first sorting."
    (my-gtd-invert-timestamp-for-reverse-sort (my-gtd-extract-created-timestamp)))

  ;; Style predicates
  (defun my-gtd-checklist-p ()
    "Check if current entry has STYLE property set to 'checklist'."
    (let ((style (org-entry-get (point) "STYLE" t)))
      (string= style "checklist")))

  (defun my-gtd-log-p ()
    "Check if current entry has STYLE property set to 'log'."
    (let ((style (org-entry-get (point) "STYLE" t)))
      (string= style "log")))

  ;; Checklist auto-advance functionality
  (defun my-gtd-checklist-do-auto-advance ()
    "Move to next heading in checklist."
    (let ((point-before (point)))
      (org-forward-heading-same-level 1)
      (when (= (point) point-before)
        (org-up-heading-safe))))

  (defun my-gtd-checklist-auto-advance ()
    "Auto-advance to next item when completing checklist items."
    ;; Only proceed if we're truly in an org file buffer, not during agenda operations
    (when (and (string= org-state "DONE")
               (not (eq this-command 'org-agenda-todo)))
      ;; Save the current buffer context and work in the actual org buffer
      (let ((orig-buffer (current-buffer)))
        (when (and (with-current-buffer orig-buffer
                     (and (derived-mode-p 'org-mode)
                          (buffer-file-name)
                          (not (derived-mode-p 'org-agenda-mode))
                          (not (string-match-p "\\*Org Agenda" (buffer-name)))
                          (org-at-heading-p))))
          (with-current-buffer orig-buffer
            (condition-case err
                (let* ((current-element (org-element-at-point))
                       (parent (org-element-property :parent current-element))
                       (parent-style-prop (and parent (org-entry-get parent "STYLE")))
                       (parent-style (or parent-style-prop "")))
                  (when (string= parent-style "checklist")
                    (run-with-idle-timer 0 nil 'my-gtd-checklist-do-auto-advance)))
              (error
               (message "GTD checklist auto-advance error: %s in buffer %s (mode: %s)"
                        err (buffer-name) major-mode))))))))

;; Sorting functions
  (defun my-gtd-extract-closed-timestamp-for-reverse-sort ()
    "Extract CLOSED timestamp for reverse chronological sorting.
  Returns inverted timestamp for DONE items, earliest date for TODO items."
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (goto-char (point-min))
        (let ((todo-state (org-get-todo-state)))
          (if (member todo-state org-done-keywords)
              ;; For DONE items, invert the timestamp for reverse sorting
              (if (re-search-forward "CLOSED: \\(\\[.*?\\]\\)" nil t)
                  (my-gtd-invert-timestamp-for-reverse-sort
                   (match-string 1)
                   "[9999-12-31 Mon 23:59]")  ; Latest possible for missing timestamps
                "[9999-12-31 Mon 23:59]")
            ;; For TODO items, return earliest date so they sort first
            "[1900-01-01 Mon 00:00]")))))

  (defun my-gtd-sort-todos ()
    "Sort TODOs by created timestamp, closed timestamp, priority, and todo state."
    (interactive)
    (my-org-require-at-heading)
    (org-sort-entries nil ?f 'my-gtd-extract-created-timestamp)
    (org-sort-entries nil ?f 'my-gtd-extract-closed-timestamp)
    (org-sort-entries nil ?p)
    (org-sort-entries nil ?o)
    (org-fold-show-children))

  (defun my-gtd-sort-checklist ()
    "Sort checklist items: TODO by priority/timestamp, DONE by closed time (newest first)."
    (interactive)
    (my-org-require-at-heading)
    ;; Apply sorts in reverse order of importance (least important first)
    ;; 1. Sort by created timestamp (affects TODO items)
    (org-sort-entries nil ?f 'my-gtd-extract-created-timestamp)
    ;; 2. Sort by priority (affects TODO items with priorities)
    (org-sort-entries nil ?p)
    ;; 3. Sort by closed timestamp for reverse chronological (affects DONE items)
    (org-sort-entries nil ?f 'my-gtd-extract-closed-timestamp-for-reverse-sort)
    ;; 4. Finally sort by TODO state to separate TODO from DONE
    (org-sort-entries nil ?o)
    (org-fold-show-children))


  (defun my-gtd-sort-by-style ()
    "Sort entries based on their STYLE property."
    (interactive)
    (my-org-require-at-heading)
    (let ((style (org-entry-get (point) "STYLE" t)))
      (cond
       ((string= style "checklist")
        (my-gtd-sort-checklist))
       ((string= style "log")
        (org-sort-entries nil ?f 'my-gtd-extract-created-timestamp-for-reverse-sort)
        (org-fold-show-children))
       (t
        (user-error "No supported STYLE property found")))))

  (defun my-gtd-sort-entries (&optional no-error)
    "Smart sort that checks current entry or parent for sorting style.
With NO-ERROR, fail silently instead of throwing user-error."
    (interactive)
    (my-org-require-at-heading)
    ;; Check if STYLE is directly on current entry (not inherited)
    (let ((style (org-entry-get (point) "STYLE" nil)))
      (if (or (string= style "checklist") (string= style "log"))
          (my-gtd-sort-by-style)
        ;; No direct STYLE property, try parent
        (if (org-up-heading-safe)
            (let ((parent-style (org-entry-get (point) "STYLE" nil)))
              (if (or (string= parent-style "checklist") (string= parent-style "log"))
                  (my-gtd-sort-by-style)
                ;; Show error unless NO-ERROR is set
                (unless no-error
                  (user-error "No supported STYLE property found"))))
          ;; Show error unless NO-ERROR is set
          (unless no-error
            (user-error "No supported STYLE property found"))))))

  (defun my-gtd-sort-entries-hook ()
    "Wrapper for my-gtd-sort-entries for use in hooks. Fails silently on error."
    (condition-case err
        (when (and (derived-mode-p 'org-mode)
                   (org-at-heading-p))
          (message "GTD: Running sort-entries hook at %s" (org-get-heading t t t t))
          (my-gtd-sort-entries t))
      (error
       (message "GTD: Sort-entries hook error: %s" err))))

  ;; Checklist management
  (defun my-gtd-reset-checklist ()
    "Reset all items in a checklist to TODO state."
    (interactive)
    (my-org-require-at-heading)
    (let ((style (org-entry-get (point) "STYLE" t)))
      (if (not (my-gtd-checklist-p))
          (error "Not at a checklist")
        (when (yes-or-no-p "Reset the checklist?")
          (org-map-entries (lambda ()
                             (org-todo "TODO")
                             ;; Remove strikethrough if present
                             (let ((heading (org-get-heading t t t t)))
                               (when (string-match "^\\+\\(.+\\)\\+$" heading)
                                 (org-edit-headline (match-string 1 heading))))
                             ;; Remove CLOSED_AS property
                             (org-delete-property "CLOSED_AS"))
                           nil
                           'tree)
          (org-todo "")))))

  (defun my-gtd-complete-as-wont-do ()
    "Toggle entry between won't-do and todo states."
    (interactive)
    (my-org-require-at-heading)
    (let ((heading (org-get-heading t t t t))
          (closed-as (org-entry-get (point) "CLOSED_AS")))
      ;; Toggle between won't-do and TODO states
      (if (string= closed-as "WONT_DO")
          ;; Currently marked as won't do - undo it
          (progn
            (org-todo "TODO")
            (org-delete-property "CLOSED_AS")
            ;; Remove strikethrough if present
            (when (string-match "^\\+\\(.+\\)\\+$" heading)
              (org-edit-headline (match-string 1 heading)))
            (message "Unmarked as won't do"))
        ;; Not marked as won't do - mark it
        (progn
          (org-todo 'done)
          (org-entry-put (point) "CLOSED_AS" "WONT_DO")
          ;; Only add strikethrough if not already present
          (unless (string-match "^\\+.+\\+$" heading)
            (org-edit-headline (format "+%s+" heading)))
          (message "Marked as won't do")))))

;; Template path resolver
(defun my-org-capture-template-path (name)
  "Get full path to capture template file."
  (expand-file-name (concat "capture-templates/" name ".txt") user-emacs-directory))

;; Project creation
(defun my-gtd-create-project ()
  "Create a new GTD project with interactive prompts."
  (interactive)
  (let* ((contexts '("Local" "Shared"))
         (context (completing-read "Context: " contexts nil t))
         (title (read-string "Project title: "))
         (priority-char (read-char-choice "Priority [A-E, default D]: "
                                          '(?A ?B ?C ?D ?E ?a ?b ?c ?d ?e ?\r ?\n)))
         (priority (char-to-string (upcase (if (memq priority-char '(?\r ?\n)) ?D priority-char))))
         (priority-cookie (format "[#%s]" priority))
         (slug (replace-regexp-in-string
                "[^a-zA-Z0-9-]" ""
                (replace-regexp-in-string "\\s-+" "-" (downcase title))))
         (filename (format "%s-%s.org"
                           (format-time-string "%Y-%m-%d")
                           slug))
         (project-dir (if (string= context "Local")
                          my-gtd-local-projects
                        my-gtd-shared-projects))
         (filepath (expand-file-name filename project-dir))
         (org-id (org-id-new))
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (template-path (my-org-capture-template-path "gtd-project")))

    ;; Validate inputs
    (when (string-empty-p title)
      (user-error "Project title cannot be empty"))

    ;; Ensure project directory exists
    (unless (file-exists-p project-dir)
      (make-directory project-dir t))

    ;; Read and format template
    (unless (file-exists-p template-path)
      (user-error "Template file not found: %s" template-path))

    (let ((template-content (with-temp-buffer
                              (insert-file-contents template-path)
                              (buffer-string))))
      ;; Create project file
      (with-temp-buffer
        (insert (format template-content priority-cookie title org-id timestamp))
        (write-file filepath))

      ;; Register org-id location
      (org-id-add-location org-id filepath)

      ;; Open the new project file
      (find-file filepath)
      (goto-char (point-min))
      (re-search-forward "^\\* " nil t)
      (end-of-line)

      ;; Ask about easysession switch
      (when (and (fboundp 'easysession-save-as)
                 (y-or-n-p "Switch to a new easysession for this project? "))
        (let ((session-name slug))
          ;; Switch to previous buffer before saving session
          (when (> (length (buffer-list)) 1)
            (switch-to-buffer (other-buffer (current-buffer) t)))

          ;; Save current session (without the project file)
          (when (fboundp 'easysession-save)
            (easysession-save))

          ;; Create new session
          (easysession-save-as session-name)

          ;; Clean up tabs - delete all other tabs and rename current to 'notes'
          (when (and (boundp 'tab-bar-mode) tab-bar-mode)
            (tab-bar-close-other-tabs)
            (tab-bar-rename-tab "notes"))

          ;; Ensure the project file is open in the new session
          (find-file filepath)))

      (message "Created project: %s" title))))

;; Area creation
(defun my-gtd-create-area ()
  "Create a new GTD area with interactive prompts."
  (interactive)
  (let* ((contexts '("Local" "Shared"))
         (context (completing-read "Context: " contexts nil t))
         (title (read-string "Area title: "))
         (slug (replace-regexp-in-string
                "[^a-zA-Z0-9-]" "-"
                (replace-regexp-in-string "\\s-+" "-" (downcase title))))
         (filename (format "%s.org" slug))
         (area-dir (if (string= context "Local")
                       my-gtd-local-areas
                     my-gtd-shared-areas))
         (filepath (expand-file-name filename area-dir))
         (org-id (org-id-new))
         (timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (template-path (my-org-capture-template-path "gtd-area")))

    ;; Validate inputs
    (when (string-empty-p title)
      (user-error "Area title cannot be empty"))

    ;; Ensure area directory exists
    (unless (file-exists-p area-dir)
      (make-directory area-dir t))

    ;; Read and format template
    (unless (file-exists-p template-path)
      (user-error "Template file not found: %s" template-path))

    (let ((template-content (with-temp-buffer
                              (insert-file-contents template-path)
                              (buffer-string))))
      ;; Create area file
      (with-temp-buffer
        (insert (format template-content title org-id timestamp))
        (write-file filepath))

      ;; Register org-id location
      (org-id-add-location org-id filepath)

      ;; Open the new area file
      (find-file filepath)
      (goto-char (point-min))
      (re-search-forward "^\\* " nil t)
      (end-of-line)

      (message "Created area: %s" title))))

;; Project archiving
(defun my-gtd-archive-project ()
  "Archive the current GTD project."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))

  (let ((current-file (buffer-file-name)))
    (unless current-file
      (user-error "Buffer is not visiting a file"))

    ;; Check if it's a GTD project
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward ":GTD_TYPE:.*project" nil t)
        (user-error "Not a GTD project file")))

    (when (yes-or-no-p "Archive this project? ")
      (let* ((current-dir (file-name-directory current-file))
             (archive-dir (expand-file-name "archive" current-dir))
             (filename (file-name-nondirectory current-file))
             (new-path (expand-file-name filename archive-dir)))

        ;; Create archive directory if it doesn't exist
        (unless (file-exists-p archive-dir)
          (make-directory archive-dir t))

        ;; Update project metadata
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^\\* " nil t)
            ;; Remove priority cookie from headline
            (let ((heading (org-get-heading t t t t)))
              (when (string-match "\\[#[A-E]\\]\\s-*\\(.*\\)" heading)
                (org-edit-headline (match-string 1 heading)))
              ;; Also remove priority property if set
              (org-entry-put (point) "PRIORITY" nil))
            ;; Set ARCHIVED property
            (org-entry-put (point) "ARCHIVED"
                           (format-time-string "[%Y-%m-%d %a %H:%M]"))))

        ;; Save changes before moving
        (save-buffer)

        ;; Move file to archive
        (rename-file current-file new-path)

        ;; Update org-id locations
        (org-id-update-id-locations nil t)

        ;; Update the visited file name
        (set-visited-file-name new-path)
        (set-buffer-modified-p nil)

        ;; Handle easysession cleanup if available
        (when (fboundp 'easysession-delete)
          (let* ((project-slug (replace-regexp-in-string
                                "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-" ""
                                (file-name-base filename)))
                 (current-session (easysession-get-session-name))
                 (is-current-session (string= current-session project-slug)))

            ;; Check if session exists using easysession's own function
            (when (condition-case nil
                    (member project-slug (easysession--get-all-names))
                  (error nil))
              ;; Session exists, ask to delete
              (when (y-or-n-p (format "Delete easysession '%s'? " project-slug))
                ;; If it's the current session, switch to main first
                (when is-current-session
                  (condition-case nil
                      (easysession-switch-to "main")
                    (error
                     ;; If main doesn't exist, create a new default session
                     (easysession-save-as "main"))))
                ;; Delete the project session
                (easysession-delete project-slug)))))

        (message "Project archived to: %s" new-path)))))

;; Capture template definitions
(defconst my-gtd-local-inbox-target `(file+headline ,my-gtd-local-inbox "Items"))

(defconst my-gtd-capture-templates
  `(("i" "Inbox")
    ("ii" "note" entry ,my-gtd-local-inbox-target (file ,(my-org-capture-template-path "gtd-note")))
    ("il" "note link" entry ,my-gtd-local-inbox-target (file ,(my-org-capture-template-path "gtd-note-link")))
    ("iI" "todo" entry ,my-gtd-local-inbox-target (file ,(my-org-capture-template-path "gtd-todo")))
    ("iL" "todo link" entry ,my-gtd-local-inbox-target (file ,(my-org-capture-template-path "gtd-todo-link")))))

;; Capture wrapper functions
(defun my-gtd-capture-note (&optional prefix)
  "Capture a note to inbox. With prefix, capture with link."
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "ii"))
        ((equal prefix '(4)) (org-capture nil "il"))
        (t (error "Invalid prefix argument: %s" prefix))))

(defun my-gtd-capture-todo (&optional prefix)
  "Capture a todo to inbox. With prefix, capture with link."
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "iI"))
        ((equal prefix '(4)) (org-capture nil "iL"))
        (t (error "Invalid prefix argument: %s" prefix))))

;; Refile target configuration
(defun my-gtd-get-refile-targets ()
  "Get all GTD files for refile targets, excluding archive directories."
  (let ((all-files '()))
    ;; Add inbox files
    (when (file-exists-p my-gtd-local-inbox)
      (push my-gtd-local-inbox all-files))

    ;; Get all .org files from GTD directories, excluding archives
    (dolist (dir (list my-gtd-local-dir my-gtd-local-areas my-gtd-local-projects
                       my-gtd-shared-dir my-gtd-shared-areas my-gtd-shared-projects))
      (when (file-exists-p dir)
        (let ((org-files (directory-files-recursively dir "\\.org\\'" nil
                                                      (lambda (subdir)
                                                        ;; Exclude archive directories
                                                        (not (string-match-p "/archive/" subdir))))))
          (setq all-files (append org-files all-files)))))

    ;; Remove duplicates and return
    (delete-dups all-files)))

;; Configure refile targets
(defconst my-gtd-refile-targets
  `((my-gtd-get-refile-targets :level . 2)))

;; Hook to add blank line after refile
(defun my-gtd-add-blank-line-after-refile ()
  "Add a blank line after the refiled item."
  (when (org-at-heading-p)
    (save-excursion
      (org-end-of-subtree t t)
      (insert "\n"))))

;; Hook to format log entries with timestamp
(defun my-gtd-format-log-entry-after-refile ()
  "Format refiled item as log entry if under a log section."
  (when (org-at-heading-p)
    ;; Check if STYLE property is "log" (with inheritance)
    (let ((style (org-entry-get (point) "STYLE" t)))
      (when (string= style "log")
        ;; Extract timestamp and format heading
        (let* ((created (org-entry-get (point) "CREATED"))
               (heading (org-get-heading t t t t)))
          (when (and created (not (string-match "^\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" heading)))
            ;; Only add timestamp if not already present (check for date pattern)
            (org-edit-headline (format "%s %s" created heading))))))))

;; Manual function to format all log entries in current subtree
(defun my-gtd-format-all-log-entries ()
  "Format all entries under current log heading with timestamps."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not at a heading"))

  ;; Check if current heading has STYLE: log
  (let ((style (org-entry-get (point) "STYLE")))
    (unless (string= style "log")
      (user-error "Current heading is not a log section (STYLE: log)")))

  (let ((formatted-count 0))
    ;; Process all subheadings under current log heading
    (org-map-entries
     (lambda ()
       (let* ((created (org-entry-get (point) "CREATED"))
              (heading (org-get-heading t t t t)))
         (when (and created (not (string-match "^\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" heading)))
           ;; Add timestamp if not already present (check for date pattern)
           (org-edit-headline (format "%s %s" created heading))
           (setq formatted-count (1+ formatted-count)))))
     nil 'tree)

    (message "Formatted %d log entries with timestamps" formatted-count)))

;; Refile target verification function
(defun my-gtd-refile-verify-target ()
  "Verify that refile target is a level 2 heading."
  (and (org-at-heading-p)
       (= (org-current-level) 2)))

;; Agenda skip functions for filtering items
(defun my-gtd-day-agenda-skip-todo-p ()
  "Skip function for GTD day agenda TODO items."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (scheduled (org-get-scheduled-time (point)))
        (deadline (org-get-deadline-time (point)))
        (priority (org-get-priority (thing-at-point 'line t)))
        (gtd-type (org-entry-get-with-inheritance "GTD_TYPE"))
        (tags (org-get-tags-at)))
    (cond
     ;; Skip scheduled/deadline items - they'll show in the calendar section
     ((or scheduled deadline) subtree-end)
     ;; For ad-hoc todos (not in projects/areas): show unless priority is E
     ((and (not (member gtd-type '("project" "area")))
           (not (member "PROJECT" tags))
           (not (member "AREA" tags))
           (not (= priority 0))) nil)  ; E priority = 0
     ;; For project/area todos: show only if priority is A or B
     ((and (or (member gtd-type '("project" "area"))
               (member "PROJECT" tags)
               (member "AREA" tags))
           (or (= priority 4000)  ; A priority
               (= priority 3000))) nil)  ; B priority
     ;; Skip everything else
     (t subtree-end))))

(defun my-gtd-day-agenda-skip-project-p ()
  "Skip function for GTD day agenda project listings."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (priority (org-get-priority (thing-at-point 'line t))))
    (when (< priority 1000)
      subtree-end)))

;; Custom category function for agenda display
(defun my-org-agenda-category-short ()
  "Return project/area name for tasks within projects/areas, or shortened name for other files."
  (let ((gtd-type-current (org-entry-get (point) "GTD_TYPE"))
        (gtd-type-inherited (org-entry-get-with-inheritance "GTD_TYPE")))
    (cond
     ;; If current heading has GTD_TYPE (i.e., it's the project/area heading itself), return empty
     ((member gtd-type-current '("project" "area"))
      "")
     ;; If we're inside a project/area (inherited GTD_TYPE), show the project/area name
     ((member gtd-type-inherited '("project" "area"))
      (let ((project-name (save-excursion
                            (save-restriction
                              (widen)
                              (goto-char (point-min))
                              (when (org-at-heading-p)
                                (org-get-heading t t t t))))))  ; Strip priority, tags, TODO, and comments
        ;; Limit to 19 characters (or 18 + ellipsis if truncated)
        (if (and project-name (> (length project-name) 19))
            (concat (substring project-name 0 18) "…")
          (or project-name ""))))
     ;; For non-project/area files, show category based on filename or top-level heading
     (t
      (let ((category-name
             (or
              ;; Try to get the top-level heading
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (point-min))
                  (when (org-at-heading-p)
                    (org-get-heading t t t t))))  ; Strip priority, tags, TODO, and comments
              ;; Fallback to filename
              (when (buffer-file-name)
                (let* ((file-name (file-name-base (buffer-file-name)))
                       ;; Strip timestamp pattern YYYY-MM-DD- from beginning
                       (cleaned-name (replace-regexp-in-string "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-" "" file-name)))
                  cleaned-name)))))
        ;; Limit to 19 characters (or 18 + ellipsis if truncated)
        (if (and category-name (> (length category-name) 19))
            (concat (substring category-name 0 18) "…")
          (or category-name "")))))))

;; GTD day agenda builder function
(defun my-gtd-build-day-agenda (local-dir local-areas local-projects shared-dir shared-areas shared-projects)
  "Build day agenda command with specified directories."
  `("d" "Day" ((agenda "" ((org-agenda-span 1)
                           (org-agenda-skip-scheduled-if-done t)
                           (org-agenda-skip-deadline-if-done t)
                           (org-agenda-skip-timestamp-if-done t)))
               (todo "TODO" ((org-agenda-overriding-header "Inbox items")
                             (org-agenda-files '(,my-gtd-local-inbox ,my-gtd-shared-inbox))))
               (todo "TODO" ((org-agenda-overriding-header "Local ad-hoc and high-prio project tasks")
                             (org-agenda-skip-function 'my-gtd-day-agenda-skip-todo-p)
                             (org-agenda-files '(,local-dir ,local-areas ,local-projects))))
               (tags "GTD_TYPE=\"project\"" ((org-agenda-overriding-header "Local projects")
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-sorting-strategy '(priority-down))
                                 (org-agenda-skip-function 'my-gtd-day-agenda-skip-project-p)
                                 (org-agenda-files '(,local-projects))))
               (todo "TODO" ((org-agenda-overriding-header "Shared ad-hoc and high-prio project tasks")
                             (org-agenda-skip-function 'my-gtd-day-agenda-skip-todo-p)
                             (org-agenda-files '(,shared-dir ,shared-areas ,shared-projects))))
               (tags "GTD_TYPE=\"project\"" ((org-agenda-overriding-header "Shared projects")
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-sorting-strategy '(priority-down))
                                 (org-agenda-skip-function 'my-gtd-day-agenda-skip-project-p)
                                 (org-agenda-files '(,shared-projects)))))))

;; GTD day agenda definition using current directories
(defconst my-gtd-day-agenda
  (my-gtd-build-day-agenda my-gtd-local-dir my-gtd-local-areas my-gtd-local-projects
                           my-gtd-shared-dir my-gtd-shared-areas my-gtd-shared-projects))

(use-package org-node
  :ensure
  :custom
  (org-mem-do-sync-with-org-id t)
  (org-mem-watch-dirs (list my-org-local-dir my-org-shared-dir))
  :config
  (org-mem-updater-mode)
  (org-node-cache-mode)
  (org-node-backlink-mode)
  :bind
  (("M-s M-f" . org-node-find)
   ("M-s M-i" . org-node-insert-link)))

(use-package envrc
  :ensure
  :hook (elpaca-after-init . envrc-global-mode))

(use-package tree-sitter
  :ensure
  :unless (my-windows-p)
  :custom
  (treesit-font-lock-level 4))

(defun lsp-corfu-setup ()
  "Configure Corfu for LSP completions."
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))

(use-package lsp-mode
  :ensure
  :custom
  (lsp-keymap-prefix "<f5>")
  (lsp-completion-provider :none)
  (lsp-pylsp-plugins-black-enabled t)
  :hook
  (python-mode . lsp)
  (python-ts-mode . lsp)
  (lsp-mode . lsp-corfu-setup)
  :commands lsp
  :bind
  (:map lsp-mode-map
        ([M-down-mouse-1] . mouse-set-point)
        ([M-mouse-1] . lsp-find-definition)
        ([M-mouse-3] . xref-go-back)))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode)

(use-package scala-ts-mode
  :ensure
  :interpreter "scala"
  :mode "\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'"
  :hook (scala-ts-mode . lsp))

(use-package lsp-metals
  :ensure
  :after (lsp-mode scala-ts-mode)
  :bind
  (:map scala-ts-mode-map
        ("<f5> I" . lsp-metals-build-import)))

(use-package pet
  :ensure
  :after envrc
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(use-package jarchive
  :ensure
  :unless (my-windows-p)
  :config
  (jarchive-setup))

(use-package copilot
  :ensure
  :when my-use-copilot
  :after hydra
  :custom
  (copilot-idle-delay 0.3)
  :custom-face
  (copilot-overlay-face ((t (:foreground "DarkOrchid1" :slant italic))))
  :config
  (add-to-list 'warning-suppress-log-types '(copilot copilot-no-mode-indent))
  (defhydra my-copilot-accept-completion (copilot-mode-map "C-M-<tab>")
    "Accept Copilot completion"
    ("C-M-<tab>" copilot-accept-completion "Accept" :color blue)
    ("C-M-f" copilot-accept-completion-by-word "By word")
    ("C-M-e" copilot-accept-completion-by-line "By line"))
  :hook
  (prog-mode . copilot-mode))

(use-package yaml-mode
  :ensure)

(use-package markdown-mode
  :ensure
  :interpreter "markdown")

(use-package dockerfile-mode
  :ensure)

(use-package graphql-mode
  :ensure)

(use-package fish-mode
  :ensure)

(use-package just-mode
  :ensure)
