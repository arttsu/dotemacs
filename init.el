(require 'cl-lib)

;;; Elpaca

;; https://github.com/progfolio/elpaca

;;;; Bootstrap Elpaca

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

;;;; Enable Elpaca's use-package support

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;; Platform helpers

(defun my-linux-p ()
  (eq system-type 'gnu/linux))

(defun my-macos-p ()
  (eq system-type 'darwin))

(defun my-windows-p ()
  (eq system-type 'windows-nt))

;;; Local init.el

;;;; Set default values for local variables

(setq my-font (cond ((my-linux-p) "Liberation Mono")
                    ((my-macos-p) "Menlo")
                    ((my-windows-p) "Cascadia Code")))
(setq my-font-height 150)

(setq my-modus-themes-startup-theme 'modus-operandi)

(setq my-vterm-shell nil)

(setq my-use-ripgrep nil)

(setq my-use-jinx nil)

(setq my-org-local-dir "~/org-local")
(setq my-org-shared-dir "~/org-shared")

(setq my-day-agenda-include-shared-by-default nil)

(setq my-use-tree-sitter nil)

(setq my-use-jarchive nil)

(setq my-use-copilot nil)

(setq my-use-kubel nil)

;;;; Load local init

(let ((local-init (expand-file-name "local-init.el" user-emacs-directory)))
  (if (file-exists-p local-init)
      (progn
        (load local-init)
        (message "Loaded local init."))
    (message "No local init.")))

;;; Custom file

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; MacOS Exec Path From Shell

;; https://github.com/purcell/exec-path-from-shell

(use-package exec-path-from-shell
  :ensure
  :when (my-macos-p)
  :config
  (exec-path-from-shell-initialize))

;;; Core setup

;;;; Core helpers

(defun my-jump-home ()
  (interactive)
  (find-file "~/"))

(defun my-pop-mark ()
  (interactive)
  (set-mark-command '(4)))

;;;; use-package Emacs

(defun my-disabled-window-toggle-side-windows ()
  (interactive)
  (user-error "window-toggle-side-window: Disabled, as it seems to be breaking easysessions."))

(use-package emacs
  :custom
  (create-lockfiles nil)
  (make-backup-files nil)
  (global-auto-revert-non-file-buffers t)
  (inhibit-startup-message t)
  (initial-major-mode 'text-mode)
  (initial-scratch-message "✅ All systems go! 🚀🪐✨")
  (visible-bell t)
  (indent-tabs-mode nil)
  (tab-width 4)
  (require-final-newline t)
  (save-interprogram-paste-before-kill t)
  (epg-pinentry-mode 'loopback)
  (disabled-command-function nil)
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  :config
  (when (and my-font my-font-height)
    (set-face-attribute 'default nil :font my-font :height my-font-height)
    (set-frame-font my-font nil t))
  (when (my-windows-p)
    (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'append))
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (global-auto-revert-mode)
  (global-subword-mode)
  (tab-bar-mode)
  (tab-bar-history-mode)
  (repeat-mode)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  :bind
  (("C-c j x" . scratch-buffer)
   ("C-c j h" . my-jump-home)
   ("<f8>" . my-pop-mark)
   ("C-M-<return>" . tab-switch)
   ("M-g w" . forward-to-word)
   ("M-g W" . backward-to-word)
   ("C-M-; d" . duplicate-dwim)
   ("C-c d h" . erase-buffer)
   ("M-z" . zap-up-to-char)
   ("M-Z" . zap-to-char)
   ("C-x K" . crux-delete-file-and-buffer)
   ("C-x E" . eval-buffer)
   ("C-x w s" . my-disabled-window-toggle-side-windows)))

;;; Dired

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

;;; Project

(use-package project
  :config
  (add-to-list 'project-switch-commands '(project-dired "Dired" "<return>") t))

;;; Imenu

(defun my-elisp-imenu-index ()
  (let* ((my-generic '(("Sections" "^;;; \\(.*\\)$" 1)
                       ("Subsections" "^;;;; \\(.*\\)$" 1)
                       ("Use-packages" "^\\s-*(use-package!?\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)))
         (generic (save-excursion (imenu--generic-function my-generic)))
         (default (save-excursion (imenu-default-create-index-function))))
    (append generic default)))

(defun my-elisp-imenu-setup ()
  (setq-local imenu-create-index-function 'my-elisp-imenu-index))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-imenu-setup)

;;; Savehist

(use-package savehist
  :config
  (savehist-mode))

;;; Super Save

;; https://github.com/bbatsov/super-save

(use-package super-save
  :ensure
  :custom
  (super-save-all-buffers t)
  (super-save-auto-save-when-idle t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-silent t)
  (super-save-exclude '(".sbt"
                        "project/"
                        ".gpg"))
  (auto-save-default nil)
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode))

;;; easysession

;; https://github.com/jamescherti/easysession.el

(defun my-easysession-visible-buffer-list ()
  (let ((visible-buffers '()))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (or
                  (string= (buffer-name buffer) "*scratch*")
                  (get-buffer-window buffer 'visible)
                  (and (bound-and-true-p tab-bar-mode)
                       (fboundp 'tab-bar-get-buffer-tab)
                       (tab-bar-get-buffer-tab buffer t nil))))
        (push buffer visible-buffers)))
    visible-buffers))

(defun my-easysession-empty ()
  (when (and (boundp 'tab-bar-mode) tab-bar-mode)
    (tab-bar-close-other-tabs)
    (tab-bar-rename-tab ""))
  (delete-other-windows)
  (scratch-buffer))

(defun my-easysession-reset ()
  (interactive)
  (when (yes-or-no-p "Reset current session?")
    (my-easysession-empty)))

;; TODO: Custom load and save handlers for non-file-visiting buffers
;; https://github.com/jamescherti/easysession.el?tab=readme-ov-file#how-to-create-custom-load-and-save-handlers-for-non-file-visiting-buffers

;; TODO: try buffer-terminator and/or easysession-reset

(use-package easysession
  :ensure
  :custom
  (easysession-mode-line-misc-info t)
  (easysession-save-interval (* 5 60))
  (easysession-buffer-list-function 'my-easysession-visible-buffer-list)
  (easysession-switch-to-exclude-current t)
  :init
  (unless noninteractive
    (add-hook 'emacs-startup-hook 'easysession-load-including-geometry 102)
    (add-hook 'emacs-startup-hook 'easysession-save-mode 103))
  :config
  (add-hook 'easysession-new-session-hook 'my-easysession-empty)
  :bind
  (("<f12> <f12>" . easysession-switch-to)
   ("<f12> s" . easysession-save)
   ("<f12> S" . easysession-save-as)
   ("<f12> k" . easysession-delete)
   ("<f12> r" . easysession-rename)
   ("<f12> x" . my-easysession-reset)))

;;; Modus Themes

;; https://protesilaos.com/emacs/modus-themes

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
  (modus-themes-load-theme my-modus-themes-startup-theme))

;;; Ace Window

;; https://github.com/abo-abo/ace-window

(use-package ace-window
  :ensure
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (aw-scope 'frame)
  :bind
  ("M-o" . ace-window))

;;; Vertico

;; https://github.com/minad/vertico

(use-package vertico
  :ensure
  :demand
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))
  (add-to-list 'vertico-multiform-categories
               '(consult-ripgrep buffer indexed))
  :bind
  (:map vertico-map
        ("C-;" . vertico-quick-insert)))

;;; Orderless

;; https://github.com/oantolin/orderless

(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

;;; Corfu

;; https://github.com/minad/corfu

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

;;; Consult

;; https://github.com/minad/consult

;; TODO: Use 'consult-flymake'?
(use-package consult
  :ensure
  :config
  ;; Don't live preview buffers to avoid triggering package loading (esp. Org)
  (consult-customize consult-buffer :preview-key "M-.")
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)
   ("M-g M-g" . consult-goto-line)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-g o" . consult-outline)
   ("M-s l" . consult-line)
   ("M-s k" . consult-keep-lines)
   ("M-s f" . consult-focus-lines)
   ("M-s r" . consult-ripgrep)))

;;; Avy

;; https://github.com/abo-abo/avy

(use-package avy
  :ensure
  :custom
  (avy-single-candidate-jump t)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'my-avy-embark-act)
  (setf (alist-get ?\; avy-dispatch-alist) 'my-avy-embark-dwim)
  :bind
  (("C-;" . avy-goto-char-timer)
   ("M-;" . avy-pop-mark)
   ("M-g g" . avy-goto-line)
   ("M-g G" . avy-goto-end-of-line)
   ("M-g h" . avy-goto-heading-timer)
   ("M-g s" . avy-goto-word-1)
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

;;;; Avy Embark actions

(defun my-avy-embark-act (target-point)
  (goto-char target-point)
  (embark-act))

(defun my-avy-embark-dwim (target-point)
  (goto-char target-point)
  (embark-dwim))

;;; Embark

;; https://github.com/oantolin/embark

(use-package embark
  :ensure
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

;;;; Embark-Consult

(use-package embark-consult
  :ensure
  :after (embark consult))

;;; Link Hint

;; https://github.com/noctuid/link-hint.el

(use-package link-hint
  :ensure
  :bind
  (("C-c f" . link-hint-open-link)
   ("C-c y" . link-hint-copy-link)))

;;; Smartparens

;; https://github.com/Fuco1/smartparens

(use-package smartparens
  :ensure
  :custom
  (sp-highlight-pair-overlay nil)
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

;;; crux

;; https://github.com/bbatsov/crux

(use-package crux
  :ensure
  :bind
  (("C-o" . crux-smart-open-line)
   ("C-S-o" . crux-smart-open-line-above)
   ("C-^" . crux-top-join-line)
   ("C-M-; D" . crux-duplicate-and-comment-current-line-or-region)))

;;; Whole Line or Region

;; https://github.com/purcell/whole-line-or-region

(use-package whole-line-or-region
  :ensure
  :demand
  :config
  (whole-line-or-region-global-mode)
  :bind
  ("M-/" . whole-line-or-region-comment-dwim))

;;; Expand Region

;; https://github.com/magnars/expand-region.el

(use-package expand-region
  :ensure
  :bind
  (("C-=" . er/expand-region)))


;;; Multiple Cursors

;; https://github.com/magnars/multiple-cursors.el

(use-package multiple-cursors
  :ensure
  :bind
  (("C-c k l" . mc/edit-lines)
   ("C-+" . mc/mark-next-like-this)
   ("C-c k m" . mc/mark-all-dwim)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-<return>" . set-rectangular-region-anchor)))

;;; Go to Char

(use-package iy-go-to-char
  :ensure (:host github :repo "arttsu/iy-go-to-char")
  :bind
  (("M-g f" . iy-go-to-char)
   ("M-g F" . iy-go-to-char-backward)
   ("M-g t" . iy-go-up-to-char)
   ("M-g T" . iy-go-up-to-char-backward)
   ("M-g ;" . iy-go-to-or-up-to-continue)
   ("M-g ," . iy-go-to-or-up-to-continue-backward)))

;;; Yasnippet

;; https://github.com/joaotavora/yasnippet

(use-package yasnippet
  :ensure
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-global-mode))

;; TODO: Try https://github.com/elken/yasnippet-capf.

;;; Hydra

;; https://github.com/abo-abo/hydra

(use-package hydra
  :ensure)

;;; page-break-lines

;; https://github.com/purcell/page-break-lines

(use-package page-break-lines
  :ensure
  :config
  (global-page-break-lines-mode))

;;; Rainbow Delimiters

;; https://github.com/Fanael/rainbow-delimiters

(use-package rainbow-delimiters
  :ensure
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Transient

;; https://github.com/magit/transient

;; Transient is a dependency of Magit. Installing it explicitly to
;; ensure I get a recent version to avoid compatibility issues.

(use-package transient
  :ensure)

;;; Magit

;; https://magit.vc/

(use-package magit
  :ensure
  :bind
  (("C-c g" . magit-file-dispatch)))

;;; Vterm

;; https://github.com/akermu/emacs-libvterm

(defun my-vterm-unbind-keys ()
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

(defun my-vterm--new-project-buffer (name root)
  (let ((default-directory root))
    (vterm name)
    (switch-to-buffer name)))

(defun my-vterm-project (&optional prefix)
  (interactive "P")
  (if-let ((current-project (project-current)))
      (let ((root (project-root current-project))
            (target-buffer-name (format "*%s: vterm*" (project-name current-project))))
        (if (get-buffer target-buffer-name)
            (if prefix
                (let ((new-buffer-name (generate-new-buffer-name target-buffer-name)))
                  (my-vterm--new-project-buffer new-buffer-name root))
              (switch-to-buffer target-buffer-name))
          (my-vterm--new-project-buffer target-buffer-name root)))
    (message "vterm-project: No current project.")))

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
  (add-hook 'vterm-mode-hook 'my-vterm-unbind-keys)
  :bind
  (("C-x v" . vterm)
   ("C-x 4 v" . vterm-other-window)
   ("C-x p v" . my-vterm-project)))

;;; Ripgrep

;; https://github.com/dajva/rg.el

(use-package rg
  :when my-use-ripgrep
  :ensure
  :bind
  (("M-s R" . rg)
   ("C-x p g" . rg-project)))

;;; Jinx

;; https://github.com/minad/jinx

(use-package jinx
  :when my-use-jinx
  :ensure
  :custom
  (jinx-languages "en_US ru de_DE es_ES")
  :hook
  (emacs-startup . global-jinx-mode)
  :bind
  (("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages)))

;;; GPTel

;; https://github.com/karthink/gptel

(define-derived-mode my-gptel-mode org-mode "MyGPT")

(defun my-gptel-mode-setup ()
  (org-mode)
  (gptel-mode))

(use-package gptel
  :ensure
  :demand
  :custom
  (gptel-model 'gpt-4o)
  (gptel-default-mode 'org-mode)
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (add-hook 'gptel-mode-hook 'toggle-truncate-lines)
  (add-hook 'my-gptel-mode-hook 'my-gptel-mode-setup)
  (add-to-list 'auto-mode-alist '("\\.gptel\\'" . my-gptel-mode))
  :bind
  (("C-c SPC" . gptel)
   ("C-c m" . gptel-menu)
   :map gptel-mode-map
   ("C-c C-c" . gptel-send)))

;;; TODO: EMMS

;;; Ledger

;; https://github.com/ledger/ledger-mode

(use-package ledger-mode
  :ensure
  :custom
  (ledger-default-date-format "%Y-%m-%d")
  :config
  (ledger-reports-add "bal-this-month" "%(binary) -f %(ledger-file) --invert -p \"this month\" -S amount bal ^income ^expenses")
  (ledger-reports-add "bal-last-month" "%(binary) -f %(ledger-file) --invert -p \"last month\" -S amount bal ^income ^expenses")
  (ledger-reports-add "bal-this-month-vs-budget" "%(binary) -f %(ledger-file) --monthly -p \"this month\" --budget bal ^expenses")
  (ledger-reports-add "bal-last-month-vs-budget" "%(binary) -f %(ledger-file) --monthly -p \"last month\" --budget bal ^expenses")
  (ledger-reports-add "this-month-unbudgeted" "%(binary) -f %(ledger-file) --monthly -p \"this month\" --unbudgeted bal ^expenses")
  (ledger-reports-add "last-month-unbudgeted" "%(binary) -f %(ledger-file) --monthly -p \"last month\" --unbudgeted bal ^expenses"))

;;; Anki Editor

;; https://github.com/louietan/anki-editor

(use-package anki-editor
  :ensure)

;;; Kubel

;; https://github.com/abrochard/kubel

(use-package kubel
  :ensure
  :when my-use-kubel
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

;;; Org

;; TODO: Split into pages

(defun my-inbox-path (org-dir) (expand-file-name "agenda/local-inbox.org" org-dir))

(defun my-org-entry-scheduled-or-deadline (point)
  (or (org-get-scheduled-time (point)) (org-get-deadline-time (point))))

(defun my-org-get-top-level-heading ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (org-at-heading-p)
        (org-get-heading t t t t)))))

(defconst my-org-contexts '("Local" "Shared"))

(defun my-org-context-dir (context)
  (cond ((string= context "Local") my-org-local-dir)
        ((string= context "Shared") my-org-shared-dir)
        (t (error "Invalid context: %s" context))))

(defun my-org-now-timestamp ()
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun my-org-priority-char-to-cookie (priority-char)
  (let ((char (cond ((memq priority-char '(?\r ?\n)) ?D)
                    ((and (>= priority-char ?a) (<= priority-char ?e)) (upcase priority-char))
                    ((and (>= priority-char ?A) (<= priority-char ?E)) priority-char)
                    (t ?D))))
    (format "[#%c]" char)))

(defun my-org-require-at-heading ()
  (unless (org-at-heading-p) (error "Must be at a heading")))

(defun my-reset-checklist ()
  (interactive)
  (my-org-require-at-heading)
  (when (not (string= (org-entry-get (point) "STYLE") "checklist"))
    (error "Reset checklist: Not a checklist"))
  (when (yes-or-no-p "Reset the checklist?")
    (org-map-entries (lambda () (org-todo "TODO")) nil 'tree)
    ;; Remove "TODO" set by 'org-map-entries' on the checklist heading itself.
    (org-todo "")))

(defun my-checklist-do-auto-advance ()
  (let ((point-before (point)))
    (org-forward-heading-same-level 1)
    (when (= (point) point-before)
      (org-up-heading-safe))))

(defun my-checklist-auto-advance ()
  (ignore-errors
    (when (and (not (eq this-command 'org-agenda-todo))
               (string= (org-entry-get-with-inheritance "STYLE") "checklist")
               (string= org-state "DONE"))
      (run-with-idle-timer 0 nil 'my-checklist-do-auto-advance))))

(defun my-org-remove-priority-when-done ()
  (ignore-errors
    (when (string= org-state "DONE")
      (org-entry-put (point) "PRIORITY" nil))))

(defun my-org-complete-as-wont-do ()
  (interactive)
  (my-org-require-at-heading)
  (when (not (string= (org-get-todo-state) "DONE"))
    (let ((heading (org-get-heading t t t t)))
      (org-todo 'done)
      (org-edit-headline (format "+%s+" heading)))))

(defun my-org-undo-wont-do ()
  (ignore-errors
    (when (not (string= org-state "DONE"))
      (let ((heading (org-get-heading t t t t)))
        (when (string-match (rx string-start "+" (group (1+ anychar)) "+" string-end) heading)
          (org-edit-headline (match-string 1 heading)))))))

(defconst my-org-default-timestamp "[1900-01-01 Mon 00:00]")

(defun my-org-extract-created-timestamp ()
  (or (org-entry-get (point) "CREATED") my-org-default-timestamp))

(defun my-org-extract-closed-timestamp ()
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (if (re-search-forward (rx "CLOSED: " (group "[" (1+ (not "]")) "]")) nil t)
          (match-string-no-properties 1)
        my-org-default-timestamp))))

(defun my-sort-checklist ()
  (interactive)
  (my-org-require-at-heading)
  (org-sort-entries nil ?f 'my-org-extract-created-timestamp)
  (org-sort-entries nil ?f 'my-org-extract-closed-timestamp)
  ;; By priority.
  (org-sort-entries nil ?p)
  ;; By TODO state.
  (org-sort-entries nil ?o))

(defun my-sort-log ()
  (interactive)
  (my-org-require-at-heading)
  ;; Reverse sort by 'CREATED' timestamp.
  (org-sort-entries nil ?f (lambda () (- (org-time-string-to-seconds (my-org-extract-created-timestamp))))))

(defun my-org-has-children ()
  (my-org-require-at-heading)
  (save-excursion
    (org-goto-first-child)))

;; TODO: Use 'user-error' in other places where appropriate.
(defun my-sort-entries ()
  (interactive)
  (my-org-require-at-heading)
  (when (my-org-has-children)
    (let ((style (org-entry-get (point) "STYLE")))
      (cond ((string= style "checklist") (my-sort-checklist))
            ((string= style "log") (my-sort-log))
            (t (user-error "Sort Entries: No supported STYLE property found."))))))

(defun my-easysession-from-heading ()
  (interactive)
  (my-org-require-at-heading)
  (require 'org-node)
  (let* ((heading (org-get-heading t t t t))
         (session-title (file-name-sans-extension (org-node-title-to-basename heading)))
         (file-to-open (when (not (easysession--exists session-title)) buffer-file-name)))
    (easysession-switch-to-and-restore-geometry session-title)
    (when file-to-open (find-file file-to-open))))

(defun my-org-add-update ()
  (interactive)
  (condition-case nil
      (org-back-to-heading)
    (error (user-error "Point is not inside a heading.")))
  (org-end-of-subtree)
  (insert (concat "\n" "-----" "\n" "Update " (my-org-now-timestamp) ": ")))

;;;; Org Auto-format

(defun my-org-add-created-timestamp-to-heading ()
  (my-org-require-at-heading)
  (let* ((timestamp (my-org-extract-created-timestamp))
         (heading (org-get-heading t t t t))
         (clean-heading (replace-regexp-in-string (rx string-start "[" (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) space (= 3 letter) space (= 2 digit) ":" (= 2 digit) "] ")
                                                  ""
                                                  heading)))
    (unless (string= timestamp my-org-default-timestamp)
      (org-edit-headline (format "%s %s" timestamp clean-heading)))))

(defun my-org-update-attachments-heading ()
  (my-org-require-at-heading)
  (save-excursion
    (org-mark-subtree)
    (forward-line)
    (delete-region (region-beginning) (region-end))
    (deactivate-mark)
    (let ((dir (org-attach-dir-from-id (org-entry-get-with-inheritance "ID"))))
      (when (file-directory-p dir)
        (let* ((paths (directory-files dir t))
               (attachments (cl-remove-if (lambda (path) (member (file-name-nondirectory path) '("." ".."))) paths)))
          (dolist (attachment attachments)
            (insert (format "- [[file:%s][%s]]\n" attachment (file-name-nondirectory attachment)))))))))

(defun my-org-auto-format ()
  (interactive)
  (ignore-errors
    (org-map-entries (lambda () (org-map-entries 'my-org-add-created-timestamp-to-heading nil 'tree)) "STYLE=\"log\"" 'file))
  (ignore-errors
    (org-map-entries 'my-sort-entries "STYLE=\"checklist\"|STYLE=\"log\"" 'file))
  (ignore-errors
    (org-map-entries 'my-org-update-attachments-heading "SYNC_ATTACH" 'file)))

(defun my-list-org-files (dir)
  (directory-files dir t (rx ".org" string-end)))

;;;; Org Refile Note

(defun my-org-do-refile-note (refile-f)
  (let ((original-targets org-refile-targets))
    (unwind-protect
        (let* ((local-note-files (my-list-org-files (expand-file-name "notes" my-org-local-dir)))
               (shared-note-files (my-list-org-files (expand-file-name "notes" my-org-shared-dir)))
               (note-targets (append local-note-files shared-note-files)))
          (setq org-refile-targets `((,note-targets :maxlevel 3)))
          (apply refile-f ()))
      (setq org-refile-targets original-targets))))

(defun my-org-refile-note ()
  (interactive)
  (my-org-do-refile-note 'org-refile))

(defun my-org-refile-copy-note ()
  (interactive)
  (my-org-do-refile-note 'org-refile-copy))

;;;; Org Capture

(defun my-capture-template-path (name)
  (expand-file-name (concat "capture-templates/" name ".txt") user-emacs-directory))

(defun my-capture-templates (org-dir)
  (let ((inbox-target `(file+headline ,(my-inbox-path org-dir) "Items")))
    `(("i" "Inbox")
      ("ii" "note" entry ,inbox-target (file ,(my-capture-template-path "note")))
      ("il" "note link" entry ,inbox-target (file ,(my-capture-template-path "note-link")))
      ("iI" "todo" entry ,inbox-target (file ,(my-capture-template-path "todo")))
      ("iL" "todo link" entry ,inbox-target (file ,(my-capture-template-path "todo-link"))))))

(defun my-capture-note (&optional prefix)
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "ii"))
        ((equal prefix '(4)) (org-capture nil "il"))
        (t (error "Capture note: Invalid prefix argument: %s" prefix))))

(defun my-capture-todo (&optional prefix)
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "iI"))
        ((equal prefix '(4)) (org-capture nil "iL"))
        (t (error "Capture to-do: Invalid prefix argument: %s" prefix))))

;;;; Org Agenda

(defun my-agenda-files (org-dir)
  (list (expand-file-name "agenda" org-dir)
        (expand-file-name "agenda/projects" org-dir)
        (expand-file-name "agenda/areas" org-dir)))

(defun my-agenda-category-short ()
  (if (derived-mode-p 'org-mode)
      (if-let ((type (org-entry-get (point) "MY_TYPE")))
          "" ;; Project or area - no category necessary.
        (if-let ((top-level-heading (my-org-get-top-level-heading)))
            (if (> (length top-level-heading) 19)
                (concat (substring top-level-heading 0 18) "…")
              top-level-heading)
          (buffer-file-name)))
    ""))

(defun my-day-agenda-low-prio-todo ()
  (let ((priority (org-get-priority (thing-at-point 'line t)))
        (type (org-entry-get-with-inheritance "MY_TYPE")))
    (or (= priority 0)
        (and type (<= priority 2000)))))

(defun my-day-agenda-skip-todo ()
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (when (or (my-org-entry-scheduled-or-deadline (point))
              (my-day-agenda-low-prio-todo))
      subtree-end)))

(defun my-day-agenda-skip-project ()
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (priority (org-get-priority (thing-at-point 'line t))))
    (when (= priority 0)
      subtree-end)))

(defun my-day-agenda (files)
  `((agenda "" ((org-agenda-span 1)
                (org-agenda-skip-scheduled-if-done t)
                (org-agenda-skip-deadline-if-done t)
                (org-agenda-skip-timestamp-if-done t)
                (org-agenda-files ',files)))
    (todo "TODO" ((org-agenda-overriding-header "Non-scheduled To-dos")
                  (org-agenda-skip-function 'my-day-agenda-skip-todo)
                  (org-agenda-files ',files)))
    (tags "MY_TYPE=\"project\"" ((org-agenda-overriding-header "Projects")
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-sorting-strategy '(priority-down))
                                 (org-agenda-skip-function 'my-day-agenda-skip-project)
                                 (org-agenda-files ',files)))))

(defun my-day-agenda-commands ()
  (let* ((all-files (append (my-agenda-files my-org-local-dir) (my-agenda-files my-org-shared-dir)))
         (local-files (my-agenda-files my-org-local-dir))
         (default-files (if my-day-agenda-include-shared-by-default all-files local-files))
         (secondary-files (if my-day-agenda-include-shared-by-default local-files all-files))
         (secondary-label (if my-day-agenda-include-shared-by-default "Day w/o shared" "Day w/ shared")))
    `(("d" "Day" ,(my-day-agenda default-files))
      ("D" ,secondary-label ,(my-day-agenda secondary-files)))))

;;;; Create Project

(defun my-create-project-content (title priority-char)
  (let ((template (with-temp-buffer (insert-file-contents (my-capture-template-path "project")) (buffer-string)))
        (priority-cookie (my-org-priority-char-to-cookie priority-char))
        (id (org-id-new))
        (timestamp (my-org-now-timestamp)))
    (format template priority-cookie title id timestamp)))

(defun my-open-project-new-session (path)
  (let ((name (file-name-sans-extension (file-name-nondirectory path))))
    (easysession-switch-to-and-restore-geometry name)
    (find-file path)))

(defun my-create-project ()
  (interactive)
  (let ((context (completing-read "Context: " my-org-contexts nil t))
        (title (read-string "Title: "))
        (priority (read-char-choice "Priority [A-E, default D]: " '(?A ?B ?C ?D ?E ?a ?b ?c ?d ?e ?\r ?\n))))
    (when (string-empty-p title) (user-error "Create project: Title cannot be empty"))
    (let ((dir (expand-file-name "agenda/projects" (my-org-context-dir context))))
      (unless (file-directory-p dir) (make-directory dir t))
      (require 'org-node)
      (let* ((filename (org-node-title-to-basename title))
             (path (expand-file-name filename dir)))
        (with-temp-buffer (insert (my-create-project-content title priority)) (write-file path))
        (message "Project created: %s" path)
        (let ((choice (read-char-choice
                       "Open project: [c]urrent window, [o]ther window, new [t]ab, new [s]ession, [d]on't open: "
                       '(?c ?o ?t ?s ?d))))
          (cond ((eq choice ?c) (find-file path))
                ((eq choice ?o) (find-file-other-window path))
                ((eq choice ?t) (find-file-other-tab path))
                ((eq choice ?s) (my-open-project-new-session path))
                ((eq choice ?d) nil)))))))

;;;; Archive Project

(defun my-archive-project ()
  (interactive)
  (let ((file-path (buffer-file-name)))
    (when (yes-or-no-p "Archive this project?")
      (let* ((title (my-org-get-top-level-heading))
             (project-dir (file-name-directory file-path))
             (archive-dir (expand-file-name "archive" project-dir))
             (file-name (file-name-nondirectory file-path))
             (archive-path (expand-file-name file-name archive-dir)))
        (unless (file-directory-p archive-dir) (make-directory archive-dir t))
        (with-temp-buffer
          (insert-file-contents file-path)
          (goto-char (point-min))
          (org-mode)
          (when (org-at-heading-p)
            (org-entry-put (point) "PRIORITY" nil)
            (org-entry-put (point) "ARCHIVED" (my-org-now-timestamp)))
          (write-region (point-min) (point-max) file-path))
        (rename-file file-path archive-path)
        (set-visited-file-name archive-path)
        (set-buffer-modified-p nil)
        (revert-buffer t t)
        (message "Project archived: %s" archive-path)
        (let ((session-name (file-name-sans-extension file-name)))
          (cond ((string= (easysession-get-session-name) session-name) (when (yes-or-no-p "Delete the session and switch to 'main'?")
                                                                         (easysession-switch-to-and-restore-geometry "main")
                                                                         (easysession-delete session-name)))
                ((easysession--exists session-name) (when (yes-or-no-p (format "Delete the session '%s'? " session-name))
                                                      (easysession-delete session-name)))))))))

;;;; Create Area

(defun my-create-area-content (title)
  (let ((template (with-temp-buffer (insert-file-contents (my-capture-template-path "area")) (buffer-string)))
        (id (org-id-new))
        (timestamp (my-org-now-timestamp)))
    (format template title id timestamp)))

(defun my-create-area ()
  (interactive)
  (let ((context (completing-read "Context: " my-org-contexts nil t))
        (title (read-string "Title: ")))
    (when (string-empty-p title) (user-error "Create area: Title cannot be empty"))
    (let ((dir (expand-file-name "agenda/areas" (my-org-context-dir context))))
      (unless (file-directory-p dir) (make-directory dir t))
      (require 'org-node)
      (let* ((filename (org-node-title-to-basename title))
             (path (expand-file-name filename dir)))
        (with-temp-buffer (insert (my-create-area-content title)) (write-file path))
        (message "Area created: %s" path)
        (let ((choice (read-char-choice
                       "Open area: [c]urrent window, [o]ther window, new [t]ab, [d]on't open: "
                       '(?c ?o ?t ?d))))
          (cond ((eq choice ?c) (find-file path))
                ((eq choice ?o) (find-file-other-window path))
                ((eq choice ?t) (find-file-other-tab path))
                ((eq choice ?d) nil)))))))

;;;; Create Notes

;; TODO: Extract reading a file to a string to a helper.
(defun my-create-notes-content (title)
  (let ((template (with-temp-buffer (insert-file-contents (my-capture-template-path "notes")) (buffer-string)))
        (id (org-id-new))
        (timestamp (my-org-now-timestamp)))
    (format template title id timestamp)))

(defun my-create-notes ()
  (interactive)
  (let ((context (completing-read "Context: " my-org-contexts nil t))
        (title (read-string "Title: ")))
    (when (string-empty-p title) (user-error "Create notes: Title cannot be empty"))
    (let ((dir (expand-file-name "notes" (my-org-context-dir context)))
          (full-title (concat title " Notes")))
      (unless (file-directory-p dir) (make-directory dir t))
      (require 'org-node)
      (let* ((filename (org-node-title-to-basename full-title))
             (path (expand-file-name filename dir)))
        (with-temp-buffer (insert (my-create-notes-content full-title)) (write-file path))
        (message "Notes created: %s" path)
        (let ((choice (read-char-choice
                       "Open notes: [c]urrent window, [o]ther window, new [t]ab, [d]on't open: "
                       '(?c ?o ?t ?d))))
          (cond ((eq choice ?c) (find-file path))
                ((eq choice ?o) (find-file-other-window path))
                ((eq choice ?t) (find-file-other-tab path))
                ((eq choice ?d) nil)))))))

;;;; Org Ctrl-C Ctrl-C

(defun my-org-ctrl-c-ctrl-c-heading ()
  (cond ((string= (org-get-todo-state) "TODO") (org-todo 'done))
        ((string= (org-get-todo-state) "DONE") (org-todo 'todo))))

(defun my-org-ctrl-c-ctrl-c ()
  (interactive)
  (if (org-at-heading-p)
      (my-org-ctrl-c-ctrl-c-heading)
    (call-interactively #'org-ctrl-c-ctrl-c)))

;;;; Org Config

(use-package org
  :ensure
  :custom
  (org-startup-indented t)
  (org-startup-with-inline-images nil)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-use-speed-commands t)
  (org-use-sub-superscripts '{})
  (org-src-window-setup 'split-window-below)
  (org-confirm-babel-evaluate nil)
  (org-log-done 'time)
  (org-priority-lowest 69)
  (org-priority-default 68)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-habit-graph-column 60)
  (org-habit-show-done-always-green t)
  (org-attach-id-dir (expand-file-name "attachments" my-org-local-dir))
  (org-capture-templates (my-capture-templates my-org-local-dir))
  (org-agenda-files (append (my-agenda-files my-org-local-dir) (my-agenda-files my-org-shared-dir)))
  (org-refile-targets '((org-agenda-files :level . 2)))
  (org-agenda-custom-commands (my-day-agenda-commands))
  (org-agenda-prefix-format '((agenda . " %i %-20(my-agenda-category-short) %?-12t% s")
                              (todo . " %i %-20(my-agenda-category-short) ")
                              (tags . " %i %-20(my-agenda-category-short) ")
                              (search . " %i %-20(my-agenda-category-short) ")))
  (org-clock-persist 'history)
  (org-clock-auto-clockout-timer 600)
  (org-clock-out-remove-zero-time-clocks t)
  (org-log-into-drawer t)
  :config
  (require 'org-attach)
  (require 'org-id)
  (require 'org-habit)
  (add-hook 'org-after-refile-insert-hook 'my-org-auto-format)
  (add-hook 'org-after-todo-state-change-hook 'my-org-remove-priority-when-done -10)
  (add-hook 'org-after-todo-state-change-hook 'my-org-undo-wont-do -10)
  (add-hook 'org-after-todo-state-change-hook 'my-checklist-auto-advance 10)
  (org-clock-persistence-insinuate)
  (org-clock-auto-clockout-insinuate)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c i" . my-capture-note)
   ("C-c I" . my-capture-todo)
   ("C-c o c p" . my-create-project)
   ("C-c o c a" . my-create-area)
   ("C-c o c n" . my-create-notes)
   :map org-mode-map
   ("C-c o C-i" . org-id-get-create)
   ("C-c o r" . my-reset-checklist)
   ("C-c o f" . my-org-auto-format)
   ("C-c o x" . my-org-complete-as-wont-do)
   ("C-c o C-r" . my-org-refile-note)
   ("C-c o M-r" . my-org-refile-copy-note)
   ("C-c o u" . my-org-add-update)
   ("C-c C-c" . my-org-ctrl-c-ctrl-c)))

;;;; Org Side Windows

(add-to-list 'display-buffer-alist
             `(,(rx (or "*Org Agenda*" "*Agenda Commands*"))
               display-buffer-in-side-window
               (side . right)
               (slot . 0)
               (window-parameters . ((no-delete-other-windows . t)))
               (window-width . 100)
               (dedicated . t)))

(add-to-list 'display-buffer-alist
             `(,(rx string-start "CAPTURE-")
               display-buffer-in-direction
               (direction . bottom)
               (window . root)
               (window-height . 0.3)
               (dedicated . t)))

;;; Org Export

;;;; Org Export to GitHub-flavored Markdown

;; https://github.com/larstvei/ox-gfm

(use-package ox-gfm
  :ensure
  :after org)

;;; Org Modern

;; https://github.com/minad/org-modern

(defface my-done-checkbox-face
  '((t (:inherit org-done)))
  "Face for a 'done' Org mode checkbox.")

(defconst my-done-checkbox-rx (rx line-start
                                  (* space)
                                  (any "-" "+" "*")
                                  (+ space)
                                  "[X]"
                                  (* (not "\n"))))

(defun my-org-modern-font-lock-done-checkbox ()
  (font-lock-add-keywords 'org-mode
                          `((,my-done-checkbox-rx 0 'my-done-checkbox-face prepend))
                          'append))

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
  (org-modern-horizontal-rule nil)
  :custom-face
  (org-modern-tag ((t (:background "CornflowerBlue" :foreground "White" :slant italic))))
  :config
  (global-org-modern-mode)
  (my-org-modern-font-lock-done-checkbox))

;;; Org Node

;; https://github.com/meedstrom/org-node

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
   ("M-s M-i" . org-node-insert-link)
   ("M-s M-t" . org-node-add-tags-here)))

;;; Babel Restclient

;; https://github.com/alf/ob-restclient.el

(use-package ob-restclient
  :ensure
  :after org)

;;; envrc

;; https://github.com/purcell/envrc

(use-package envrc
  :ensure
  :hook (elpaca-after-init . envrc-global-mode))

;;; Tree-Sitter

;; https://emacs-tree-sitter.github.io/

(use-package tree-sitter
  :ensure
  :when my-use-tree-sitter
  :custom
  (treesit-font-lock-level 4))

;;; LSP Mode

;; https://github.com/emacs-lsp/lsp-mode

(defun my-lsp-corfu-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))

(use-package lsp-mode
  :ensure
  :custom
  (lsp-keymap-prefix "<f5>")
  (lsp-completion-provider :none)
  (lsp-pylsp-plugins-black-enabled t)
  (lsp-clojure-custom-server-command "~/.local/bin/clojure-lsp")
  :hook
  (python-mode . lsp)
  (python-ts-mode . lsp)
  (scala-mode . lsp)
  (scala-ts-mode .lsp)
  (clojure-mode . lsp)
  (lsp-mode . my-lsp-corfu-setup)
  :commands lsp
  :bind
  (:map lsp-mode-map
        ([M-down-mouse-1] . mouse-set-point)
        ([M-mouse-1] . lsp-find-definition)
        ([M-mouse-3] . xref-go-back)))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode)

;;; Scala

;;;; Scala TS Mode

;; https://github.com/KaranAhlawat/scala-ts-mode

(use-package scala-ts-mode
  :ensure
  :interpreter "scala"
  :mode (rx "." (or "scala" "sbt" "worksheet" "sc") string-end))

;;;; LSP Metals

;; https://github.com/emacs-lsp/lsp-metals

(use-package lsp-metals
  :ensure
  :after (lsp-mode scala-ts-mode)
  :bind
  (:map scala-ts-mode-map
        ("<f5> I" . lsp-metals-build-import)))

;;;; Jarchive

(use-package jarchive
  :ensure
  :when my-use-jarchive
  :config
  (jarchive-setup))

;;; Python

;;;; Python Pet

;; https://github.com/wyuenho/emacs-pet

(use-package pet
  :ensure
  :after envrc
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;;; Clojure

;;;; Clojure Mode

;; https://github.com/clojure-emacs/clojure-mode

(use-package clojure-mode
  :ensure)

;;;; CIDER

;; https://github.com/clojure-emacs/cider

(use-package cider
  :ensure
  :hook (clojure-mode . cider-mode)
  :custom
  (cider-save-file-on-load t))

;;; Copilot

;; https://github.com/copilot-emacs/copilot.el

(defun my-copilot-accept-completion-hydra ()
  (defhydra my-copilot-accept-completion (copilot-mode-map "C-M-<tab>")
    ("C-M-<tab>" copilot-accept-completion "Accept" :color blue)
    ("C-M-f" copilot-accept-completion-by-word "By word")
    ("C-M-e" copilot-accept-completion-by-line "By line")))

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
  (my-copilot-accept-completion-hydra)
  :hook
  (prog-mode . copilot-mode))

;;; Major Modes

;;;; YAML Mode

;; https://github.com/yoshiki/yaml-mode

(use-package yaml-mode
  :ensure)

;;;; Markdown Mode

;; https://github.com/jrblevin/markdown-mode

(use-package markdown-mode
  :ensure
  :interpreter "markdown")

;;;; Dockerfile Mode

;; https://github.com/spotify/dockerfile-mode

(use-package dockerfile-mode
  :ensure)

;;;; GraphQL Mode

;; https://github.com/davazp/graphql-mode

(use-package graphql-mode
  :ensure)

;;;; Fish Mode

;; https://github.com/emacsmirror/fish-mode

(use-package fish-mode
  :ensure)

;;;; Just Mode

;; https://github.com/leon-barrett/just-mode.el

(use-package just-mode
  :ensure)

;;; Lambda World

(use-package haskell-mode
  :ensure)

(use-package q-mode
  :ensure)
