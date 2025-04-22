(defun my-linux-p ()
  (eq system-type 'gnu/linux))

(defun my-windows-p ()
  (eq system-type 'windows-nt))

(defun my-macos-p ()
  (eq system-type 'darwin))

(setq my-font (cond ((my-linux-p) "Liberation Mono")
                    ((my-macos-p) "Menlo")
                    ((my-windows-p) "Cascadia Code")))
(setq my-font-height 125)

(setq my-vterm-shell nil)

(let ((path-to-local-config (expand-file-name "local.el" user-emacs-directory)))
  (if (file-exists-p path-to-local-config)
      (progn
        (load path-to-local-config)
        (message "Loaded local config."))
    (message "No local config.")))

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

(use-package emacs
  :custom
  (inhibit-splash-screen t)
  (initial-major-mode 'text-mode)
  (initial-scratch-message "✅ All systems go! 🚀🪐✨")
  (disabled-command-function nil)
  (save-interprogram-paste-before-kill t)
  (create-lockfiles nil)
  (make-backup-files nil)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (require-final-newline t)
  (indent-tabs-mode nil)
  (visible-bell t)
  (global-auto-revert-non-file-buffers t)
  :config
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (global-auto-revert-mode)
  (global-subword-mode)
  (tab-bar-mode)
  (tab-bar-history-mode)
  (repeat-mode))

(use-package magit
  :ensure
  :bind
  (("C-c g" . magit-file-dispatch)))

(use-package transient
  :ensure)

(defconst my-org-personal-dir (expand-file-name "~/org-personal"))
(defconst my-gtd-personal-dir (expand-file-name "gtd" my-org-personal-dir))
(defconst my-gtd-personal-inbox (expand-file-name "personal-inbox.org" my-gtd-personal-dir))
(defconst my-gtd-personal-areas (expand-file-name "areas" my-gtd-personal-dir))
(defconst my-gtd-personal-projects (expand-file-name "projects" my-gtd-personal-dir))
(defconst my-gtd-personal-dirs (list my-gtd-personal-dir
                                     my-gtd-personal-areas
                                     my-gtd-personal-projects))

(defconst my-org-open-dir (expand-file-name "~/org-open"))
(defconst my-gtd-open-dir (expand-file-name "gtd" my-org-open-dir))
(defconst my-gtd-open-projects (expand-file-name "projects" my-gtd-open-dir))
(defconst my-gtd-open-areas (expand-file-name "areas" my-gtd-open-dir))
(defconst my-gtd-open-dirs (list my-gtd-open-dir
                                 my-gtd-open-areas
                                 my-gtd-open-projects))

(defconst my-gtd-all-dirs (append my-gtd-personal-dirs my-gtd-open-dirs))

(defun my-org-remove-priority-when-done ()
  (when (string= org-state "DONE")
    (ignore-errors (org-entry-put (point) "PRIORITY" nil))))

(defun my-org-capture-template-path (name)
  (expand-file-name (concat "capture-templates/" name ".txt") user-emacs-directory))

(defconst my-gtd-personal-inbox-target `(file+headline ,my-gtd-personal-inbox "Inbox items"))

(defconst my-gtd-capture-templates
  `(("i" "Inbox")
    ("ii" "note" entry ,my-gtd-personal-inbox-target (file ,(my-org-capture-template-path "gtd-note")))
    ("il" "note link" entry ,my-gtd-personal-inbox-target (file ,(my-org-capture-template-path "gtd-note-link")))
    ("iI" "todo" entry ,my-gtd-personal-inbox-target (file ,(my-org-capture-template-path "gtd-todo")))
    ("iL" "todo link" entry ,my-gtd-personal-inbox-target (file ,(my-org-capture-template-path "gtd-todo-link")))))

(defun my-gtd-capture-note (&optional prefix)
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "ii"))
        ((equal prefix '(4)) (org-capture nil "il"))
        (t (error "Invalid prefix argument: %s" prefix))))

(defun my-gtd-capture-todo (&optional prefix)
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "iI"))
        ((equal prefix '(4)) (org-capture nil "iL"))
        (t (error "Invalid prefix argument: %s" prefix))))

(defconst my-gtd-day-agenda
  `("d" "Day" ((agenda "" ((org-agenda-span 1)
                           (org-agenda-skip-scheduled-if-done t)
                           (org-agenda-skip-deadline-if-done t)
                           (org-agenda-skip-timestamp-if-done t)))
               (todo "TODO" ((org-agenda-overriding-header "Ad-hoc tasks")
                             (org-agenda-files ',my-gtd-personal-dirs)))
               (tags "+PROJECT" ((org-agenda-overriding-header "Projects")
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-sorting-strategy '(priority-down))
                                 (org-agenda-files '(,my-gtd-personal-projects)))))))

(defun my-org-setup ()
  (setq-local fill-column 120)
  (auto-fill-mode 1))

(use-package org
  :ensure
  :custom
  (org-startup-folded 'showall)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-confirm-babel-evaluate nil)
  (org-use-sub-superscripts '{})
  (org-src-window-setup 'split-window-below)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-use-speed-commands t)
  (org-log-done 'time)
  (org-capture-templates my-gtd-capture-templates)
  (org-agenda-files my-gtd-all-dirs)
  (org-agenda-custom-commands `(,my-gtd-day-agenda))
  (org-refile-targets '((org-agenda-files :level . 2)))
  :config
  (add-hook 'org-mode-hook 'my-org-setup)
  (add-hook 'org-after-todo-state-change-hook 'my-org-remove-priority-when-done)
  :bind
  (("C-c c" . org-capture)
   ("C-c i" . my-gtd-capture-note)
   ("C-c I" . my-gtd-capture-todo)
   ("C-c a" . org-agenda)))

(use-package modus-themes
  :ensure
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-headings '((0 . (ultrabold 1.7))
                           (1 . (ultrabold 1.7))
                           (2 . (extrabold 1.5))
                           (3 . (extrabold 1.3))
                           (4 . (extrabold 1.1))
                           (t . (extrabold))))
  (modus-themes-variable-pitch-ui t)
  :config
  (modus-themes-load-theme 'modus-vivendi))

(set-face-attribute 'default nil :font my-font :height my-font-height)
(set-frame-font my-font nil t)

(when (my-windows-p)
  (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'append))

(use-package ace-window
  :ensure
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (aw-scope 'frame)
  :bind
  (("M-o" . ace-window)))

(use-package vertico
  :ensure
  :demand
  :config
  (vertico-mode)
  :bind
  (:map vertico-map
        ("C-;" . vertico-quick-insert)))

(use-package savehist
  :config
  (savehist-mode))

(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure
  :config
  ;; Don't live preview buffers to avoid triggering package loading (esp. Org).
  ;; See https://github.com/minad/consult#live-previews
  (consult-customize consult-buffer :preview-key "M-.")
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-s l" . consult-line)
   ("M-s k" . consult-keep-lines)
   ("M-s f" . consult-focus-lines)))

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

(defun my-easysession-not-main-p ()
  (not (string= (easysession-get-current-session-name) "main")))

(defun my-easysession-visible-buffer-list ()
  (let ((visible-buffers '()))
    (dolist (buffer (buffer-list))
      (when (or (get-buffer-window buffer 'visible)
                (and (bound-and-true-p tab-bar-mode)
                     (fboundp 'tab-bar-get-buffer-tab)
                     (tab-bar-get-buffer-tab buffer t nil)))
        (push buffer visible-buffers)))
    visible-buffers))

(defun my-easysession-setup-minimal ()
  (when (and (boundp 'tab-bar-mode) tab-bar-mode)
    (tab-bar-close-other-tabs)
    (tab-bar-rename-tab ""))
  (delete-other-windows)
  (scratch-buffer))

(use-package easysession
  :ensure
  :commands (easysession-switch-to
             easysession-save-as
             easysession-save-mode
             easysession-load-including-geometry)
  :custom
  (easysession-mode-line-misc-info t)
  (easysession-save-interval (* 5 60))
  (easysession-buffer-list-function 'my-easysession-visible-buffer-list)
  (easysession-save-mode-predicate 'my-easysession-not-main-p)
  (easysession-switch-to-exclude-current t)
  :init
  (add-hook 'emacs-startup-hook 'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook 'easysession-save-mode 103)
  :config
  (add-hook 'easysession-new-session-hook 'my-easysession-setup-minimal)
  :bind
  (("<f12> <f12>" . easysession-switch-to)
   ("<f12> s" . easysession-save)
   ("<f12> k" . easysession-delete)))

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

(use-package whole-line-or-region
  :ensure
  :demand
  :config
  (whole-line-or-region-global-mode)
  :bind
  (("M-/" . whole-line-or-region-comment-dwim)))

(use-package avy
  :custom
  (avy-single-candidate-jump t)
  :bind
  (("C-;" . avy-goto-char-timer)
   ("M-;" . avy-pop-mark)
   ("M-g g" . avy-goto-line)
   ("M-g G" . avy-goto-end-of-line)
   ("M-g h" . avy-org-goto-heading-timer)
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

(use-package org-auto-tangle
  :ensure
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-modern
  :ensure
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  :config
  (global-org-modern-mode))

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

(use-package vterm
  :when my-vterm-shell
  :ensure
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-shell my-vterm-shell)
  (vterm-max-scrollback 50000)
  (vterm-clear-scrollback-when-clearing t)
  :config
  (add-hook 'vterm-mode-hook 'my-vterm-unbind-keys)
  :bind
  (("C-x v" . vterm)
   ("C-x 4 v" . vterm-other-window)))

(use-package fish-mode
  :ensure)
