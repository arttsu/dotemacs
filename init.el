(load (expand-file-name "local.el" user-emacs-directory))

(defun my-windows-p ()
  (eq system-type 'windows-nt))

(defun my-mac-p ()
  (eq system-type 'darwin))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(when (my-windows-p)
  (setenv "PATH" (concat my-git-path ";" (getenv "PATH")))
  (push my-git-path exec-path))

(setq package-enable-at-startup nil)

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
  :when (my-mac-p)
  :config
  (exec-path-from-shell-initialize))

(use-package emacs
  :custom
  (inhibit-splash-screen t)
  (initial-major-mode 'text-mode)
  (initial-scratch-message "✅ All systems go! 🚀🪐✨")
  (disabled-command-function nil)
  (visible-bell t)
  (save-interprogram-paste-before-kill t)
  (global-auto-revert-non-file-buffers t)
  (create-lockfiles nil)
  (make-backup-files nil)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (require-final-newline t)
  (indent-tabs-mode nil)
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (backward-delete-char-untabify-method 'hungry)
  (split-width-threshold 200)
  (split-height-threshold 60)
  (epa-file-encrypt-to (list my-gpg-key-id))
  :config
  (set-frame-name "Main")
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (global-auto-revert-mode)
  (global-subword-mode)
  (tab-bar-history-mode)
  (tab-bar-mode)
  (repeat-mode)
  :bind
  (("C-c j x" . scratch-buffer)
   ("C-x C-b" . ibuffer-other-window)
   ("M-g w" . forward-to-word)
   ("M-g W" . backward-to-word)
   ("C-M-; d" . duplicate-dwim)
   ("C-c d h" . erase-buffer)
   ("M-z" . zap-up-to-char)
   ("M-Z" . zap-to-char)
   ("C-x K" . kill-this-buffer)
   ("s-t" . tab-switch)
   ([down-mouse-2] . mouse-set-point)
   ([mouse-2] . delete-window)
   :map prog-mode-map
   ("DEL" . backward-delete-char-untabify)
   ("M-g N" . flymake-goto-next-error)
   ("M-g P" . flymake-goto-prev-error)))

(use-package my-emacs
  :straight nil
  :demand
  :after emacs
  :bind
  (("C-c d w" . my/kill-forward-to-word)
   ("C-c d W" . my/kill-backward-to-word)
   ("C-c d <" . my/kill-to-beginning-of-buffer)
   ("C-c d >" . my/kill-to-end-of-buffer)
   ("C-c j h" . my/jump-home)))

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

(if my-use-iosevka-comfy
    (let ((font "Iosevka Comfy Fixed"))
      (set-face-attribute 'default nil :font font :height my-iosevka-comfy-height)
      (set-frame-font font nil t)))

(use-package dired
  :straight nil
  :custom
  (dired-dwim-target t)
  (insert-directory-program (cond ((my-windows-p) insert-directory-program)
                                  ((my-mac-p) "gls")
                                  (t "ls")))
  (dired-listing-switches (cond ((my-windows-p) dired-listing-switches)
                                (t "-alh --group-directories-first")))
  :bind
  (("<f7>" . dired-jump)
   :map dired-mode-map
   ("o" . crux-open-with)
   ("<tab>" . dired-find-file-other-window)))

(use-package project
  :config
  (add-to-list 'project-switch-commands '(project-dired "Dired" "<return>") t))

(use-package my-project
  :straight nil
  :after project
  :demand
  :bind
  (("C-x p P" . my-project-open-new-frame)
   ("C-x p v" . my-project-vterm)))

(use-package super-save
  :custom
  (super-save-all-buffers t)
  (super-save-auto-save-when-idle t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-silent t)
  (super-save-exclude '(".sbt" "project/"))
  (auto-save-default nil)
  :config
  (super-save-mode))

(use-package emms
  :unless (my-windows-p)
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-player-mpv-update-metadata t)
  (emms-streams-file (expand-file-name "streams.emms" user-emacs-directory))
  :config
  (emms-all)
  :bind
  (("C-c r r" . emms-streams)
   ("C-c r p" . emms-pause)
   ("C-c r s" . emms-stop)))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (aw-scope 'frame)
  :bind
  (("M-o" . ace-window)))

(use-package vertico
  :demand
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))
  (add-to-list 'vertico-multiform-commands
               '(consult-ripgrep buffer indexed))
  :bind
  (:map vertico-map
        ("C-;" . vertico-quick-insert)))

(use-package savehist
  :config
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :config
  ;; Don't live preview buffers to avoid triggering package loading.
  ;; See https://github.com/minad/consult#live-previews
  (consult-customize consult-buffer :preview-key "M-.")
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

(use-package my-consult
  :straight nil
  :after (consult org))

(use-package company
  :init
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  :config
  (global-company-mode))

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
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
        ("C-c p (" . sp-wrap-round)
        ("C-c p [" . sp-wrap-square)
        ("C-c p {" . sp-wrap-curly)
        ("C-c p r" . sp-rewrap-sexp)))

(use-package yasnippet
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-global-mode))

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

(use-package my-avy-embark
  :straight nil
  :after avy
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'my-avy-embark)
  (setf (alist-get ?\; avy-dispatch-alist) 'my-avy-embark-dwim))

(use-package link-hint
  :bind
  (("C-c f" . link-hint-open-link)
   ("C-c y" . link-hint-copy-link)))

(use-package crux
  :bind
  (("C-o" . crux-smart-open-line)
   ("C-S-o" . crux-smart-open-line-above)
   ("C-^" . crux-top-join-line)
   ("C-M-; D" . crux-duplicate-and-comment-current-line-or-region)))

(use-package whole-line-or-region
  :demand
  :config
  (whole-line-or-region-global-mode)
  :bind
  (("M-/" . whole-line-or-region-comment-dwim)))

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

(use-package magit
  :bind
  (("C-c g" . magit-file-dispatch)))

(use-package org
  :custom
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
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   :map org-mode-map
   ("C-c P i" . org-id-get-create)))

(use-package my-org
  :straight nil
  :demand t
  :after org
  :bind
  (:map org-mode-map
        ("M-g e" . my-org-end-of-subtree)))

(use-package org-modern
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  :config
  (global-org-modern-mode))

(use-package my-org-looks
  :straight nil
  :after org)

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package my-gtd
  :straight nil
  :demand
  :after org
  :init
  (setq my-gtd-dir "~/org/gtd")
  (setq my-gtd-shared-dir "~/org-shared/gtd")
  :bind
  (("C-c i" . my-gtd-capture-to-inbox)
   ("C-c I" . my-gtd-capture-note-to-inbox)
   ("C-c j p" . my-gtd-jump-to-project)
   ("C-c j s" . my-gtd-jump-to-someday)))

(use-package ob-restclient
  :after org)

(use-package ox-slack
  :after org)

(use-package gptel
  :custom
  (gptel-model "gpt-4o")
  (gptel-default-mode 'org-mode)
  :config
  (add-hook 'gptel-mode-hook 'toggle-truncate-lines)
  (require 'my-gptel)
  :bind
  (("C-c SPC" . gptel)
   :map gptel-mode-map
   ("C-c k" . gptel-abort)))

(use-package my-gptel
  :straight nil
  :mode ("\\.gpt\\'" . my-gptel-mode)
  :bind
  (:map gptel-mode-map
        ("C-c d h" . my-gptel-clear-buffer)
        ("C-c C-c" . my-gptel-send)))

(use-package ledger-mode
  :unless (my-windows-p)
  :custom
  (ledger-default-date-format "%Y-%m-%d"))

(use-package my-ledger
  :straight nil
  :after ledger-mode)

(use-package vterm
  :unless (my-windows-p)
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-shell my-fish-path)
  (vterm-max-scrollback 50000)
  (vterm-clear-scrollback-when-clearing t)
  :bind
  (("C-x v" . vterm)
   ("C-x 4 v" . vterm-other-window)))

(use-package shell
  :custom
  (shell-kill-buffer-on-exit t))

(use-package copilot
  :unless (my-windows-p)
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :custom
  (copilot-idle-delay 0.3)
  :config
  (add-to-list 'warning-suppress-log-types '(copilot copilot-no-mode-indent))
  :hook (prog-mode . copilot-mode))

(use-package my-copilot
  :straight nil
  :after copilot)

(use-package jinx
  :unless (my-windows-p)
  :hook (emacs-startup . global-jinx-mode)
  :bind
  (("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages)))

(use-package rg
  :unless (my-windows-p)
  :bind
  (("M-s R" . rg-project)))

(use-package tree-sitter
  :unless (my-windows-p)
  :custom
  (treesit-font-lock-level 4))

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "<f5>")
  :hook
  (scala-ts-mode . lsp)
  :commands lsp
  :bind
  (:map lsp-mode-map
        ([M-down-mouse-1] . mouse-set-point)
        ([M-mouse-1] . lsp-find-definition)
        ([M-mouse-3] . xref-go-back)
        ("<f5> I" . lsp-metals-build-import)))

(use-package lsp-metals)

(use-package lsp-ui :commands lsp-ui-mode)

(setq lsp-completion-provider :none)

(defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))

(add-hook 'lsp-mode-hook #'corfu-lsp-setup)

(use-package consult-lsp
  :bind
  (:map lsp-mode-map
        ("<f5> d" . consult-lsp-diagnostics)
        ("<f5> s" . consult-lsp-file-symbols)
        ("<f5> S" . consult-lsp-symbols)))

(use-package flycheck
  :init
  (setq flycheck-global-modes '(not org-mode))
  :config
  (global-flycheck-mode))

(use-package scala-ts-mode
  :unless (my-windows-p)
  :interpreter "scala")

(use-package jarchive
  :unless (my-windows-p)
  :after eglot
  :config
  (jarchive-setup))

(use-package kubel
  :unless (my-windows-p)
  :bind
  (("C-c K" . kubel)
   :map kubel-mode-map
   ("n" . next-line)
   ("p" . previous-line)
   ("N" . kubel-set-namespace)
   ("v" . kubel-exec-shell-pod)
   ("D" . kubel-exec-pod)))

(use-package markdown-mode
  :interpreter "markdown")

(define-derived-mode anki-mode org-mode "Anki")

(add-to-list 'auto-mode-alist '("\\.anki\\'" . anki-mode))

(use-package anki-editor
  :unless (my-windows-p)
  :hook (anki-mode . anki-editor-mode)
  :bind
  (:map anki-mode-map
        ("C-<return>" . anki-editor-insert-note)
        ("C-c p p" . anki-editor-push-notes)
        ("C-c p r" . anki-editor-retry-failure-notes)))

(use-package plantuml-mode)

(use-package clojure-mode)

(use-package cider)

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)

(use-package my-openai-tools
  :straight nil)

(use-package terraform-mode)

(use-package graphql-mode)

(use-package decide)

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

(use-package embark-consult
  :after (embark consult))

(use-package org-roam
  :custom
  (org-roam-directory "~/roam")
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-gc-threshold most-positive-fixnum)
  :config
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-dailies)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n d t" . org-roam-dailies-capture-today)
   ("C-c n d y" . org-roam-dailies-capture-yesterday)
   ("C-c n d T" . org-roam-dailies-goto-today)
   ("C-c n d Y" . org-roam-dailies-goto-yesterday)))

(use-package org-roam-ui)
