;; -*- lexical-binding: t; -*-

;;; System Type Helpers

(defun my-macos-p ()
  "Return t if the host system is macOS."
  (eq system-type 'darwin))

(defun my-linux-p ()
  "Return t if the host system is GNU/Linux."
  (eq system-type 'gnu/linux))

(defun my-windows-p ()
  "Return t if the host system is Windows."
  (eq system-type 'windows-nt))

;;; Emacs

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
  :config
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (global-auto-revert-mode +1)
  (global-subword-mode +1)
  (tab-bar-mode +1)
  (tab-bar-history-mode +1)
  (repeat-mode +1)
  (fset 'yes-or-no-p 'y-or-n-p)
  :hook
  (prog-mode . display-line-numbers-mode)
  (emacs-lisp-mode . flymake-mode)
  :bind (:map global-map
              ("C-c j x" . scratch-buffer)))

(use-package my-emacs
  :bind (:map global-map
              ("C-c j h" . my-jump-home)
              ("<f8>" . my-pop-mark)))

;;; Dired

(use-package dired
  :demand
  :custom
  (dired-dwim-target t)
  (insert-directory-program (cond ((my-windows-p) insert-directory-program)
                                  ((my-macos-p) "gls") ; TODO: Use executable-find and 'ls' as fallback.
                                  (t "ls")))
  (dired-listing-switches (cond ((my-windows-p) dired-listing-switches)
                                (t "-alh --group-directories-first")))
  :bind (:map global-map
              ("<f7>" . dired-jump))
  :bind (:map dired-mode-map
              ("<tab>" . dired-find-file-other-window)))

;;; Project

(use-package project
  :config
  (add-to-list 'project-switch-commands '(project-dired "Dired" "<return>") t))

;;; Imenu

(use-package imenu
  :preface
  (defun my-imenu-elisp-index ()
    "Return an Imenu index for Emacs Lisp buffers."
    (let* ((section-regex (rx line-start (>= 3 ";") (+ blank) (group (* not-newline)) line-end))
           (section-rules `(("Sections" ,section-regex 1))))
      (append (imenu--generic-function section-rules)
              (imenu-default-create-index-function))))
  (defun my-imenu-setup-elisp ()
    "Use custom Imenu index in Emacs Lisp Mode."
    (setq-local imenu-create-index-function #'my-imenu-elisp-index))
  :hook
  (emacs-lisp-mode . my-imenu-setup-elisp))

;;; Savehist

(use-package savehist
  :config
  (savehist-mode))

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
  (modus-themes-variable-pitch-ui t))

;;; Transient
;; https://github.com/magit/transient

;; Transient is a dependency of Magit. I install it explicitly as I
;; ran into version incompatibility when I was installing just Magit.

(use-package transient
  :ensure)

;;; Magit
;; https://magit.vc

(use-package magit
  :ensure
  :bind (:map global-map
              ("C-c g" . magit-file-dispatch)))

;;; Super Save
;; https://github.com/bbatsov/super-save

(use-package super-save
  :ensure
  :custom
  (super-save-all-buffers t)
  (super-save-auto-save-when-idle t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-silent t)
  (super-save-exclude '(".gpg" ".sbt" "project/"))
  (auto-save-default nil)
  :config
  ;; TODO: Add Ace Window to the list of triggers.
  (super-save-mode))

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
  :bind (:map smartparens-mode-map
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

;;; Crux
;; https://github.com/bbatsov/crux

(use-package crux
  :ensure
  :bind (:map global-map
              ("C-o" . crux-smart-open-line)
              ("C-S-o" . crux-smart-open-line-above)
              ("C-^" . crux-top-join-line)
              ("C-M-; D" . crux-duplicate-and-comment-current-line-or-region))
  :bind (:map dired-mode-map
              ("o" . crux-open-with)))

;;; Whole Line or Region
;; https://github.com/purcell/whole-line-or-region

(use-package whole-line-or-region
  :ensure
  :demand
  :config
  (whole-line-or-region-global-mode +1)
  :bind (:map global-map
              ("M-/" . whole-line-or-region-comment-dwim)))

;;; Rainbow Delimiters
;; https://github.com/Fanael/rainbow-delimiters

(use-package rainbow-delimiters
  :ensure
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;; Vertico
;; https://github.com/minad/vertico

;; TODO: Add multiform categories for Jinx.
;; TODO: Add multiform categories for Consult Ripgrep.
(use-package vertico
  :ensure
  :demand
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode +1)
  (vertico-multiform-mode +1)
  :bind (:map vertico-map
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
  (global-corfu-mode +1)
  (corfu-history-mode +1)
  :bind (:map corfu-map
              ("C-SPC" . corfu-insert-separator)
              ("C-;" . corfu-quick-select)))

;;; Consult
;; https://github.com/minad/consult

;; TODO: Bind consult-ripgrep.
(use-package consult
  :ensure
  :config
  ;; Don't live-preview buffers to avoid triggering package loading
  ;; (especially Org).
  (consult-customize consult-buffer :preview-key "M-.")
  :bind (:map global-map
              ("C-x b" . consult-buffer)
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
              ("M-s f" . consult-focus-lines)))

;;; Avy
;; https://github.com/abo-abo/avy

(use-package avy
  :ensure
  :custom
  (avy-single-candidate-jump t)
  :bind (:map global-map
              ("C-;" . avy-goto-char-timer)
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
              ("C-M-; S" . avy-kill-ring-save-region))
  :bind (:map isearch-mode-map
              ("C-;" . avy-isearch)))

;;; Org

(use-package org
  :ensure
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-use-sub-superscripts '{})
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-use-speed-commands t)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-log-done 'time)
  (org-priority-lowest 69)
  (org-priority-default 68)
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-confirm-babel-evaluate nil)
  :config
  (require 'org-id)
  (require 'org-attach)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t))))
