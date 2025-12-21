;;; -*- lexical-binding: t; -*-

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
  :bind
  (:map global-map
        ("C-c j x" . scratch-buffer)))

(use-package my-emacs
  :bind
  (:map global-map
        ("C-c j h" . my-jump-home)
        ("<f8>" . my-pop-mark)))

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
  :bind
  (:map global-map
        ("C-c g" . magit-file-dispatch)))

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

;;; Crux
;; https://github.com/bbatsov/crux

(use-package crux
  :ensure
  :bind
  (:map global-map
        ("C-o" . crux-smart-open-line)
        ("C-S-o" . crux-smart-open-line-above)
        ("C-^" . crux-top-join-line)
        ("C-M-; D" . crux-duplicate-and-comment-current-line-or-region)))

;;; Whole Line or Region
;; https://github.com/purcell/whole-line-or-region

(use-package whole-line-or-region
  :ensure
  :demand
  :config
  (whole-line-or-region-global-mode +1)
  :bind
  ("M-/" . whole-line-or-region-comment-dwim))

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
  (global-corfu-mode +1)
  (corfu-history-mode +1)
  :bind
  (:map corfu-map
        ("C-SPC" . corfu-insert-separator)
        ("C-;" . corfu-quick-select)))
