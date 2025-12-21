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
