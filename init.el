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
