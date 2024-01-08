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
  :config
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (global-auto-revert-mode)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (global-subword-mode)
  :bind
  (("C-c v s" . scratch-buffer)
   ("C-c v b" . bookmark-jump)
   ("C-x C-b" . ibuffer-other-window)))

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

(use-package magit
  :bind
  (("C-c g" . magit-file-dispatch)))

(straight-use-package 'org)

(use-package org
  :defer t
  :custom
  (org-confirm-babel-evaluate nil)
  (org-startup-indented t))

(use-package super-save
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-exclude '(".sbt" "project/"))
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-all-buffers t)
  (auto-save-default nil)
  :config
  (super-save-mode))

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
