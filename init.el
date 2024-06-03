(load (expand-file-name "local.el" user-emacs-directory))

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

(use-package emacs
  :custom
  (inhibit-splash-screen t)
  (initial-major-mode 'text-mode)
  (initial-scratch-message "✅ All systems go! 🪐🚀✨")
  (disabled-command-function nil)
  (visible-bell t)
  (save-interprogram-paste-before-kill t)
  (global-auto-revert-non-file-buffers t)
  (create-lockfiles nil)
  (make-backup-files nil)
  (require-final-newline t)
  (indent-tabs-mode nil)
  :config
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (global-auto-revert-mode)
  (global-subword-mode)
  (tab-bar-history-mode)
  :bind
  (("C-c j s" . scratch-buffer)
   ("C-x C-b" . ibuffer-other-window)
   ("M-g w" . forward-to-word)
   ("M-g W" . backward-to-word)
   ("C-M-; d" . duplicate-dwim)
   ("C-c d h" . erase-buffer)
   ("M-z" . zap-up-to-char)
   ("M-Z" . zap-to-char)))

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
  :config
  (vertico-mode))

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
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)))

(use-package org-modern
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  :config
  (global-org-modern-mode))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package toc-org
  :hook (org-mode . toc-org-mode))
