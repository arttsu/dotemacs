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
  (custom-file my-custom-file)
  :init
  ;; Make right option behave like AltGr, so that EurKEY layout works
  ;; properly.
  (when (my-macos-p)
    (setq mac-right-option-modifier 'none))
  :config
  (require 'my-emacs)
  (require 'my-ui)
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
  (elpaca-after-init . my-ui-set-theme-and-font)
  (prog-mode . display-line-numbers-mode)
  (emacs-lisp-mode . flymake-mode)
  :bind (:map global-map
              ("C-c j x" . scratch-buffer)
              ("C-c j h" . my-jump-home)
              ("<f8>" . my-pop-mark)))

;;; Dired

(use-package dired
  :custom
  (dired-dwim-target t)
  (insert-directory-program (cond ((my-windows-p) insert-directory-program)
                                  ((my-macos-p) (or (executable-find "gls") "ls"))
                                  (t "ls")))
  (dired-listing-switches (cond ((my-windows-p) dired-listing-switches)
                                (t "-alh --group-directories-first")))
  :bind (:map global-map
              ("<f7>" . dired-jump))
  :bind (:map dired-mode-map
              ("<tab>" . dired-find-file-other-window)
              ("o" . crux-open-with)))

;;; Project

(use-package project
  :config
  (add-to-list 'project-switch-commands '(project-dired "Dired" "<return>") t))

;;; Imenu

(use-package imenu
  :config
  (require 'my-imenu)
  :hook
  (emacs-lisp-mode . my-imenu-setup-elisp))

;;; Savehist

(use-package savehist
  :config
  (savehist-mode))

;;; macOS: Exec Path From Shell
;; https://github.com/purcell/exec-path-from-shell

(when (my-macos-p)
  (use-package exec-path-from-shell
    :ensure
    :config
    (exec-path-from-shell-initialize)))

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
  (add-to-list 'super-save-triggers 'ace-window) ; Must be before enabling 'super-save-mode'!
  (super-save-mode +1))

;;; Easysession
;; https://github.com/jamescherti/easysession.el

(use-package easysession
  :ensure
  :custom
  (easysession-mode-line-misc-info t)
  (easysession-save-interval 60)
  (easysession-buffer-list-function #'my-easysession-visible-buffer-list)
  (easysession-switch-to-exclude-current t)
  :init
  (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103)
  :hook
  (easysession-new-session . my-easysession-do-reset)
  :config
  (require 'my-easysession)
  :bind (:map global-map
              ("<f12> <f12>" . easysession-switch-to)
              ("<f12> s" . easysession-save)
              ("<f12> S" . easysession-save-as)
              ("<f12> k" . easysession-delete)
              ("<f12> r" . easysession-rename)
              ("<f12> x" . my-easysession-reset)))

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
              ("C-M-; D" . crux-duplicate-and-comment-current-line-or-region)))

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
  (org-refile-targets '((org-agenda-files :tag . "refile")))
  (org-tags-exclude-from-inheritance '("refile"))
  (org-attach-id-dir (expand-file-name "local/attachments" my-org-dir))
  :config
  (require 'org-id)
  (require 'org-attach)
  (require 'my-org)
  (my-org-setup-gtd-and-knowledge-management)
  (setq org-capture-templates (my-org-capture-templates))
  (setq org-agenda-files (append (my-org-agenda-files "local") (my-org-agenda-files "shared")))
  (setq org-agenda-custom-commands (my-org-day-agenda-commands my-org-day-agenda-include-shared-by-default))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)))
  :bind (:map global-map
              ("C-c c" . org-capture)
              ("C-c a" . org-agenda)
              ("C-c i" . my-org-capture-note)
              ("C-c I" . my-org-capture-todo))
  :bind (:map org-mode-map
              ("C-c C-S-w" . my-org-refile-note)
              ("C-c M-W" . my-org-refile-copy-note)))

;;;; Org Modern
;; https://github.com/minad/org-modern

(use-package org-modern
  :ensure
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  (org-modern-star 'replace)
  (org-modern-table nil)
  (org-modern-horizontal-rule nil)
  (org-modern-priority-faces '((?A :background "Firebrick3" :foreground "White")
                               (?B :background "Goldenrod" :foreground "White")
                               (?C :background "SeaGreen" :foreground "White")
                               (?D :background "MediumOrchid" :foreground "White")
                               (?E :background "Seashell3" :foreground "Black")))
  (org-modern-tag-faces '(("refile" :inherit default :height 0.75 :slant normal)))
  :custom-face
  (org-modern-tag ((t (:background "AntiqueWhite" :foreground "Black" :slant italic))))
  :config
  (global-org-modern-mode +1))

;;;; Org Node
;; https://github.com/meedstrom/org-node

(use-package org-node
  :ensure
  :custom
  (org-mem-do-sync-with-org-id t)
  (org-mem-watch-dirs (list my-org-dir))
  :config
  (org-mem-updater-mode)
  (org-node-cache-mode)
  (org-node-backlink-mode)
  :bind (:map global-map
              ("M-s M-f" . org-node-find)
              ("M-s M-i" . org-node-insert-link)
              ("M-s M-t" . org-node-add-tags-here)))
