(load (expand-file-name "local.el" user-emacs-directory))

(use-package emacs
  :custom
  (create-lockfiles nil)
  (make-backup-files nil)
  (inhibit-splash-screen t)
  (initial-scratch-message "All systems go! 🚀")
  (initial-major-mode 'text-mode))

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

(use-package org
  :defer t
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-window-setup 'split-window-below)
  (org-startup-indented t)
  (org-startup-with-inline-images t))

(use-package magit
  :bind
  (("C-c g" . magit-file-dispatch)))

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
  :demand
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

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (aw-scope 'global)
  :bind
  (("M-o" . ace-window)))

(defun my-avy-embark (point)
  (goto-char point)
  (embark-act))

(defun my-avy-embark-dwim (point)
  (goto-char point)
  (embark-dwim))

(use-package avy
  :custom
  (avy-single-candidate-jump t)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'my-avy-embark)
  (setf (alist-get ?\; avy-dispatch-alist) 'my-avy-embark-dwim)
  :bind
  (("C-;" . avy-goto-char-timer)
   ("M-;" . avy-pop-mark)
   ("M-g g" . avy-goto-line)
   ("M-g G" . avy-goto-end-of-line)
   ("M-g h" . avy-org-goto-heading-timer)
   ("M-g w" . avy-goto-word-1)
   :map isearch-mode-map
   ("C-;" . avy-isearch)))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

(use-package embark-consult
  :after (embark consult))

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

(use-package company
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.3)
  (company-selection-wrap-around t)
  (company-dabbrev-downcase nil)
  (company-show-numbers t)
  :config
  (global-company-mode)
  :bind
  (("M-<tab>" . company-complete)))

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "<f5>")
  :hook
  (scala-mode . lsp)
  :commands lsp
  :bind
  (:map lsp-mode-map
	([M-down-mouse-1] . mouse-set-point)
	([M-mouse-1] . lsp-find-definition)
	([M-mouse-3] . xref-go-back)))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package consult-lsp
  :after (consult lsp-mode)
  :bind
  (:map lsp-mode-map
	("<f5> d" . consult-lsp-diagnostics)
	("<f5> s" . consult-lsp-file-symbols)
	("<f5> S" . consult-lsp-symbols)))

(use-package lsp-metals
  :after (lsp-mode scala))
