(defun my-linux-p ()
  (eq system-type 'gnu/linux))

(defun my-macos-p ()
  (eq system-type 'darwin))

(defun my-windows-p ()
  (eq system-type 'windows-nt))

(setq my-font (cond ((my-linux-p) "Liberation Mono")
                    ((my-macos-p) "Menlo")
                    ((my-windows-p) "Cascadia Code")))
(setq my-font-height 125)

(let ((path-to-local-config (expand-file-name "local.el" user-emacs-directory)))
  (if (file-exists-p path-to-local-config)
      (progn
        (load path-to-local-config)
        (message "Loaded local config."))
    (message "No local config.")))

(setq custom-file-path (expand-file-name "custom.el" user-emacs-directory))
(setq custom-file custom-file-path)

(when (file-exists-p custom-file-path)
  (load custom-file-path))

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

(use-package exec-path-from-shell
  :ensure
  :when (my-macos-p)
  :config
  (exec-path-from-shell-initialize))

(use-package emacs
  :custom
  ;; UI and startup
  (inhibit-startup-message t)
  (initial-scratch-message "✅ All systems go! 🚀🪐✨")
  (initial-major-mode 'text-mode)
  (visible-bell t)

  ;; Editing behavior
  (indent-tabs-mode nil)
  (tab-width 4)
  (require-final-newline t)
  (save-interprogram-paste-before-kill t)

  ;; File handling
  (create-lockfiles nil)
  (make-backup-files nil)
  (global-auto-revert-non-file-buffers t)

  ;; Security and permissions
  (epg-pinentry-mode 'loopback)
  (disabled-command-function nil)

  :config
  ;; UI cleanup
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)

  ;; Font configuration
  (set-face-attribute 'default nil :font my-font :height my-font-height)
  (set-frame-font my-font nil t)

  ;; Better emoji support on Windows
  (when (my-windows-p)
    (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'append))

  ;; Essential modes
  (global-auto-revert-mode)
  (savehist-mode)
  (global-subword-mode)
  (tab-bar-mode)
  (tab-bar-history-mode)
  (repeat-mode)

  ;; Simplify prompts
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; Only in programming buffers to avoid clutter in text modes
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  :bind
  (;; Navigation
   ("C-c j x" . scratch-buffer)
   ("C-c j h" . my-jump-home)
   ("<f8>" . my-pop-mark)
   ("C-M-<return>" . tab-switch)

   ;; Text navigation
   ("M-g w" . forward-to-word)
   ("M-g W" . backward-to-word)

   ;; Editing
   ("C-M-; d" . duplicate-dwim)
   ("C-c d h" . erase-buffer)
   ("M-z" . zap-up-to-char)
   ("M-Z" . zap-to-char)

   ;; Mouse
   ([down-mouse-2] . mouse-set-point)
   ([mouse-2] . delete-window)))

(defun my-pop-mark ()
  "Jump back to previous mark position"
  (interactive)
  (set-mark-command '(4)))

(defun my-jump-home ()
  "Open home directory in dired"
  (interactive)
  (find-file "~/"))

(defun my-add-super-save-advice (command)
  "Add advice to COMMAND to auto-save before execution."
  (advice-add command :before
              (lambda (&rest _)
                (when (bound-and-true-p super-save-mode)
                  (super-save-command)))))

(use-package dired
  :custom
  (dired-dwim-target t)
  (insert-directory-program (cond ((my-windows-p) insert-directory-program)
                                  ((my-macos-p) "gls")
                                  (t "ls")))
  (dired-listing-switches (cond ((my-windows-p) dired-listing-switches)
                                (t "-alh --group-directories-first"))))

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
  (modus-themes-variable-pitch-ui t)
  :config
  (modus-themes-load-theme 'modus-vivendi))

(use-package vertico
  :ensure
  :demand
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  ;; Configure multiform for specific commands
  (add-to-list 'vertico-multiform-commands
               '(consult-ripgrep buffer indexed))
  :bind
  (:map vertico-map
        ("C-;" . vertico-quick-insert)))

(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

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

(use-package consult
  :ensure
  :config
  ;; Don't live preview buffers to avoid triggering package loading (esp. Org)
  (consult-customize consult-buffer :preview-key "M-.")
  :bind
  (;; Buffer and project navigation
   ("C-x b" . consult-buffer)
   ("C-x B" . consult-buffer-other-tab)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)

   ;; Navigation
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)

   ;; Search
   ("M-s l" . consult-line)
   ("M-s k" . consult-keep-lines)
   ("M-s f" . consult-focus-lines)
   ("M-s r" . consult-ripgrep)

   :map prog-mode-map
   ("C-c ! !" . consult-flymake)))

(defun my-avy-embark-act (target-point)
  "Jump to avy target and run embark-act."
  (goto-char target-point)
  (embark-act))

(defun my-avy-embark-dwim (target-point)
  "Jump to avy target and run embark-dwim."
  (goto-char target-point)
  (embark-dwim))

(use-package avy
  :ensure
  :custom
  (avy-single-candidate-jump t)
  :config
  ;; Embark integration - use . for embark-act, ; for embark-dwim
  (setf (alist-get ?. avy-dispatch-alist) 'my-avy-embark-act)
  (setf (alist-get ?\; avy-dispatch-alist) 'my-avy-embark-dwim)
  :bind
  (;; Core avy commands
   ("C-;" . avy-goto-char-timer)
   ("M-;" . avy-pop-mark)
   ("M-g g" . avy-goto-line)
   ("M-g G" . avy-goto-end-of-line)
   ("M-g h" . avy-org-goto-heading-timer)
   ("M-g s" . avy-goto-word-1)

   ;; Avy actions
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

(use-package embark
  :ensure
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

(use-package embark-consult
  :ensure
  :after (embark consult))

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

(use-package expand-region
  :ensure
  :bind
  ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure
  :bind
  (("C-+" . mc/mark-next-like-this)
   ("C-c k l" . mc/edit-lines)
   ("C-c k m" . mc/mark-all-dwim)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-<return>" . set-rectangular-region-anchor)))

(use-package iy-go-to-char
  :ensure (:host github :repo "arttsu/iy-go-to-char")
  :bind
  (("M-g f" . iy-go-to-char)
   ("M-g F" . iy-go-to-char-backward)
   ("M-g t" . iy-go-up-to-char)
   ("M-g T" . iy-go-up-to-char-backward)
   ("M-g ;" . iy-go-to-or-up-to-continue)
   ("M-g ," . iy-go-to-or-up-to-continue-backward)))

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

(use-package ace-window
  :ensure
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (aw-scope 'frame)
  :config
  ;; Manual integration with super-save since triggers don't work reliably
  (my-add-super-save-advice 'ace-window)
  :bind
  ("M-o" . ace-window))

(use-package whole-line-or-region
  :ensure
  :demand
  :config
  (whole-line-or-region-global-mode)
  :bind
  ("M-/" . whole-line-or-region-comment-dwim))

(defun tempel-include (elt)
  "Include template from another template."
  (when (eq (car-safe elt) 'i)
    (if-let (template (alist-get (cadr elt) (tempel--templates)))
        (cons 'l template)
      (message "Template %s not found" (cadr elt))
      nil)))

(use-package tempel
  :ensure
  :demand
  :custom
  (tempel-trigger-prefix "<")
  :init
  (defun my-tempel-setup-capf()
    (setq-local completion-at-point-functions
                (cons 'tempel-complete completion-at-point-functions)))
  (add-hook 'text-mode-hook 'my-tempel-setup-capf)
  (add-hook 'conf-mode-hook 'my-tempel-setup-capf)
  (add-hook 'prog-mode-hook 'my-tempel-setup-capf)
  :config
  (tempel-key "C-c t f" fun emacs-lisp-mode-map)
  (tempel-key "C-c t t" today)
  (tempel-key "C-c t T" now)
  (add-to-list 'tempel-user-elements #'tempel-include)
  :bind
  ("M-+" . tempel-insert))

(use-package transient
  :ensure)

(use-package magit
  :ensure
  :bind
  ("C-c g" . magit-file-dispatch))
