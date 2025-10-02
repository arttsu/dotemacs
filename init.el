;;; Elpaca

;; https://github.com/progfolio/elpaca

;;;; Bootstrap Elpaca

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

;;;; Enable Elpaca's use-package support

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;; Platform helpers

(defun my-linux-p ()
  (eq system-type 'gnu/linux))

(defun my-macos-p ()
  (eq system-type 'darwin))

(defun my-windows-p ()
  (eq system-type 'windows-nt))

;;; Local init.el

;;;; Set default values for local variables

(setq my-font (cond ((my-linux-p) "Liberation Mono")
                    ((my-macos-p) "Menlo")
                    ((my-windows-p) "Cascadia Code")))
(setq my-font-height 150)

(setq my-modus-themes-startup-theme 'modus-operandi)

;;;; Load local init

(let ((local-init (expand-file-name "local-init.el" user-emacs-directory)))
  (if (file-exists-p local-init)
      (progn
        (load local-init)
        (message "Loaded local init."))
    (message "No local init.")))

;;; Custom file

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; TODO: MacOS exec path from shell setup

;;; Core setup

;;;; Core helpers

(defun my-jump-home ()
  (interactive)
  (find-file "~/"))

(defun my-pop-mark ()
  (interactive)
  (set-mark-command '(4)))

;;;; use-package Emacs

(use-package emacs
  :custom
  (create-lockfiles nil)
  (make-backup-files nil)
  (global-auto-revert-non-file-buffers t)
  (inhibit-startup-message t)
  (initial-major-mode 'text-mode)
  (initial-scratch-message "✅ All systems go! 🚀🪐✨")
  (visible-bell t)
  (when (and my-font my-font-height)
    (set-face-attribute 'default nil :font font :height 130)
    (set-frame-font font nil t))
  (when (my-windows-p)
    (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'append))
  (indent-tabs-mode nil)
  (tab-width 4)
  (require-final-newline t)
  (save-interprogram-paste-before-kill t)
  (epg-pinentry-mode 'loopback)
  (diabled-command-function nil)
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  :config
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (global-auto-revert-mode)
  (global-subword-mode)
  (tab-bar-mode)
  (tab-bar-history-mode)
  (repeat-mode)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  :bind
  (("C-c j x" . scratch-buffer)
   ("C-c j h" . my-jump-home)
   ("<f8>" . my-pop-mark)
   ("C-M-<return>" . tab-switch)
   ("M-g w" . forward-to-word)
   ("M-g W" . backward-to-word)
   ("C-M-; d" . duplicate-dwim)
   ("C-c d h" . erase-buffer)
   ("M-z" . zap-up-to-char)
   ("M-Z" . zap-to-char)))

;;; Dired

(use-package dired
  :custom
  (dired-dwim-target t)
  (insert-directory-program (cond ((my-windows-p) insert-directory-program)
                                  ((my-macos-p) "gls")
                                  (t "ls")))
  (dired-listing-switches (cond ((my-windows-p) dired-listing-switches)
                                (t "-alh --group-directories-first")))
  :bind
  (("<f7>" . dired-jump)
   :map dired-mode-map
   ("o" . crux-open-with)
   ("<tab>" . dired-find-file-other-window)))

;;; Project

(use-package project
  :config
  (add-to-list 'project-switch-commands '(project-dired "Dired" "<return>") t))

;;; Imenu

(defun my-elisp-imenu-index ()
  (let* ((my-generic '(("Sections" "^;;; \\(.*\\)$" 1)
                       ("Subsections" "^;;;; \\(.*\\)$" 1)
                       ("Use-packages" "^\\s-*(use-package!?\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)))
         (generic (save-excursion (imenu--generic-function my-generic)))
         (default (save-excursion (imenu-default-create-index-function))))
    (append generic default)))

(defun my-elisp-imenu-setup ()
  (setq-local imenu-create-index-function 'my-elisp-imenu-index))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-imenu-setup)

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
  (modus-themes-variable-pitch-ui t)
  :config
  (modus-themes-load-theme my-modus-themes-startup-theme))

;;; Savehist

(use-package savehist
  :config
  (savehist-mode))

;;; Vertico

;; https://github.com/minad/vertico

(use-package vertico
  :ensure
  :demand
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))
  (add-to-list 'vertico-multiform-categories
               '(consult-ripgrep buffer indexed))
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
  (global-corfu-mode)
  (corfu-history-mode)
  :bind
  (:map corfu-map
        ("C-SPC" . corfu-insert-separator)
        ("C-;" . corfu-quick-insert)))

;;; Consult

;; https://github.com/minad/consult

;; TODO: Use 'consult-flymake'?
(use-package consult
  :ensure
  :config
  ;; Don't live preview buffers to avoid triggering package loading (esp. Org)
  (consult-customize consult-buffer :preview-key "M-.")
  :bind
  (("C-x b" . consult-buffer)
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
   ("M-s f" . consult-focus-lines)
   ("M-s r" . consult-ripgrep)))

;;; Avy

;; https://github.com/abo-abo/avy

(use-package avy
  :ensure
  :custom
  (avy-single-candidate-jump t)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'my-avy-embark-act)
  (setf (alist-get ?\; avy-dispatch-alist) 'my-avy-embark-dwim)
  :bind
  (("C-;" . avy-goto-char-timer)
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
   ("C-M-; S" . avy-kill-ring-save-region)
   :map isearch-mode-map
   ("C-;" . avy-isearch)))

;;;; Avy Embark actions

(defun my-avy-embark-act (target-point)
  (goto-char target-point)
  (embark-act))

(defun my-avy-embark-dwim (target-point)
  (goto-char target-point)
  (embark-dwim))

;;; Embark

;; https://github.com/oantolin/embark

(use-package embark
  :ensure
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

;;;; Embark-Consult

(use-package embark-consult
  :ensure
  :after (embark consult))

;;; Link Hint

;; https://github.com/noctuid/link-hint.el

(use-package link-hint
  :ensure
  :bind
  (("C-c f" . link-hint-open-link)
   ("C-c y" . link-hint-copy-link)))

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

;;; crux

;; https://github.com/bbatsov/crux

(use-package crux
  :ensure
  :bind
  (("C-o" . crux-smart-open-line)
   ("C-S-o" . crux-smart-open-line-above)
   ("C-^" . crux-top-join-line)
   ("C-M-; D" . crux-duplicate-and-comment-current-line-or-region)))

;;; Expand Region

;; https://github.com/magnars/expand-region.el

(use-package expand-region
  :ensure
  :bind
  (("C-=" . er/expand-region)))


;;; Multiple Cursors

;; https://github.com/magnars/multiple-cursors.el

(use-package multiple-cursors
  :ensure
  :bind
  (("C-c k l" . mc/edit-lines)
   ("C-+" . mc/mark-next-like-this)
   ("C-c k m" . mc/mark-all-dwim)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-<return>" . set-rectangular-region-anchor)))

;;; Go to Char

(use-package iy-go-to-char
  :ensure (:host github :repo "arttsu/iy-go-to-char")
  :bind
  (("M-g f" . iy-go-to-char)
   ("M-g F" . iy-go-to-char-backward)
   ("M-g t" . iy-go-up-to-char)
   ("M-g T" . iy-go-up-to-char-backward)
   ("M-g ;" . iy-go-to-or-up-to-continue)
   ("M-g ," . iy-go-to-or-up-to-continue-backward)))

;;; page-break-lines

;; https://github.com/purcell/page-break-lines

(use-package page-break-lines
  :ensure
  :config
  (global-page-break-lines-mode))

