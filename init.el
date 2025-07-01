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
