(load (expand-file-name "local.el" user-emacs-directory))

(defun my-windows-p ()
  (eq system-type 'windows-nt))

(defun my-mac-p ()
  (eq system-type 'darwin))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(when (my-windows-p)
  (setenv "PATH" (concat my-git-path ";" (getenv "PATH")))
  (push my-git-path exec-path))

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

(use-package exec-path-from-shell
  :when (my-mac-p)
  :config
  (exec-path-from-shell-initialize))

(use-package emacs
  :custom
  (inhibit-splash-screen t)
  (initial-major-mode 'text-mode)
  (initial-scratch-message "✅ All systems go! 🚀🪐✨")
  (disabled-command-function nil)
  (visible-bell t)
  (save-interprogram-paste-before-kill t)
  (global-auto-revert-non-file-buffers t)
  (create-lockfiles nil)
  (make-backup-files nil)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (require-final-newline t)
  (indent-tabs-mode nil)
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (backward-delete-char-untabify-method 'hungry)
  (split-width-threshold 200)
  (split-height-threshold 60)
  (epa-file-encrypt-to (list my-gpg-key-id))
  (epa-pinentry-mode 'loopback)
  :config
  (set-frame-name "Main")
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (global-auto-revert-mode)
  (global-subword-mode)
  (tab-bar-history-mode)
  (tab-bar-mode)
  (repeat-mode)
  :bind
  (("C-c j x" . scratch-buffer)
   ("C-x C-b" . ibuffer-other-window)
   ("M-g w" . forward-to-word)
   ("M-g W" . backward-to-word)
   ("C-M-; d" . duplicate-dwim)
   ("C-c d h" . erase-buffer)
   ("M-z" . zap-up-to-char)
   ("M-Z" . zap-to-char)
   ("C-x K" . kill-this-buffer)
   ("s-t" . tab-switch)
   ([down-mouse-2] . mouse-set-point)
   ([mouse-2] . delete-window)
   :map prog-mode-map
   ("DEL" . backward-delete-char-untabify)
   ("M-g N" . flymake-goto-next-error)
   ("M-g P" . flymake-goto-prev-error)))

(use-package my-emacs
  :straight nil
  :demand
  :after emacs
  :bind
  (("C-c d w" . my/kill-forward-to-word)
   ("C-c d W" . my/kill-backward-to-word)
   ("C-c d <" . my/kill-to-beginning-of-buffer)
   ("C-c d >" . my/kill-to-end-of-buffer)
   ("C-c j h" . my/jump-home)))

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-headings '((0 . (ultrabold 1.7))
                           (1 . (ultrabold 1.7))
                           (2 . (extrabold 1.5))
                           (3 . (extrabold 1.3))
                           (4 . (extrabold 1.1))
                           (t . (extrabold))))
  (modus-themes-variable-pitch-ui t)
  :config
  (modus-themes-load-theme 'modus-vivendi))

(set-face-attribute 'default nil :font my-font :height my-font-height)
(set-frame-font my-font nil t)

(when (my-windows-p)
  (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'append))

(defun my-dired-rename-to-timestamp ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (dolist (file files)
      (let* ((attributes (file-attributes file))
             (mod-time (file-attribute-modification-time attributes))
             (timestamp (format-time-string "%Y%m%d%H%M%S" mod-time))
             (new-name (concat timestamp (file-name-extension file t)))
             (new-path (concat (file-name-directory file) new-name)))
        (dired-rename-file file new-path nil)))))

(use-package dired
  :straight nil
  :custom
  (dired-dwim-target t)
  (insert-directory-program (cond ((my-windows-p) insert-directory-program)
                                  ((my-mac-p) "gls")
                                  (t "ls")))
  (dired-listing-switches (cond ((my-windows-p) dired-listing-switches)
                                (t "-alh --group-directories-first")))
  :bind
  (("<f7>" . dired-jump)
   :map dired-mode-map
   ("o" . crux-open-with)
   ("<tab>" . dired-find-file-other-window)
   ("C-c D t" . my-dired-rename-to-timestamp)))

(use-package project
  :config
  (add-to-list 'project-switch-commands '(project-dired "Dired" "<return>") t))

(use-package my-project
  :straight nil
  :after project
  :demand
  :bind
  (("C-x p P" . my-project-open-new-frame)
   ("C-x p v" . my-project-vterm)))

(use-package super-save
  :custom
  (super-save-all-buffers t)
  (super-save-auto-save-when-idle t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-silent t)
  (super-save-exclude '(".sbt" "project/" ".gpg"))
  (auto-save-default nil)
  :config
  (super-save-mode))

(use-package emms
  :unless (my-windows-p)
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
  :demand
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))
  (add-to-list 'vertico-multiform-commands
               '(consult-ripgrep buffer indexed))
  :bind
  (:map vertico-map
        ("C-;" . vertico-quick-insert)))

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

(use-package my-consult
  :straight nil
  :after (consult org))

(use-package company
  :init
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  :config
  (global-company-mode))

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

(use-package avy
  :custom
  (avy-single-candidate-jump t)
  :bind
  (("C-;" . avy-goto-char-timer)
   ("M-;" . avy-pop-mark)
   ("M-g g" . avy-goto-line)
   ("M-g G" . avy-goto-end-of-line)
   ("M-g h" . avy-org-goto-heading-timer)
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

(use-package my-avy-embark
  :straight nil
  :after avy
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'my-avy-embark)
  (setf (alist-get ?\; avy-dispatch-alist) 'my-avy-embark-dwim))

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

(use-package magit
  :bind
  (("C-c g" . magit-file-dispatch)))

(defconst my-open-org-dir "~/org-open")

(defconst my-local-gtd-dir (concat my-local-org-dir "/gtd"))
(defconst my-open-gtd-dir (concat my-open-org-dir "/gtd"))

(defun my-gtd-path (name)
  (expand-file-name (concat my-local-gtd-dir name ".org")))

(defconst my-gtd-inbox (my-gtd-path "/inbox"))

(defconst my-local-gtd-projects-dir (expand-file-name "projects" my-local-gtd-dir))
(defconst my-local-gtd-areas-dir (expand-file-name "areas" my-local-gtd-dir))
(defconst my-open-gtd-projects-dir (expand-file-name "projects" my-open-gtd-dir))
(defconst my-open-gtd-areas-dir (expand-file-name "areas" my-open-gtd-dir))

(defconst my-local-gtd-files `(,my-local-gtd-dir ,my-local-gtd-projects-dir ,my-local-gtd-areas-dir))
(defconst my-open-gtd-files `(,my-open-gtd-dir ,my-open-gtd-projects-dir ,my-open-gtd-areas-dir))
(defconst my-all-gtd-files (append my-local-gtd-files my-open-gtd-files))

(defconst my-local-notes (expand-file-name "notes" my-local-org-dir))
(defconst my-open-notes (expand-file-name "notes" my-open-org-dir))

(defun my-org-setup ()
  (setq-local fill-column 120)
  (auto-fill-mode 1))

(defun my-org-files-in-dir (dir)
  (directory-files dir t "\\.org$"))

(defun my-org-sort-todos ()
  "Sort the current subtree: first 'open' items by priority, then 'done' items by priority."
  (interactive)
  (unless (org-at-heading-p)
    (error "Not at a heading"))
  (org-sort-entries nil ?p)
  (org-sort-entries nil ?o)
  (org-cycle)
  (org-cycle))

(defun my-org-end-of-subtree ()
  "Jump to the last item of the current subtree."
  (interactive)
  (org-end-of-subtree)
  (org-back-to-heading))

(defun my-org-add-update ()
  "Add an update entry with a timestamp to the current Org heading."
  (interactive)
  (org-end-of-subtree)
  (end-of-line)
  (newline-and-indent)
  (insert (format "# Upd. %s" (format-time-string "<%Y-%m-%d %H:%M>")))
  (newline-and-indent))

(defun my-org-insert-timestamped-heading ()
  (interactive)
  (org-insert-heading-respect-content)
  (forward-line)
  (insert "# CREATED: ")
  (org-insert-timestamp (current-time) t t)
  (insert "\n")
  (forward-line -2)
  (end-of-line))

(defun my-org-duplicate-subtree ()
  (interactive)
  (when (org-at-heading-p)
    (beginning-of-line)
    (org-copy-subtree)
    (org-paste-subtree)
    (org-delete-property "ID")))

(defun my-org-refile-note ()
  (interactive)
  (let ((original-targets org-refile-targets))
    (unwind-protect
        (progn
          (setq org-refile-targets `((,(append (my-org-files-in-dir my-local-notes) (my-org-files-in-dir my-open-notes)) :level 2)))
          (org-refile))
      (setq org-refile-targets original-targets))))

(defun my-org-capture-template-path (name)
  (expand-file-name (concat "capture-templates/" name ".txt") user-emacs-directory))

(defconst my-gtd-inbox-target `(file+headline ,my-gtd-inbox "Inbox items"))

(defconst my-gtd-capture-templates
  `(("i" "Inbox")
    ("ii" "note" entry ,my-gtd-inbox-target (file ,(my-org-capture-template-path "gtd-note")))
    ("iI" "todo" entry ,my-gtd-inbox-target (file ,(my-org-capture-template-path "gtd-todo")))
    ("il" "note link" entry ,my-gtd-inbox-target (file ,(my-org-capture-template-path "gtd-note-link")))
    ("iL" "todo link" entry ,my-gtd-inbox-target (file ,(my-org-capture-template-path "gtd-todo-link")))))

(defun my-gtd-capture-note (&optional prefix)
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "ii"))
        ((equal prefix '(4)) (org-capture nil "il"))
        (t (error "Invalid prefix argument: %s" prefix))))

(defun my-gtd-capture-todo (&optional prefix)
  (interactive "P")
  (cond ((equal prefix nil) (org-capture nil "iI"))
        ((equal prefix '(4)) (org-capture nil "iL"))
        (t (error "Invalid prefix argument: %s" prefix))))

(defun my-org-day-agenda-skip-todo-if ()
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (scheduled (org-get-scheduled-time (point)))
        (deadline (org-get-deadline-time (point)))
        (priority (org-get-priority (thing-at-point 'line t)))
        (tags (org-get-tags))
        (category (org-get-category)))
    (if (or scheduled
            deadline
            (string= category "OPENINBOX")
            (< priority 1000)
            (and (or (member "PROJECT" tags) (member "AREA" tags))
                 (< priority 3000)))
        subtree-end)))

(defun my-org-day-agenda-skip-project-if ()
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (priority (org-get-priority (thing-at-point 'line t))))
    (if (< priority 1000)
        subtree-end)))

(defconst my-org-day-agenda
  `("d" "Day" ((agenda "" ((org-agenda-span 1)
                           (org-agenda-skip-scheduled-if-done t)
                           (org-agenda-skip-deadline-if-done t)
                           (org-agenda-skip-timestamp-if-done t)))
               (todo "TODO" ((org-agenda-overriding-header "Ad-hoc tasks and high-prio project tasks")
                             (org-agenda-skip-function 'my-org-day-agenda-skip-todo-if)
                             (org-agenda-files ',my-local-gtd-files)))
               (tags "+PROJECT" ((org-agenda-overriding-header "Projects")
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-sorting-strategy '(priority-down))
                                 (org-agenda-skip-function 'my-org-day-agenda-skip-project-if)
                                 (org-agenda-files '(,my-local-gtd-projects-dir))))
               (todo "TODO" ((org-agenda-overriding-header "Open ad-hoc tasks and high-prio project tasks")
                             (org-agenda-skip-function 'my-org-day-agenda-skip-todo-if)
                             (org-agenda-files ',my-open-gtd-files)))
               (tags "+PROJECT" ((org-agenda-overriding-header "Open projects")
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-sorting-strategy '(priority-down))
                                 (org-agenda-skip-function 'my-org-day-agenda-skip-project-if)
                                 (org-agenda-files '(,my-open-gtd-projects-dir)))))))

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
  (org-use-speed-commands t)
  (org-attach-id-dir (concat my-local-org-dir "/attachments/"))
  (org-attach-use-inheritance nil)
  (org-startup-folded 'showall)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-log-done 'time)
  (org-capture-templates my-gtd-capture-templates)
  (org-agenda-files `(,my-local-gtd-dir
                      ,(expand-file-name "projects" my-local-gtd-dir)
                      ,(expand-file-name "areas" my-local-gtd-dir)
                      ,my-open-gtd-dir
                      ,(expand-file-name "projects" my-open-gtd-dir)
                      ,(expand-file-name "areas" my-open-gtd-dir)))
  (org-refile-targets '((org-agenda-files :level . 2)))
  (org-priority-lowest 69)
  (org-priority-default 68)
  (org-agenda-custom-commands `(,my-org-day-agenda))
  (org-habit-graph-column 80)
  (org-habit-show-done-always-green t)
  :config
  (require 'org-attach)
  (require 'org-id)
  (require 'org-habit)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))
  (add-to-list 'org-modules 'org-id)
  (add-hook 'org-mode-hook 'my-org-setup)
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c S" . my-org-sort-todos)
   ("M-g e" . my-org-end-of-subtree)
   ("C-c i" . my-gtd-capture-note)
   ("C-c I" . my-gtd-capture-todo)
   ("C-c o u" . my-org-add-update)
   ("C-c o l" . my-org-insert-timestamped-heading)
   ("C-c o D" . my-org-duplicate-subtree)
   ("C-c o w" . my-org-refile-note)
   :map org-mode-map
   ("C-c P i" . org-id-get-create)))

(defface my-org-checked-checkbox-face
  '((t (:inherit org-done)))
  "Face for a checked Org mode checkbox.")

(defun my-org-set-checked-checkbox-face ()
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'my-org-checked-checkbox-face prepend))
   'append))

(use-package org-modern
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  (org-modern-priority-faces '((?A :background "IndianRed" :foreground "white")
                               (?B :background "Goldenrod" :foreground "white")
                               (?C :background "DarkOliveGreen" :foreground "white")
                               (?D :background "SteelBlue" :foreground "white")
                               (?E :background "DarkOrchid" :foreground "white")))
  :custom-face
  (org-modern-tag ((t (:foreground "white" :background "#9575cd" :slant italic))))
  (org-checkbox ((t (:height 1.3))))
  :config
  (global-org-modern-mode)
  (my-org-set-checked-checkbox-face))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package ob-restclient
  :after org)

(use-package ox-slack
  :after org)

(use-package org-super-links
  :straight (org-super-links :type git :host github :repo "toshism/org-super-links" :branch "develop")
  :bind (("C-c s s" . org-super-links-link)
         ("C-c s l" . org-super-links-store-link)
         ("C-c s C-l" . org-super-links-insert-link)))

(use-package gptel
  :custom
  (gptel-model 'gpt-4o)
  (gptel-default-mode 'org-mode)
  :config
  (add-hook 'gptel-mode-hook 'toggle-truncate-lines)
  (require 'my-gptel)
  :bind
  (("C-c SPC" . gptel)
   :map gptel-mode-map
   ("C-c k" . gptel-abort)))

(use-package my-gptel
  :straight nil
  :mode ("\\.gpt\\'" . my-gptel-mode)
  :bind
  (:map gptel-mode-map
        ("C-c d h" . my-gptel-clear-buffer)
        ("C-c C-c" . my-gptel-send)))

(use-package ledger-mode
  :unless (my-windows-p)
  :custom
  (ledger-default-date-format "%Y-%m-%d"))

(use-package my-ledger
  :straight nil
  :after ledger-mode)

(use-package vterm
  :unless (my-windows-p)
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-shell my-fish-path)
  (vterm-max-scrollback 50000)
  (vterm-clear-scrollback-when-clearing t)
  :bind
  (("C-x v" . vterm)
   ("C-x 4 v" . vterm-other-window)))

(use-package shell
  :custom
  (shell-kill-buffer-on-exit t))

(use-package copilot
  :unless (my-windows-p)
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :custom
  (copilot-idle-delay 0.3)
  :custom-face (copilot-overlay-face ((t (:foreground "DarkOrchid" :slant italic))))
  :config
  (add-to-list 'warning-suppress-log-types '(copilot copilot-no-mode-indent))
  :hook (prog-mode . copilot-mode))

(use-package my-copilot
  :straight nil
  :after copilot)

(use-package jinx
  :unless (my-windows-p)
  :custom (jinx-languages "en_US de_DE ru")
  :hook (emacs-startup . global-jinx-mode)
  :bind
  (("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages)))

(use-package rg
  :unless (my-windows-p)
  :bind
  (("M-s R" . rg-project)))

(use-package tree-sitter
  :unless (my-windows-p)
  :custom
  (treesit-font-lock-level 4))

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "<f5>")
  (lsp-pylsp-plugins-black-enabled t)
  :hook
  (scala-ts-mode . lsp)
  (python-mode . lsp)
  :commands lsp
  :bind
  (:map lsp-mode-map
        ([M-down-mouse-1] . mouse-set-point)
        ([M-mouse-1] . lsp-find-definition)
        ([M-mouse-3] . xref-go-back)
        ("<f5> I" . lsp-metals-build-import)))

(use-package lsp-metals)

(use-package lsp-ui :commands lsp-ui-mode)

(setq lsp-completion-provider :none)

(defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))

(add-hook 'lsp-mode-hook #'corfu-lsp-setup)

(use-package consult-lsp
  :bind
  (:map lsp-mode-map
        ("<f5> d" . consult-lsp-diagnostics)
        ("<f5> s" . consult-lsp-file-symbols)
        ("<f5> S" . consult-lsp-symbols)))

(use-package flycheck
  :init
  (setq flycheck-global-modes '(not org-mode))
  :config
  (global-flycheck-mode))

(use-package scala-ts-mode
  :unless (my-windows-p)
  :interpreter "scala")

(use-package jarchive
  :unless (my-windows-p)
  :after eglot
  :config
  (jarchive-setup))

(use-package kubel
  :unless (my-windows-p)
  :config
  (with-eval-after-load 'vterm
    (add-to-list 'vterm-tramp-shells '("kubectl" "/bin/bash")))
  :bind
  (("C-c K" . kubel)
   :map kubel-mode-map
   ("n" . next-line)
   ("p" . previous-line)
   ("N" . kubel-set-namespace)
   ("v" . kubel-exec-vterm-pod)
   ("D" . kubel-exec-pod)
   ("P" . kubel-port-forward-pod)))

(use-package markdown-mode
  :interpreter "markdown")

(define-derived-mode anki-mode org-mode "Anki")

(add-to-list 'auto-mode-alist '("\\.anki\\'" . anki-mode))

(use-package anki-editor
  :unless (my-windows-p)
  :hook (anki-mode . anki-editor-mode)
  :bind
  (:map anki-mode-map
        ("C-<return>" . anki-editor-insert-note)
        ("C-c p p" . anki-editor-push-notes)
        ("C-c p r" . anki-editor-retry-failure-notes)))

(use-package plantuml-mode)

(use-package clojure-mode)

(use-package cider)

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)

(use-package my-openai-tools
  :straight nil)

(use-package terraform-mode)

(use-package graphql-mode)

(use-package decide)

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

(use-package embark-consult
  :after (embark consult))

(use-package notdeft
  :straight (notdeft :type git :host github :repo "hasu/notdeft" :files ("*.el" "xapian"))
  :custom
  (notdeft-directory "~/roam")
  (notdeft-directories '("~/roam"))
  (notdeft-xapian-program (expand-file-name "straight/build/notdeft/xapian/notdeft-xapian" user-emacs-directory))
  :config
  (notdeft-install)
  :bind
  (("C-c n n" . notdeft)))

(use-package dash)

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  (require 'envrc)
  (add-hook 'change-major-mode-after-body-hook 'envrc-mode))

(use-package just-mode)

(use-package elfeed
  :custom
  (elfeed-feeds my-elfeed-feeds))

(use-package aider
  :unless (my-windows-p)
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  ;; For latest claude sonnet model
  (setq aider-args '("--model" "o3-mini"))
  (global-set-key (kbd "C-c b") 'aider-transient-menu))
