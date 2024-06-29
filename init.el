(load (expand-file-name "local.el" user-emacs-directory))

(defun my-windows-p ()
  (eq system-type 'windows-nt))

(defun my-mac-p ()
  (eq system-type 'darwin))

(defun my-jq (json-string jq-filter)
  (let ((temp-file (make-temp-file "emacs-jq")))
    ;; Write JSON to a temporary file
    (with-temp-file temp-file
      (insert json-string))
    ;; Call jq with the specified filter on the JSON file
    ;; and return the output
    (shell-command-to-string (format "jq '%s' %s" jq-filter temp-file))))

(defun my-restclient-extract-json (response)
  (let* ((begin-marker "#+BEGIN_SRC js")
         (end-marker "// ")
         (begin-pos (string-match (regexp-quote begin-marker) response))
         (end-pos (string-match (regexp-quote end-marker) response begin-pos)))
    (when (and begin-pos end-pos)
      (substring response (+ begin-pos (length begin-marker)) end-pos))))

(defun my-restclient-extract-ids-as-json (in jq-query)
  (json-encode-list (mapcar 'string-to-number (split-string (my-jq (my-restclient-extract-json in) jq-query) "\n" t))))

(defun string-in-list-p (str lst)
  (require 'cl-lib)
  (cl-find-if (lambda (item) (string-equal str item)) lst))

(defun my-grab-java-package-name ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^package \\(.*\\)" nil t)
        (let* ((package-path (match-string 1))
               (package-components (split-string package-path "\\.")))
          (car (last package-components)))
      (error "Package declaration not found"))))

(defun my-indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(when (my-windows-p)
  (setenv "PATH" (concat my-git-path ";" (getenv "PATH")))
  (push my-git-path exec-path))

(use-package emacs
  :custom
  (create-lockfiles nil)
  (make-backup-files nil)
  (inhibit-splash-screen t)
  (initial-scratch-message "All systems go! 🚀")
  (initial-major-mode 'text-mode)
  (global-auto-revert-non-file-buffers t)
  (disabled-command-function nil)
  (save-interprogram-paste-before-kill t)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024))
  (visible-bell t)
  (require-final-newline t)
  (indent-tabs-mode nil)
  (zoneinfo-style-world-list '(("America/Los_Angeles" "LA")
			       ("America/New_York" "NYC")
			       ("Europe/London" "London")
			       ("Europe/Lisbon" "Lisbon")
			       ("Europe/Berlin" "Berlin")
			       ("Europe/Kyiv" "Kyiv")))
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (backward-delete-char-untabify-method 'hungry)
  :config
  (global-auto-revert-mode)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (global-subword-mode)
  (tab-bar-history-mode)
  :bind
  (("C-c j s" . scratch-buffer)
   ("C-x C-b" . ibuffer-other-window)
   ("C-M-; d" . duplicate-dwim)
   ("M-g w" . forward-to-word)
   ("M-g W" . backward-to-word)
   ("C-c d h" . erase-buffer)
   ("M-z" . zap-up-to-char)
   ("M-Z" . zap-to-char)
   ("<f8> h" . tab-bar-history-back)
   ("<f8> l" . tab-bar-history-forward)
   ("<f8> H" . previous-buffer)
   ("<f8> L" . next-buffer)
   ("<f5> = =" . my-indent-buffer)
   :map prog-mode-map
   ("DEL" . backward-delete-char-untabify)))

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

(use-package hydra)

(use-package org-ql)

(defconst my-inbox-path "~/org/inbox.org")
(defconst my-shared-inbox-path "~/org_shared/shared_inbox.org")

(defconst my-tasks-path "~/org/tasks.org")
(defconst my-shared-tasks-path "~/org_shared/shared_tasks.org")

(defconst my-projects-path "~/org/projects.org")
(defconst my-shared-projects-path "~/org_shared/shared_projects.org")
(defconst my-project-files (list my-projects-path my-shared-projects-path))

(defconst my-calendar-path "~/org/calendar.org")
(defconst my-shared-calendar-path "~/org_shared/shared_calendar.org")

(defconst my-someday-path "~/org/someday.org")
(defconst my-shared-someday-path "~/org_shared/shared_someday.org")

(defconst my-interests-path "~/org/interests.org")
(defconst my-shared-interests-path "~/org_shared/shared_interests.org")
(defconst my-interests-files (list my-interests-path my-shared-interests-path))

(defun my-select-projects ()
  (org-ql-query
    :select '(cons (substring-no-properties (org-get-heading t t t t))
                   (org-id-get-create))
    :from my-project-files
    :where '(property "MY_TYPE" "project")))

(defun my-template (name)
  (expand-file-name (concat "templates/" name ".txt") user-emacs-directory))

(defvar my-prompt-history nil)

(defun my-prompt (prompt var)
  (make-local-variable var)
  (set var (read-string (concat prompt ": ") nil 'my-prompt-history)))

(defconst my-templates
  `(("i" "Inbox")
    ("ii" "Todo" entry (file+headline ,my-inbox-path "Inbox") "* TODO %?")
    ("iI" "Note" entry (file+headline ,my-inbox-path "Inbox") "* %?")
    ("il" "Todo Link" entry (file+headline ,my-inbox-path "Inbox") "* TODO [[%c][%^{Description}]]%? :LINK:")
    ("iL" "Link" entry (file+headline ,my-inbox-path "Inbox") "* [[%c][%^{Description}]]%? :LINK:")
    ("it" "Log" entry (file+headline ,my-inbox-path "Inbox") "* %u %?")
    ("ia" "Annotation" entry (file+headline ,my-inbox-path "Inbox") "* %A%?")
    ("f" "Folder")
    ("fp" "Project" entry (file ,my-projects-path) (file ,(my-template "project")))
    ("fP" "Shared Project" entry (file ,my-shared-projects-path) (file ,(my-template "project")))
    ("ft" "Subproject" entry (file ,my-inbox-path) (file ,(my-template "subproject")))
    ("fs" "Someday" entry (file ,my-someday-path) (file ,(my-template "folder")))
    ("fS" "Shared Someday" entry (file ,my-shared-someday-path) (file ,(my-template "folder")))
    ("fi" "Interest" entry (file ,my-interests-path) (file ,(my-template "interest")))
    ("fI" "Shared Interest" entry (file ,my-shared-interests-path) (file ,(my-template "interest")))))

(defun my-capture-to-inbox (&optional prefix)
  (interactive "P")
  (cond
   ((equal prefix nil) (org-capture nil "ii"))
   ((equal prefix '(4)) (org-capture nil "il"))
   ((equal prefix '(16)) (org-capture nil "it"))
   (t (message "Prefix '%s' not supported" prefix))))

(defun my-capture-note-to-inbox (&optional prefix)
  (interactive "P")
  (cond
   ((equal prefix nil) (org-capture nil "iI"))
   ((equal prefix '(4)) (org-capture nil "iL"))
   (t (message "Prefix '%s' not supported" prefix))))

(defun my-annotate ()
  (interactive)
  (org-store-link nil)
  (org-capture nil "ia"))

(defconst my-refile-targets
  `(((,my-inbox-path
      ,my-shared-inbox-path
      ,my-someday-path
      ,my-shared-someday-path
      ,my-interests-path
      ,my-shared-interests-path
      ,my-tasks-path
      ,my-shared-tasks-path
      ,my-calendar-path
      ,my-shared-calendar-path)
     :level . 1)
    ((,my-projects-path ,my-shared-projects-path) :maxlevel . 2)))

(defun my-verify-refile-target ()
  (if (string-in-list-p (buffer-file-name) (mapcar 'expand-file-name (list my-projects-path my-shared-projects-path)))
      (when-let ((type (org-element-property :MY_TYPE (org-element-at-point))))
          (string= type "project"))
    t))

(defun my-org-after-refile-insert ()
  (save-excursion
    (org-back-to-heading t)
    (org-update-parent-todo-statistics)))

(defconst my-agenda-files (list my-tasks-path
                                my-shared-tasks-path
                                my-projects-path
                                my-shared-projects-path
                                my-calendar-path
                                my-shared-calendar-path))

(defconst my-custom-agendas
  `(("d" "Day"
     ((agenda "" ((org-agenda-span 1)
                  (org-agenda-skip-scheduled-if-done t)
                  (org-agenda-skip-deadline-if-done t)
                  (org-agenda-skip-timestamp-if-done t)
                  (org-agenda-files ',my-agenda-files)))
      (todo "TODO" ((org-agenda-overriding-header "Not-Scheduled Tasks")
                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                    (org-agenda-files '(,my-tasks-path ,my-shared-tasks-path))))
      (tags "NOW+MY_TYPE=\"project\"-TODO=\"DONE\"" ((org-agenda-overriding-header "Projects")
                                                     (org-agenda-files '(,my-projects-path ,my-shared-projects-path))))))))

(defun my-add-timestamp-to-heading ()
  (interactive)
  (when (org-at-heading-p)
    (org-set-property "MY_TIMESTAMP" (format-time-string "[%Y-%m-%d %a %H:%M]"))))

(defun my-make-heading-hash-map (headings-with-id)
  (let ((hm (make-hash-table :test 'equal)))
    (dolist (heading headings-with-id)
      (puthash (car heading) (cdr heading) hm))
    hm))

(defun my-jump-to-heading (hash-map-f select-prompt)
  (let* ((title-to-id (funcall hash-map-f))
         (selected-title (completing-read select-prompt title-to-id))
         (selected-id (gethash selected-title title-to-id))
         (file (org-id-find-id-file selected-id)))
    (if-let (win (get-buffer-window (get-file-buffer file) t))
        (select-window win)
      (switch-to-buffer-other-window file))
    (widen)
    (org-id-goto selected-id)
    (org-show-subtree)
    (org-narrow-to-subtree)))

(defun my-project-title-to-id ()
  (my-make-heading-hash-map (my-select-projects)))

(defun my-jump-to-project ()
  (interactive)
  (my-jump-to-heading 'my-project-title-to-id "Project: "))

(defun my-select-interests ()
  (org-ql-query
    :select '(cons (substring-no-properties (org-get-heading t t t t))
                   (org-id-get-create))
    :from my-interests-files
    :where '(property "MY_TYPE" "interest")))

(defun my-interest-title-to-id ()
  (my-make-heading-hash-map (my-select-interests)))

(defun my-jump-to-interest ()
  (interactive)
  (my-jump-to-heading 'my-interest-title-to-id "Interest: "))

(use-package org
  :defer t
  :custom
  (org-confirm-babel-evaluate nil)
  (org-src-window-setup 'split-window-below)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-use-sub-superscripts '{})
  (org-capture-templates my-templates)
  (org-refile-targets my-refile-targets)
  (org-refile-target-verify-function 'my-verify-refile-target)
  (org-agenda-files my-agenda-files)
  (org-agenda-custom-commands my-custom-agendas)
  (org-habit-graph-column 60)
  (org-use-speed-commands t)
  :config
  (require 'org-attach)
  (add-to-list 'org-export-backends 'md)
  (add-to-list 'org-modules 'org-habit)
  (add-hook 'org-after-refile-insert-hook 'my-org-after-refile-insert)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c i" . my-capture-to-inbox)
   ("C-c I" . my-capture-note-to-inbox)
   ("C-c n" . my-annotate)
   ("C-c a" . org-agenda)
   ("C-c j p" . my-jump-to-project)
   ("C-c j i" . my-jump-to-interest)
   :map org-mode-map
   ("C-c t" . my-add-timestamp-to-heading)))

(define-derived-mode anki-mode org-mode "Anki")

(add-to-list 'auto-mode-alist '("\\.anki\\'" . anki-mode))

(use-package anki-editor
  :hook (anki-mode . anki-editor-mode)
  :bind
  (:map anki-mode-map
        ("C-<return>" . anki-editor-insert-note)
        ("C-c p p" . anki-editor-push-notes)
        ("C-c p r" . anki-editor-retry-failure-notes)))

(use-package org-modern
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  :config
  (global-org-modern-mode))

(use-package ox-slack)

(use-package magit
  :bind
  (("C-c g" . magit-file-dispatch)))

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
  :config
  (consult-customize
   consult-buffer
   :preview-key "M-.")
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
  (aw-scope 'frame)
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

(use-package corfu
  :demand
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-auto-prefix 2)
  :config
  (global-corfu-mode)
  :bind
  (:map corfu-map
        (("C-SPC" . corfu-insert-separator)
         ("C-;" . corfu-quick-insert))))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :custom
  (copilot-idle-delay 0.3)
  :config
  (add-to-list 'warning-suppress-log-types '(copilot copilot-no-mode-indent))
  (defhydra my-accept-copilot-completion-hydra (copilot-mode-map "C-<tab>")
    "Accept Copilot completion"
    ("C-<tab>" copilot-accept-completion "Accept" :color blue)
    ("M-f" copilot-accept-completion-by-word "Word")
    ("C-e" copilot-accept-completion-by-line "Line"))
  :hook (prog-mode . copilot-mode))

(defun my-gptel-clear ()
  (interactive)
  (erase-buffer)
  (insert "*** "))

(define-derived-mode my-gptel-mode org-mode "GPTel")

(add-to-list 'auto-mode-alist '("\\.gptel\\'" . my-gptel-mode))

(defun my-gptel-mode-setup ()
  (interactive)
  (org-mode)
  (gptel-mode))

(use-package gptel
  :custom
  (gptel-model "gpt-4o")
  (gptel-default-mode 'org-mode)
  :config
  (add-hook 'gptel-mode-hook 'toggle-truncate-lines)
  (add-hook 'my-gptel-mode-hook 'my-gptel-mode-setup)
  :bind
  (("C-c SPC" . gptel)
   :map gptel-mode-map
   ("C-c C-c" . gptel-send)
   ("C-c d h" . my-gptel-clear)
   ("C-c x a" . gptel-abort)))

(use-package ob-restclient)

(defun my-vterm-unbind-keys ()
  (local-unset-key (kbd "M-s"))
  (local-unset-key (kbd "<f8>")))

(defun my-vterm-project ()
  (interactive)
  (let* ((project-path (when-let ((project (project-current)))
                         (project-root project)))
         (project-name (when project-path
                         (file-name-nondirectory (directory-file-name project-path))))
         (buffer-name (when project-name
                        (format "*%s: vterm*" project-name))))
    (if project-path
        (progn
          (unless (get-buffer buffer-name)
            (let ((default-directory project-path))
              (vterm buffer-name)))
          (switch-to-buffer buffer-name))
      (message "Not in a project"))))

(use-package vterm
  :when (not (my-windows-p))
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-shell my-fish-path)
  (vterm-max-scrollback 50000)
  :config
  (add-hook 'vterm-mode-hook 'my-vterm-unbind-keys)
  :bind
  (("C-x v" . vterm)
   ("C-x 4 v" . vterm-other-window)
   ("C-x p v" . my-vterm-project)))

(use-package shell
  :custom
  (shell-kill-buffer-on-exit t))

(use-package rg
  :bind
  (("M-s R" . rg-project)))

(use-package project
  :config
  (add-to-list 'project-switch-commands '(my-vterm-project "Vterm" "V") t)
  (add-to-list 'project-switch-commands '(project-dired "Dired" "<return>") t))

(setq my-insert-directory-program
      (cond ((my-windows-p) insert-directory-program)
            ((my-mac-p) "gls")
            (t "ls")))

(setq my-dired-listing-switches
      (cond ((my-windows-p) dired-listing-switches)
            (t "-alh --group-directories-first")))

(use-package dired
  :straight nil
  :custom
  (dired-dwim-target t)
  (insert-directory-program my-insert-directory-program)
  (dired-listing-switches my-dired-listing-switches)
  :bind
  (("<f7>" . dired-jump)
   :map dired-mode-map
   ("o" . crux-open-with)
   ("<tab>" . dired-find-file-other-window)))

(use-package kubel
  :bind
  (("C-c K" . kubel)
   :map kubel-mode-map
   ("n" . next-line)
   ("p" . previous-line)
   ("N" . kubel-set-namespace)
   ("v" . kubel-exec-shell-pod)
   ("D" . kubel-exec-pod)))

(use-package terraform-mode
  :bind
  (:map terraform-mode-map
        ("<f5> = =" . terraform-format-buffer)))

(use-package js
  :custom
  (js-indent-level 2)
  :bind
  (:map js-json-mode-map
        ("<f5> = =" . json-pretty-print-buffer)))

(use-package emms
  :defer t
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

(defun my-ledger-bal (period)
  (format "%%(binary) -f %%(ledger-file) --invert --period \"%s\" -S amount bal ^Income ^Expenses" period))

(use-package clojure-mode)

(use-package sbt-mode)

(use-package tree-sitter
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package scala-ts-mode)

(use-package eglot
  :hook (scala-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((scala-ts-mode) . ("metals")))
  :bind
  (:map eglot-mode-map
        ("C-c e a" . eglot-code-actions)
        ("C-c e i" . eglot-code-action-organize-imports)
        ("C-c e f" . eglot-format-buffer)
        ("C-c e r" . eglot-rename)
        ([M-down-mouse-1] . mouse-set-point)
        ([M-mouse-1] . xref-find-definitions)
        ([M-mouse-3] . xref-go-back)))

(use-package jarchive
  :after eglot
  :config
  (jarchive-setup))

(defun display-string-with-newlines (str)
  "Display the current region or prompt for a string, with `\\n` interpreted as line breaks."
  (interactive
   (if (use-region-p)
       (list (buffer-substring-no-properties (region-beginning) (region-end)))
     (list (read-string "Enter the string: "))))
  (with-output-to-temp-buffer "*String Display*"
    (princ (replace-regexp-in-string "\\\\n" "\n" str))))

(use-package markdown-mode)

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (markdown-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (add-hook 'flyspell-mode-hook (lambda () (define-key flyspell-mode-map (kbd "C-;") nil))))
