(defconst my-file-timestamp-format "%Y%m%d%H%M%S")

(defun my-mac-p ()
  (equal system-type 'darwin))

(defun my-windows-p ()
  (equal system-type 'windows-nt))

(load (expand-file-name "local.el" user-emacs-directory))

(when (or (my-mac-p) (my-windows-p))
  (let* ((separator (if (my-mac-p) ":" ";")))
    (setenv "PATH" (concat my-git-path separator (getenv "PATH")))
    (push my-git-path exec-path)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package exec-path-from-shell
  :if (my-mac-p)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "JAVA_HOME")
  (exec-path-from-shell-copy-env "XINGBOX_OWNER")
  (exec-path-from-shell-copy-env "XINGBOX_API_KEY"))

(straight-use-package 'org)

(use-package bookmark+
  :straight (bookmark+ :type git :host github :repo "emacsmirror/bookmark-plus")
  :init
  (setq bookmark-default-file "~/.emacs-state/bookmarks")
  :custom
  (bmkp-last-as-first-bookmark-file nil)
  (bmkp-bmenu-state-file "~/.emacs-state/bmenu-state"))

(use-package org-bookmark-heading)

(use-package emms
  :init
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-player-mpv-update-metadata t)
  (setq emms-streams-file (expand-file-name "streams.emms" user-emacs-directory))
  :config
  (emms-all)
  :bind
  ("C-c r r" . 'emms-streams)
  ("C-c r p" . 'emms-pause)
  ("C-c r s" . 'emms-stop))

(defun my-initial-buffer ()
  (emms-streams)
  (get-buffer "Emms Streams"))

(setq disabled-command-function nil)

(setq initial-buffer-choice #'my-initial-buffer)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)

(setq save-interprogram-paste-before-kill t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq gc-cons-threshold 100000000) ;; 100mb
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq shell-file-name "/bin/sh")

(setq create-lockfiles nil)
(setq make-backup-files nil)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode)

(setq custom-file (concat user-emacs-directory "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell t) ;; Alert via flashing the screen instead of playing a sound
(fset 'yes-or-no-p 'y-or-n-p) ;; Confirm with 'y' or 'n' instead of 'yes' or 'no'
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(if (boundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode +1)
  (pixel-scroll-mode +1))

(setq require-final-newline t)
(setq-default indent-tabs-mode nil) ;; Always use spaces for indentation

(global-subword-mode)

(global-set-key (kbd "C-c q") #'bury-buffer)
(global-set-key (kbd "C-c x") #'kill-current-buffer)
(global-set-key (kbd "C-c X") #'kill-buffer-and-window)

(global-set-key (kbd "C-x C-b") #'ibuffer)

(global-set-key (kbd "M-z") 'zap-up-to-char) ;; Replace zap-to-char
(global-set-key (kbd "C-M-z") 'zap-to-char)

(global-set-key (kbd "C-M-; d") 'duplicate-dwim)

(global-set-key (kbd "C-c t") 'toggle-truncate-lines)

(global-set-key [C-down-mouse-1] 'mouse-set-point)
(global-set-key [C-mouse-1] 'ffap-at-mouse)

(global-set-key (kbd "C-c E") 'erase-buffer)

(global-set-key (kbd "C-c f") 'forward-to-word)
(global-set-key (kbd "C-c b") 'backward-to-word)

(with-eval-after-load 'conf-mode
  (define-key conf-mode-map (kbd "C-c SPC") nil))

;; GPT-4
(defun my-project-name ()
  "Return the current project name or an empty string if not in a project."
  (let ((project-root (project-current)))
    (if project-root
        (file-name-nondirectory (directory-file-name (car (project-roots project-root))))
      "")))

;; GPT-4
(defun my-project-modeline ()
  "Return the current project name for the mode line, or an empty string if not in a project."
  (let ((project-name (my-project-name)))
    (if (not (string-empty-p project-name))
        (concat " [" project-name "]")
      "")))

;; GPT-4
(setq-default mode-line-format
              (list
               ;; Standard elements
               "%e"  ; Display error message about full memory, etc.
               mode-line-front-space
               mode-line-mule-info
               mode-line-client
               mode-line-modified
               mode-line-remote
               mode-line-frame-identification
               mode-line-buffer-identification
               "   "
               mode-line-position

               ;; Add the current project name
               '(:eval (my-project-modeline))

               ;; More standard elements
               "  "
               mode-line-modes
               mode-line-misc-info
               mode-line-end-spaces))

(epa-file-enable)
(setq epa-file-select-keys 'silent)
(setq epa-pinentry-mode 'loopback)
(setq epa-file-encrypt-to my-gpg-key-email)

;; Written by ChatGPT
(defun my-vterm-project-root ()
  "Start a vterm session in the root of the current project with the buffer named after the project name."
  (interactive)
  (let* ((project-root (when-let ((project (project-current)))
                         (project-root project)))
         (project-name (when project-root
                         (file-name-nondirectory (directory-file-name project-root))))
         (vterm-buffer-name (when project-name
                              (format "*%s: vterm*" project-name))))
    (if project-root
        (progn
          (unless (get-buffer vterm-buffer-name)
            (let ((default-directory project-root))
              (vterm vterm-buffer-name)))
          (switch-to-buffer vterm-buffer-name))
      (message "Not in a project"))))

(defun my-vterm-unbind-keys ()
  (local-unset-key (kbd "M-s")))

(use-package vterm
  :demand
  :when (not (my-windows-p))
  :init
  (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (setq vterm-shell my-fish-path)
  (setq vterm-max-scrollback 50000)
  :config
  (add-hook 'vterm-mode-hook 'my-vterm-unbind-keys)
  (add-hook 'vterm-mode-hook 'compilation-shell-minor-mode)
  :bind
  (("C-x v" . vterm)
   ("C-x 4 v" . vterm-other-window)
   ("C-x p v" . my-vterm-project-root)
   :map vterm-mode-map
   ("<f4>" . rename-buffer)))

;; Written by ChatGPT
(defun my-normalize-file-name (file)
  "Normalize the given FILE name by replacing characters except Latin letters, numbers, and dashes."
  (let* ((basename (file-name-nondirectory file))
         (is-directory (file-directory-p file))
         (file-ext (and (not is-directory) (file-name-extension basename)))
         (file-name-without-ext (if is-directory basename (file-name-sans-extension basename)))
         (normalized-file-name (replace-regexp-in-string "\\([^a-zA-Z0-9-]\\)+" "-" file-name-without-ext)))
    (if file-ext
        (concat normalized-file-name "." file-ext)
      normalized-file-name)))

;; Written by ChatGPT
(defun my-rename-normalize-files ()
  "Normalize the marked files in dired by replacing characters except Latin letters, numbers, and dashes."
  (interactive)
  (require 'dired-aux)
  (let ((files (dired-get-marked-files)))
    (dolist (file files)
      (let* ((dir (file-name-directory file))
             (new-basename (my-normalize-file-name file))
             (new-name (expand-file-name new-basename dir)))
        (dired-rename-file file new-name 1)))))

;; Written by ChatGPT
(defun my-rename-normalize-files-with-timestamp (arg)
  "Normalize the marked files in dired and add a timestamp to their names.
With a prefix argument, prompt for a custom date and time to use in the timestamp."
  (interactive "P")
  (let* ((files (dired-get-marked-files))
         (timestamp-regex "^\\([0-9]\\{14\\}\\)-"))
    (dolist (file files)
      (let* ((dir (file-name-directory file))
             (normalized-basename (my-normalize-file-name file))
             (file-attrs (file-attributes file))
             (file-last-modified (when file-attrs (nth 5 file-attrs)))
             (custom-timestamp (when arg (org-read-date t t)))
             (timestamp (if custom-timestamp
                            (format-time-string my-file-timestamp-format custom-timestamp)
                          (format-time-string my-file-timestamp-format file-last-modified)))
             (new-basename (if (string-match timestamp-regex normalized-basename)
                               (replace-match timestamp nil nil normalized-basename 1)
                             (concat timestamp "-" normalized-basename)))
             (new-name (expand-file-name new-basename dir)))
        (dired-rename-file file new-name 1)))))

(use-package dired
  :straight nil
  :demand
  :init
  (setq dired-dwim-target t)
  :bind
  (:map dired-mode-map
        ("o" . crux-open-with)
        ("<tab>" . dired-find-file-other-window)
        ("C-c d r" . my-rename-normalize-files)
        ("C-c d t" . my-rename-normalize-files-with-timestamp)))

(use-package dirvish
  :config
  (dirvish-override-dired-mode)
  :bind
  (("<f7>" . dired-jump)
   :map dirvish-mode-map
   ("<f7>" . dired-jump)))

(use-package hydra)

(when (not (my-windows-p))
  (set-face-attribute 'default nil :font "Iosevka Comfy Wide Fixed" :height my-font-height)
  (set-frame-font "Iosevka Comfy Wide Fixed" nil t))

(use-package modus-themes
  :init
  (setq modus-themes-bold-constructs nil)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-prompts '(ultrabold))
  (setq modus-themes-org-blocks 'gray-background)
  (setq modus-themes-headings '((0 . (ultrabold 1.7))
                                (1 . (ultrabold 1.5))
                                (2 . (extrabold 1.3))
                                (t . (1.1))))
  (setq modus-themes-variable-pitch-ui t)
  :config
  (setq modus-themes-common-palette-overrides nil)
  (modus-themes-load-theme 'modus-vivendi))

(defun my-zoom-frame (&optional n frame amt)
  "Increase the default size of text by AMT inside FRAME N times.
  N can be given as a prefix arg.
  AMT will default to 10.
  FRAME will default the selected frame."
  (interactive "p")
  (let ((frame (or frame (selected-frame)))
        (height (+ (face-attribute 'default :height frame) (* n (or amt 10)))))
    (set-face-attribute 'default frame :height height)
    (when (called-interactively-p)
      (message "Set frame's default text height to %d." height))))

(defun my-zoom-frame-out (&optional n frame amt)
  "Call `my-zoom-frame' with -N."
  (interactive "p")
  (my-zoom-frame (- n) frame amt))

(defun my-zoom-frame-default ()
  (interactive)
  (set-face-attribute 'default (selected-frame) :height my-font-height))

(defhydra my-zoom-and-theme-hydra (global-map "C-c L")
  "Zoom and theme"
  ("+" my-zoom-frame "In")
  ("-" my-zoom-frame-out "Out")
  ("0" my-zoom-frame-default "Default")
  ("t" modus-themes-toggle "Toggle theme" :color blue)
  ("m" global-org-modern-mode "Toggle modern mode" :color blue)
  ("q" nil "Quit" :color blue))

(use-package super-save
  :init
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil)
  (setq super-save-exclude '(".sbt" "project/"))
  :config
  (super-save-mode +1))

;; GPT-4
(defun my-org-toggle-todo-and-done ()
  "Toggle between TODO and DONE states."
  (interactive)
  (if (org-entry-is-done-p)
      (org-todo "TODO")
    (org-todo "DONE")))

(use-package org
  :demand
  :init
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-indented t)
  (setq org-export-with-sub-superscripts nil)
  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up urgency-down category-keep)
          (todo urgency-down category-keep)
          (tags urgency-down priority-down category-keep)
          (search category-keep)))
  :config
  (setq org-agenda-files '("~/org/tasks.org"
                           "~/org/calendar.org"
                           "~/org/projects.org"
                           "~/org/someday.org"
                           "~/org_work/tasks.org"
                           "~/org_work/calendar.org"
                           "~/org_work/projects.org"
                           "~/org_work/someday.org"))
  :bind
  (("C-c a" . org-agenda)
   ("C-c l" . org-store-link)
   :map org-mode-map
   ("C-c C-t" . my-org-toggle-todo-and-done)))

(advice-add 'org-agenda-goto :after
            (lambda (&rest args)
              (org-narrow-to-subtree)))

(advice-add 'org-agenda-switch-to :after
            (lambda (&rest args)
              (org-narrow-to-subtree)))

(use-package org-ql)

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(use-package toc-org
  :hook ((org-mode markdown-mode) . toc-org-mode))

(use-package restclient)

(use-package ob-restclient)

(with-eval-after-load 'org-babel-load-languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(defun my-execute-first-src-block-under-heading-new ()
  "Execute the first source code block within the content of the current Org heading."
  (interactive)
  (when (org-at-heading-p)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (if (org-babel-next-src-block)
            (org-babel-execute-src-block)
          (message "No source code blocks found within the heading."))))))

(defun my-advice-org-ctrl-c-ctrl-c (orig-fn &rest args)
  "Advice for `org-ctrl-c-ctrl-c' that executes the first source block under the heading when called on a heading."
  (if (and (org-at-heading-p) (not (org-at-block-p)))
      (my-execute-first-src-block-under-heading-new)
    (apply orig-fn args)))

(advice-add 'org-ctrl-c-ctrl-c :around #'my-advice-org-ctrl-c-ctrl-c)

(defun my-open-first-src-block-under-heading-for-editing ()
  "Open the first source code block within the content of the current Org heading for editing."
  (interactive)
  (when (org-at-heading-p)
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (if (org-babel-next-src-block)
            (org-edit-special)
          (message "No source code blocks found within the heading."))))))

(defun my-advice-org-edit-special (orig-fn &rest args)
  "Advice for `org-edit-special' that opens the first source block under the heading for editing when called on a heading."
  (if (and (org-at-heading-p) (not (org-at-block-p)))
      (my-open-first-src-block-under-heading-for-editing)
    (apply orig-fn args)))

(advice-add 'org-edit-special :around #'my-advice-org-edit-special)

(defun my-copy-src-message (src)
  (let ((lines (split-string src "\n")))
    (if (> (length lines) 2)
        (concat "Copied:\n" (nth 0 lines) "\n" (nth 1 lines) "\n  ...")
        (concat "Copied:\n" src))))

(defun my-copy-src (context)
  (let* ((info (org-babel-lob-get-info context))
         (info (if info (copy-tree info) (org-babel-get-src-block-info)))
         (src (nth 1 info)))
    (progn
      (kill-new src)
      (message (my-copy-src-message src)))))

(defun my-copy-link (context)
  (let* ((plist (nth 1 context))
         (raw-link (plist-get plist ':raw-link)))
    (progn
      (kill-new raw-link)
      (message (concat "Copied:\n" raw-link)))))

(defun my-smart-copy ()
  (interactive)
  (let* ((context (org-element-context))
         (context-type (nth 0 context)))
    (cond ((eq context-type 'src-block) (my-copy-src context))
          ((eq context-type 'link) (my-copy-link context))
          (t (message "Nothing to copy")))))

(global-set-key (kbd "C-c y") #'my-smart-copy)

(use-package ox-slack)

(use-package ox-jira)

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

(use-package impatient-mode)

(defface org-checkbox-done-text
    '((t (:inherit org-done)))
    "Face for the text part of a checked org-mode checkbox.")

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-checkbox-done-text prepend))
 'append)

(defun my-capture-template-path (template-name)
  (expand-file-name (format "capture-templates/%s.txt" template-name) user-emacs-directory))

(with-eval-after-load 'org-capture
  (setq org-capture-templates
        (list
         `("i" "Inbox" entry (file "~/org/inbox.org") (file ,(my-capture-template-path "inbox")))
         `("l" "Link" entry (file "~/org/inbox.org") (file ,(my-capture-template-path "link")))
         `("p" "Project")
         `("pp" "Project" entry (file "~/org/projects.org") (file ,(my-capture-template-path "project")))
         `("pw" "Work Project" entry (file "~/org_work/projects.org") (file ,(my-capture-template-path "project")))
         `("s" "Someday Folder")
         `("ss" "Someday" entry (file "~/org/someday.org") (file ,(my-capture-template-path "someday_folder")))
         `("sw" "Work Someday" entry (file "~/org_work/someday.org") (file ,(my-capture-template-path "someday_folder"))))))

(defun my-org-capture-inbox ()
  (interactive)
  (org-capture nil "i"))

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c i") #'my-org-capture-inbox)

(with-eval-after-load 'org-refile
  (setq org-refile-targets '((("~/org/tasks.org"
                               "~/org/calendar.org"
                               "~/org/projects.org"
                               "~/org/someday.org"
                               "~/org_work/tasks.org"
                               "~/org_work/calendar.org"
                               "~/org_work/projects.org"
                               "~/org_work/someday.org") :level . 1)))
  (setq org-refile-use-outline-path t)

  (advice-add 'org-refile :after #'org-save-all-org-buffers))

(defun my-day-agenda (keys title agenda-files projects-file)
  `(,keys
    ,title
    ((agenda "" ((org-agenda-span 1)
                 (org-agenda-skip-scheduled-if-done t)
                 (org-agenda-skip-deadline-if-done t)
                 (org-agenda-skip-timestamp-if-done t)
                 (org-agenda-files ',(cons projects-file agenda-files))))
     (todo "TODO" ((org-agenda-overriding-header "Not-scheduled tasks") (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
     (tags "-soon+LEVEL=1" ((org-agenda-overriding-header "Projects") (org-agenda-files '(,projects-file)))))
    ((org-agenda-files ',agenda-files))))

(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        (list
         (my-day-agenda "d" "Day" '("~/org/tasks.org" "~/org/calendar.org") "~/org/projects.org")
         (my-day-agenda "D" "Work Day" '("~/org_work/tasks.org" "~/org_work/calendar.org") "~/org_work/projects.org"))))

(defconst my/org-project-files '("~/org/projects.org" "~/org_work/projects.org"))

(defun my/get-org-project-titles ()
  (org-ql-query
    :select '(substring-no-properties (org-get-heading t t t t))
    :from my/org-project-files
    :where '(level 1)))

(defun my/search-org-project-tasks (project-title)
  (org-ql-search my/org-project-files
    `(and (level 2) (ancestors (heading ,project-title)))))

(my/get-org-project-titles)

(my/search-org-project-tasks "Winter cleaning")

(defun my/display-org-project-subtasks ()
  (interactive)
  (let ((project (completing-read "Project: " (my/get-org-project-titles))))
    (my/search-org-project-tasks project)))

(defun my/org-projects-with-ids ()
  (org-ql-query
    :select '(cons (substring-no-properties (org-get-heading t t t t))
                   (org-id-get-create))
    :from my/org-project-files
    :where '(level 1)))

(defun my/org-project-map ()
  (let ((heading-map (make-hash-table :test 'equal))
        (headings (my/org-projects-with-ids)))
    (dolist (heading headings)
      (puthash (car heading) (cdr heading) heading-map))
    heading-map))

(my/org-project-map)

(defun my/widen-all-org-buffers (buffer-list)
"Widen all org buffers in the given buffer-list."
(dolist (buffer-file buffer-list)
  (with-current-buffer (get-file-buffer buffer-file)
    (widen))))

(defun my/display-org-project ()
  (interactive)
  (org-narrow-to-subtree)
  (org-cycle '(16)))

(defun my/goto-org-project ()
  (interactive)
  (let* ((project-map (my/org-project-map))
         (selected-title (completing-read "Project: " (hash-table-keys project-map)))
         (selected-id (gethash selected-title project-map)))
    (my/widen-all-org-buffers my/org-project-files)
    (org-id-goto selected-id)
    (my/display-org-project)))

(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 51)

(defun my-title-to-filename (title)
  (let ((timestamp (format-time-string my-file-timestamp-format))
        (default-title (notdeft-default-title-to-notename title)))
    (format "%s-%s" timestamp default-title)))

(defun my-new-note-data (dir notename ext data title)
  (let* ((notename (or notename
                       (when title
                         (notdeft-title-to-notename title))))
         (file (if notename
                   (notdeft-make-filename notename ext dir)
                 (notdeft-generate-filename ext dir))))
    (cons file (or data (format "#+TITLE: %s" title)))))

(use-package notdeft
  :straight (notdeft :type git :host github :repo "hasu/notdeft" :files ("*.el" "xapian"))
  :init
  (setq notdeft-directory "~/notes")
  (setq notdeft-directories '("~/notes" "~/zettelkasten"))
  (setq notdeft-notename-function #'my-title-to-filename)
  (setq notdeft-new-file-data-function #'my-new-note-data)
  (setq notdeft-xapian-program (expand-file-name "straight/build/notdeft/xapian/notdeft-xapian" user-emacs-directory))
  :config
  (notdeft-install)
  :bind
  (("C-c n d" . notdeft)
   ("C-c n n" . notdeft-new-file-named)))

(use-package org-roam
  :custom
  (org-roam-directory "~/org/spark")
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today)
   ("C-c n t" . org-roam-tag-add))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t)
  (setq org-roam-ui-follow t)
  (setq org-roam-ui-update-on-save t)
  (setq org-roam-ui-open-on-start t)
  :bind
  (("C-c n u" . org-roam-ui-open)))

(defun my-create-anki-card ()
  "Create a new Anki card."
  (interactive)
  (insert "* \n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:END:\n\n** Front\n\n** Back\n\n")
  (backward-char 63))

(define-minor-mode my-anki-mode
  "A minor mode for .anki files"
  :lighter " Anki"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c P") 'anki-editor-push-notes)
            (define-key map (kbd "C-c N") 'my-create-anki-card)
            map))

(defun my-anki-file-p ()
  "Check if the current file is an Anki file."
  (and (buffer-file-name)
       (string= (file-name-extension (buffer-file-name)) "anki")))

(use-package anki-editor
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (when (my-anki-file-p)
                (anki-editor-mode +1)
                (my-anki-mode +1)))))

(add-to-list 'auto-mode-alist '("\\.anki\\'" . org-mode))

(use-package magit
  :bind
  (("C-c g" . magit-file-dispatch)))

(use-package forge
  :after magit
  :config
  (add-to-list 'forge-alist '("source.xing.com" "source.xing.com/api" "source.xing.com" forge-github-repository)))

(use-package project
  :straight nil
  :config
  (add-to-list 'project-switch-commands '(project-dired "Dired" "D") t)
  (add-to-list 'project-switch-commands '(my-vterm-project-root "vterm" "V") t))

(use-package vertico
  :config
  (vertico-mode))

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :init
  (setq consult-narrow-key "<")
  :bind
  (("C-c M" . #'consult-man)
   ("C-c I" . #'consult-info)
   ("C-x b" . #'consult-buffer)
   ("C-x 4 b" . #'consult-buffer-other-window)
   ("C-x 5 b" . #'consult-buffer-other-frame)
   ("C-x r b" . #'consult-bookmark)
   ("C-x p b" . #'consult-project-buffer)
   ("M-g M-g" . #'consult-goto-line)
   ("M-g o" . #'consult-outline)
   ("M-g m" . #'consult-mark)
   ("M-g M" . #'consult-global-mark)
   ("M-g i" . #'consult-imenu)
   ("M-g I" . #'consult-imenu-multi)
   ("M-s d" . #'consult-find)
   ("M-s g" . #'consult-grep)
   ("M-s G" . #'consult-git-grep)
   ("M-s r" . #'consult-ripgrep)
   ("M-s l" . #'consult-line)
   ("M-s L" . #'consult-line-multi)
   ("M-s k" . #'consult-keep-lines)
   ("M-s u" . #'consult-focus-lines)
   ("M-s e" . #'consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . #'consult-isearch-history)
   ("M-s e" . #'consult-isearch-history)
   ("M-s l" . #'consult-line)
   ("M-s L" . #'consult-line-multi)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package company
  :init
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t)
  :config
  (global-company-mode))

(use-package whole-line-or-region
  :demand
  :config
  (whole-line-or-region-global-mode)
  :bind
  (("M-/" . #'whole-line-or-region-comment-dwim)))

(defun my-goto-charseq-end ()
  (let ((line-end (save-excursion (end-of-line) (point))))
    (condition-case nil
        (progn
          (re-search-forward (rx (or whitespace "(" ")" "[" "]" "{" "}" "\"" "'" "`" ";" "," "=" "|")) line-end)
          (backward-char))
      (error (end-of-line)))))

(defun my-copy-charseq ()
  (interactive)
  (set-mark-command nil)
  (my-goto-charseq-end)
  (setq last-command nil) ;; never append to the last kill
  (copy-region-as-kill nil nil t))

(defun my-avy-action-copy-charseq (point)
  (save-excursion
    (goto-char point)
    (my-copy-charseq))
  (select-window (cdr (ring-ref avy-ring 0)))
  t)

(defun my-avy-yank ()
  (if (derived-mode-p 'vterm-mode)
      (vterm-yank)
    (yank)))

(defun my-avy-action-yank-charseq (point)
  (save-excursion
    (goto-char point)
    (my-copy-charseq))
  (select-window (cdr (ring-ref avy-ring 0)))
  (my-avy-yank)
  t)

(defun my-avy-action-yank-line (point)
  (save-excursion
    (goto-char point)
    (set-mark-command nil)
    (end-of-line)
    (setq last-command nil) ;; never append to the last kill
    (copy-region-as-kill nil nil t))
  (select-window (cdr (ring-ref avy-ring 0)))
  (my-avy-yank)
  t)

(defun my-avy-action-embark (point)
  (unwind-protect
    (goto-char point)
    (embark-act))
  t)

(defun my-avy-action-embark-dwim (point)
  (unwind-protect
    (goto-char point)
    (embark-dwim))
  t)

(use-package avy
  :init
  (setq avy-single-candidate-jump t)
  :config
  (setf (alist-get ?n avy-dispatch-alist) #'my-avy-action-copy-charseq)
  (setf (alist-get ?y avy-dispatch-alist) #'my-avy-action-yank-charseq)
  (setf (alist-get ?Y avy-dispatch-alist) #'my-avy-action-yank-line)
  (setf (alist-get ?. avy-dispatch-alist) #'my-avy-action-embark)
  (setf (alist-get ?\; avy-dispatch-alist) #'my-avy-action-embark-dwim)
  :bind
  (("C-;" . avy-goto-char-timer)
   ("M-;" . avy-pop-mark)
   ("M-g g" . avy-goto-line)
   ("M-g G" . avy-goto-end-of-line)
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

(use-package multiple-cursors
  :config
  (define-key mc/keymap (kbd "<return>") nil)
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

(use-package crux
  :bind
  (("C-o" . crux-smart-open-line)
   ("C-S-o" . crux-smart-open-line-above)
   ("C-^" . crux-top-join-line)
   ("C-M-; D" . crux-duplicate-and-comment-current-line-or-region)))

;; https://github.com/purcell/whole-line-or-region/issues/17#issuecomment-781988534
(defun my-whole-line-or-region-sp-kill-region (prefix)
  "Call `sp-kill-region' on region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-wrap-beg-end 'sp-kill-region prefix))

(use-package smartparens
  :demand
  :after whole-line-or-region
  :init
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'smartparens-mode)
  (add-hook 'scala-mode-hook 'smartparens-mode)
  (add-hook 'js-mode-hook 'smartparens-mode)
  (add-hook 'plantuml-mode-hook 'smartparens-mode)
  (add-hook 'json-ts-mode-hook 'smartparens-mode)
  (add-hook 'terraform-mode-hook 'smartparens-mode)
  (add-hook 'graphql-mode-hook 'smartparens-mode)
  (add-hook 'yaml-mode-hook 'smartparens-mode)
  :config
  (require 'smartparens-config)
  :bind
  (:map smartparens-strict-mode-map
        ("C-<right>" . sp-forward-slurp-sexp)
        ("C-<left>" . sp-backward-slurp-sexp)
        ("M-<right>" . sp-forward-barf-sexp)
        ("M-<left>" . sp-backward-barf-sexp)
        ("C-c p u" . sp-unwrap-sexp)
        ("C-c p {" . sp-wrap-curly)
        ("C-c p (" . sp-wrap-round)
        ("C-c p [" . sp-wrap-square)
        ("C-c p r" . sp-rewrap-sexp)
        ("M-a" . sp-beginning-of-sexp)
        ("M-e" . sp-end-of-sexp)
        ("C-M-u" . sp-up-sexp)
        ("C-M-S-u" . sp-backward-up-sexp)
        ("C-M-S-d" . sp-backward-down-sexp)
        ("C-w" . my-whole-line-or-region-sp-kill-region)
        :map smartparens-mode-map
        ("C-<right>" . sp-forward-slurp-sexp)
        ("C-<left>" . sp-backward-slurp-sexp)
        ("M-<right>" . sp-forward-barf-sexp)
        ("M-<left>" . sp-backward-barf-sexp)
        ("C-c p u" . sp-unwrap-sexp)
        ("C-c p u" . sp-unwrap-sexp)
        ("C-c p {" . sp-wrap-curly)
        ("C-c p (" . sp-wrap-round)
        ("C-c p [" . sp-wrap-square)
        ("C-c p r" . sp-rewrap-sexp)
        ("M-a" . sp-beginning-of-sexp)
        ("M-e" . sp-end-of-sexp)
        ("C-M-u" . sp-up-sexp)
        ("C-M-S-u" . sp-backward-up-sexp)
        ("C-M-S-d" . sp-backward-down-sexp)))

(use-package ace-window
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame)
  :bind
  (("M-o" . ace-window)))

(defun my-save-all-buffers ()
  (interactive)
  (save-some-buffers t))

(global-set-key (kbd "C-c S") 'my-save-all-buffers)

(tab-bar-history-mode +1)

(global-set-key (kbd "C-c s") 'scratch-buffer)

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

(use-package embark-consult
  :after (embark consult))

(use-package rg
  :bind
  (("M-s R" . rg-project)))

(use-package scala-mode
  :interpreter "scala")

(use-package clojure-mode)

(use-package flycheck
  :init
  (setq flycheck-global-modes '(not org-mode))
  :config
  (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "<f5>")
  :hook
  (scala-mode . lsp)
  :commands lsp
  :bind
  (:map lsp-mode-map
        ([M-down-mouse-1] . mouse-set-point)
        ([M-mouse-1] . lsp-find-definition)
        ([M-mouse-3] . xref-go-back)
        ("<f5> I" . lsp-metals-build-import)))

(use-package lsp-metals)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package consult-lsp
  :bind
  (:map lsp-mode-map
        ("<f5> d" . consult-lsp-diagnostics)
        ("<f5> s" . consult-lsp-file-symbols)
        ("<f5> S" . consult-lsp-symbols)))

(use-package kubel
  :after vterm
  :config
  (kubel-vterm-setup)
  (advice-add 'kubel-exec-vterm-pod :before (lambda () (setq vterm-shell "/bin/bash")))
  (advice-add 'kubel-exec-vterm-pod :after (lambda () (setq vterm-shell my/fish-path)))
  :bind
  (("C-c K" . kubel)
   :map kubel-mode-map
   ("n" . next-line)
   ("p" . previous-line)
   ("N" . kubel-set-namespace)
   ("P" . kubel-port-forward-pod)
   ("s" . tabulated-list-sort)
   ;("e" . kubel-exec-ansi-term-pod)
   ))

(with-eval-after-load 'smerge-mode
  (global-set-key (kbd "C-c ^ N") 'smerge-vc-next-conflict))

(defun my-relax-smartparens ()
  "Switch from smartparens-strict-mode to smartparens-mode when copilot-mode is enabled."
  (when (and copilot-mode smartparens-strict-mode)
      (progn
        (smartparens-strict-mode -1)
        (smartparens-mode 1))))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :config
  (add-hook 'emacs-lisp-mode-hook #'copilot-mode)
  (add-hook 'clojure-mode-hook #'copilot-mode)
  (add-hook 'scala-mode-hook #'copilot-mode)
  (add-hook 'conf-mode-hook #'copilot-mode)
  (add-hook 'sql-mode-hook #'copilot-mode)
  (add-hook 'plantuml-mode-hook #'copilot-mode)
  (add-hook 'copilot-mode-hook #'my-relax-smartparens))

(with-eval-after-load 'copilot
  (defhydra hydra-copilot (copilot-mode-map "M-TAB")
    "Copilot"
    ("M-TAB" copilot-accept-completion "Accept" :color blue)
    ("M-f" copilot-accept-completion-by-word "Accept by word")
    ("M-e" copilot-accept-completion-by-line "Accept by line")
    ("M-a" copilot-accept-completion-by-paragraph "Accept by paragraph")))

(use-package gptai
  :init
  (setq gptai-username my-openai-username)
  (setq gptai-api-key my-openai-api-key)
  (setq gptai-model "gpt-3.5-turbo-0301"))

(use-package jenkinsfile-mode)

(use-package fish-mode)

(use-package mermaid-mode
  :init
  (setq mermaid-output-format ".svg")
  (setq mermaid-mmdc-location "docker")
  (setq mermaid-flags "run -u 1000 -v /tmp:/tmp ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:10.1.0"))

(use-package hcl-mode)

(use-package ledger-mode
  :init
  (setq ledger-binary-path "ledger")
  (setq ledger-default-date-format "%Y-%m-%d")
  :config
  (ledger-reports-add "bal-this-month" "%(binary) -f %(ledger-file) --invert --period \"this month\" -S amount bal ^Income ^Expenses")
  (ledger-reports-add "bal-last-month" "%(binary) -f %(ledger-file) --invert --period \"last month\" -S amount bal ^Income ^Expenses"))

(if (bound-and-true-p my-work-machine-p)
    (use-package xingbox
      :straight (xingbox :type git :repo "git@source.xing.com:hackweek/xingbox.el")))

(use-package cloc)

(use-package plantuml-mode
  :init
  (setq plantuml-default-exec-mode 'jar))

(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

(add-to-list 'auto-mode-alist '("\\.hql\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.ejson\\'" . json-ts-mode))

(use-package treesit-auto
  :demand t
  :config
  (global-treesit-auto-mode))

(defun arttsu-format-json-string (start end)
  "Format the JSON string between START and END and display in a temporary buffer."
  (interactive "r")
  (let* ((json-string (buffer-substring-no-properties start end))
         (command (format "echo %s | jq fromjson" (shell-quote-argument json-string)))
         (output (shell-command-to-string command))
         (buf-name "*Formatted JSON*"))
    (with-current-buffer (get-buffer-create buf-name)
      (erase-buffer)
      (insert output)
      (json-ts-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(use-package terraform-mode
  :bind
  (:map terraform-mode-map
        ("<f5> = =" . terraform-format-buffer)))

(use-package graphql-mode)

(defun arttsu-clear-chat ()
  (interactive)
  (erase-buffer)
  (insert "*** "))

(defun arttsu-save-chat ()
  (interactive)
  (let* ((raw-chat-name (read-string "Enter chat name: "))
         (normalized-chat-name (replace-regexp-in-string "[^[:alnum:]]+" "-" raw-chat-name))
         (final-chat-name (if (string= normalized-chat-name "")
                              (md5 (format-time-string "%s"))
                            normalized-chat-name))
         (file-path (expand-file-name (concat "~/org/chats/" final-chat-name ".org"))))
    (if (file-exists-p file-path)
        (message "File already exists!")
        (progn
          (write-region (point-min) (point-max) file-path)
          (set-visited-file-name file-path)
          (set-buffer-modified-p nil)
          (message (concat "Chat saved to: " file-path))))))

(defun arttsu-open-chat ()
  (interactive)
  (let* ((chat-dir "~/org/chats")
         (chats (directory-files chat-dir nil "\\.org$"))
         (chat-name (completing-read "Open chat: " chats nil t)))
    (find-file (expand-file-name chat-name chat-dir))))

(defun arttsu-open-in-gptel-mode ()
  "Open content of the current buffer in org-mode with '*** ' inserted on first line. Cursor positioned after '*** '."
  (interactive)
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
         (buffer-name (format "ChatGPT-%s" (make-temp-name ""))))
    (switch-to-buffer (generate-new-buffer buffer-name))
    (insert "*** ")
    (save-excursion
      (insert "\n" content))
    (org-mode)
    (gptel-mode)))

(defun arttsu-ask-chatgpt ()
   "Ask to select a prompt and then open a new buffer in org-mode with '*** '
    inserted on first line followed by the selected prompt.
    Cursor positioned at the end of the buffer after content and gptel-send is executed."
   (interactive)
   (let* ((prompt-options '("Critique:" "Critique my Anki cards:"))
          (selected-prompt (completing-read "Choose a prompt: " prompt-options nil t))
          (content (buffer-substring-no-properties (point-min) (point-max)))
          (buffer-name (format "ChatGPT-%s" (make-temp-name ""))))
     (switch-to-buffer (generate-new-buffer buffer-name))
     (insert "*** " selected-prompt)
     (save-excursion
       (insert "\n" content))
     (org-mode)
     (gptel-mode)
     (goto-char (point-max))
     (gptel-send)))

(defun arttsu-gptel-send ()
  (interactive)
  (goto-char (point-max))
  (gptel-send))

(use-package gptel
  :init
  (setq-default gptel-model "gpt-4")
  (setq gptel-default-mode 'org-mode)
  :bind
  (("C-c SPC" . gptel)
   :map gptel-mode-map
   (("C-c C-c" . arttsu-gptel-send)
    ("C-c N" . arttsu-clear-chat)
    ("C-c S" . arttsu-save-chat))))

(use-package epresent)

(use-package org-modern
  :after org
  :config
  (global-org-modern-mode))

(use-package link-hint
  :bind
  ("C-c o" . link-hint-open-link)
  ("C-c O" . link-hint-copy-link))
