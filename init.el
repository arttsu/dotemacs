(defun my-mac-p ()
  (equal system-type 'darwin))

(defun my-windows-p ()
  (equal system-type 'windows-nt))

(defun my-current-buffer-or-region-substring ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun my-unix-time (iso-date)
  "Convert ISO date-time string to Unix timestamp."
  (let ((decoded-time (parse-iso8601-time-string iso-date)))
    (time-to-seconds decoded-time)))

(defun my-iso-time (unix-timestamp)
  "Convert Unix timestamp to ISO date-time string."
  (format-time-string "%FT%T%z" (seconds-to-time unix-timestamp)))

(defun my/seconds-to-days (seconds)
  (interactive
   (list (read-number "Seconds: ")))
  (let ((days (/ seconds 60.0 60 24)))
    (if (interactive-p)
        (message "%s seconds is %s days" seconds days)
      days)))

(defun my/milliseconds-to-days (millis)
  (interactive
   (list (read-number "Milliseconds: ")))
  (let ((days (/ (my/seconds-to-days millis) 1000.0)))
    (if (interactive-p)
        (message "%s milliseconds is %s days" millis days)
      days)))

(defun my/convert-time (number unit)
  "Convert time between milliseconds, seconds, minutes, hours, days, months, and years."
  (interactive
   (list (read-number "Enter number: ")
         (completing-read "Unit (milliseconds, seconds, minutes, hours, days, months, years): "
                          '("milliseconds" "seconds" "minutes" "hours" "days" "months" "years"))))
  (let ((in-seconds (cond
                     ((string= unit "milliseconds") (/ number 1000.0))
                     ((string= unit "seconds") number)
                     ((string= unit "minutes") (* number 60))
                     ((string= unit "hours") (* number 60 60))
                     ((string= unit "days") (* number 60 60 24))
                     ((string= unit "months") (* number 60 60 24 30)) ;; Approximation
                     ((string= unit "years") (* number 60 60 24 365))))) ;; Approximation
    (let ((milliseconds (* in-seconds 1000))
          (seconds in-seconds)
          (minutes (/ in-seconds 60))
          (hours (/ in-seconds 60 60))
          (days (/ in-seconds 60 60 24))
          (months (/ in-seconds 60 60 24 30)) ;; Approximation
          (years (/ in-seconds 60 60 24 365))) ;; Approximation
      (cond
       ((member unit '("milliseconds" "seconds"))
        (kill-new (number-to-string days))
        (message "Copied %s days to clipboard." days))
       (t
        (kill-new (number-to-string milliseconds))
        (message "Copied %s milliseconds to clipboard." milliseconds)))
      (if (interactive-p)
          (message "Milliseconds: %s, Seconds: %s, Minutes: %s, Hours: %s, Days: %s, Months: %s, Years: %s"
                   milliseconds seconds minutes hours days months years)
        (list milliseconds seconds minutes hours days months years)))))

(load (expand-file-name "local.el" user-emacs-directory))

(defun my-kill-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))

(defun my-kill-to-beginning-of-buffer ()
  (interactive)
  (kill-region (point-min) (point)))

(defun my-kill-to-word ()
  (interactive)
  (kill-region (point) (save-excursion (forward-to-word 1) (point))))

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
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (zoneinfo-style-world-list '(("America/Los_Angeles" "LA")
                               ("America/New_York" "NYC")
                               ("Europe/London" "London")
                               ("Europe/Lisbon" "Porto")
                               ("Europe/Berlin" "Berlin")
                               ("Europe/Kyiv" "Kyiv")))
  :config
  (when (file-exists-p custom-file)
    (load custom-file))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (global-auto-revert-mode)
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
   ("C-c x x" . erase-buffer)
   ("C-c x e" . my-kill-to-end-of-buffer)
   ("C-c x E" . my-kill-to-beginning-of-buffer)
   ("C-c x w" . my-kill-to-word)
   ("C-c x b" . bury-buffer)
   ("C-c x u" . unbury-buffer)
   ("C-c x k" . kill-current-buffer)
   ("C-c x K" . kill-buffer-and-window)
   ("M-z" . zap-up-to-char)
   ("M-Z" . zap-to-char)
   ("<f8> h" . tab-bar-history-back)
   ("<f8> l" . tab-bar-history-forward)
   ("<f8> H" . previous-buffer)
   ("<f8> L" . next-buffer)))

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

(use-package exec-path-from-shell
  :if (my-mac-p)
  :config
  (exec-path-from-shell-initialize))

(use-package magit
  :bind
  (("C-c g" . magit-file-dispatch)))

(use-package org-ql)

(defun my-capture-template (name)
  (expand-file-name (format "capture_templates/%s.txt" name) user-emacs-directory))

(defvar my-capture-prompt-history nil)

(defun my-capture-prompt (prompt var)
  (make-local-variable var)
  (set var (read-string (concat prompt ": ") nil my-capture-prompt-history)))

(defun my-gtd-file (name)
  (format "~/ordo/gtd/%s.org" name))

(defconst my-inbox-file (my-gtd-file "inbox"))
(defconst my-calendar-file (my-gtd-file "calendar"))
(defconst my-tasks-file (my-gtd-file "tasks"))
(defconst my-projects-file (my-gtd-file "projects"))
(defconst my-someday-file (my-gtd-file "someday"))

(defconst my-shared-inbox-file (expand-file-name "inbox.org" my/share-directory))

(defconst my-gtd-agenda-files (list my-calendar-file
                                    my-tasks-file
                                    my-projects-file))

(defconst my-inbox-files (list my-inbox-file my-shared-inbox-file))

(defconst my-links-file "~/ordo/links.org")

(defun my/avy-act-on-src-block (action)
  (save-excursion
    (let ((start-point (point)))
      (my/avy-jump-to-src-block)
      (if (equal start-point (point))
          (message "Haven't moved; doing nothing")
        (funcall action)))))

(defun my/avy-exec-src-block ()
  (interactive)
  (my/avy-act-on-src-block 'org-babel-execute-src-block))

(defun my/avy-edit-src-block ()
  (interactive)
  (my/avy-act-on-src-block 'org-edit-special))

(defconst my-gtd-capture-templates
  `(("i" "Inbox")
    ("ii" "Todo" entry (file+headline ,my-inbox-file "Inbox") "* TODO %?")
    ("il" "Link" entry (file+headline ,my-inbox-file "Inbox") "* [[%c][%^{Description}]]%?")
    ("it" "Log" entry (file+headline ,my-inbox-file "Inbox") "* %u %?")
    ("ia" "Annotation" entry (file+headline ,my-inbox-file "Inbox") "* %A%?")
    ("f" "Folder")
    ("fl" "Links" entry (file ,my-links-file) "* %^{Title}")
    ("fp" "Project" entry (file ,my-projects-file) (file ,(my-capture-template "project")))
    ("fs" "Someday Area" entry (file ,my-someday-file) (file ,(my-capture-template "someday_area")))))

(defun my/capture-to-inbox (&optional prefix)
  (interactive "P")
  (cond
   ((equal prefix nil)   (org-capture nil "ii"))
   ((equal prefix '(4))  (org-capture nil "il"))
   ((equal prefix '(16)) (org-capture nil "it"))
   (t (message "Prefix '%s' not supported" prefix))))

(defun my/annotate ()
  (interactive)
  (org-store-link nil)
  (org-capture nil "ia"))

(defconst my-day-agenda
  `("d"
    "Day"
    ((agenda "" ((org-agenda-span 1)
                 (org-agenda-skip-scheduled-if-done t)
                 (org-agenda-skip-deadline-if-done t)
                 (org-agenda-skip-timestamp-if-done t)
                 (org-agenda-files ',my-gtd-agenda-files)))
     (todo "TODO" ((org-agenda-overriding-header "Not-scheduled Tasks")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
                   (org-agenda-files '(,my-tasks-file))))
     (tags "NOW+LEVEL=1" ((org-agenda-overriding-header "Projects")
                          (org-agenda-files '(,my-projects-file)))))))

(defconst my-gtd-refile-targets
  `(((,my-calendar-file
      ,my-tasks-file
      ,my-projects-file
      ,my-someday-file
      ,my-inbox-file
      ,my-shared-inbox-file
      ,my-links-file)
     :level . 1)))

(defun my-gtd-projects ()
  (org-ql-query
    :select '(cons (substring-no-properties (org-get-heading t t t t))
                   (org-id-get-create))
    :from my-projects-file
    :where '(level 1)))

(defun my-gtd-project-map ()
  (let ((heading-map (make-hash-table :test 'equal))
        (headings (my-gtd-projects)))
    (dolist (heading headings)
      (puthash (car heading) (cdr heading) heading-map))
    heading-map))

(defun my-narrow-to-project ()
  (org-narrow-to-subtree)
  (org-cycle '(16)))

(defun my-jump-to-gtd-project ()
  (interactive)
  (let* ((project-map (my-gtd-project-map))
         (selected-title (completing-read "Project: " project-map))
         (selected-id (gethash selected-title project-map)))
    (if-let (win (get-buffer-window (get-file-buffer my-projects-file) t))
        (select-window win)
      (switch-to-buffer-other-window my-projects-file))
    (widen)
    (org-id-goto selected-id)
    (my-narrow-to-project)))

(straight-use-package 'org)

(use-package org
  :defer t
  :custom
  (org-agenda-files my-gtd-agenda-files)
  (org-confirm-babel-evaluate nil)
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-use-sub-superscripts '{})
  (org-capture-templates my-gtd-capture-templates)
  (org-agenda-custom-commands (list my-day-agenda))
  (org-refile-targets my-gtd-refile-targets)
  (org-attach-directory "~/ordo/attach")
  (org-attach-use-inheritance t)
  :config
  (require 'org-attach)
  (add-to-list 'org-export-backends 'md)
  :bind
  (("C-c c" . org-capture)
   ("C-c i" . my/capture-to-inbox)
   ("C-c a" . org-agenda)
   ("C-c A" . my/annotate)
   ("C-c j p" . my-jump-to-gtd-project)
   ("C-c l" . org-store-link)
   :map org-mode-map
   ("C-c v ;" . my/avy-jump-to-src-block)
   ("C-c v <return>" . my/avy-exec-src-block)
   ("C-c v '" . my/avy-edit-src-block)))

(use-package org-modern
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")
  (org-agenda-tags-column 0)
  :config
  (global-org-modern-mode))

(use-package super-save
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-exclude '(".sbt" "project/"))
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  (super-save-all-buffers t)
  (auto-save-default nil)
  :config
  (super-save-mode)
  (add-to-list 'super-save-triggers 'ace-window))

(use-package dired
  :straight nil
  :custom
  (dired-dwim-target t)
  :bind
  (("<f7>" . dired-jump)
   :map dired-mode-map
   ("o" . crux-open-with)
   ("<tab>" . dired-find-file-other-window)))

(use-package project
  :config
  (add-to-list 'project-switch-commands '(my-vterm-project-root "vterm" "V") t)
  (add-to-list 'project-switch-commands '(project-dired "Dired" "<return>") t))

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

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
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
        ("C-c p {" . sp-wrap-curly)
        ("C-c p [" . sp-wrap-square)
        ("C-c p (" . sp-wrap-round)
        ("C-c p r" . sp-rewrap-sexp)))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (aw-scope 'global)
  :bind
  (("M-o" . ace-window)))

(use-package vertico
  :config
  (vertico-mode))

(use-package vertico-posframe
  :after vertico
  :config
  (vertico-posframe-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(defun my/avy-jump-to-src-block ()
  "Jump to a visible src block using Avy."
  (interactive)
  (require 'avy)
  (avy-with avy-jump-to-src-block
    (avy-jump
     (rx (and (0+ any) "#+begin_src" (1+ (or space "\n"))))
     :window-flip nil
     :beg (window-start)
     :end (window-end))))

(defun my-avy-action-embark (point)
  (unwind-protect
    (goto-char point)
    (embark-act)
    (avy--done)))

(defun my-avy-action-embark-dwim (point)
  (unwind-protect
    (goto-char point)
    (embark-dwim)
    (avy--done)))

(use-package avy
  :custom
  (avy-single-candidate-jump t)
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'my-avy-action-embark)
  (setf (alist-get ?\; avy-dispatch-alist) 'my-avy-action-embark-dwim)
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

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x p b" . consult-project-buffer)
   ("C-x B" . consult-bookmark)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g M" . consult-global-mark)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s f" . consult-focus-lines)))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

(use-package embark-consult
  :after (embark consult))

(use-package whole-line-or-region
  :demand
  :config
  (whole-line-or-region-global-mode)
  :bind
  (("M-/" . whole-line-or-region-comment-dwim)))

(use-package crux
  :bind
  (("C-o" . crux-smart-open-line)
   ("C-S-o" . crux-smart-open-line-above)
   ("C-^" . crux-top-join-line)
   ("C-M-; D" . crux-duplicate-and-comment-current-line-or-region)))

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
  (ruby-mode . lsp)
  :commands lsp
  :bind
  (:map lsp-mode-map
        ([M-down-mouse-1] . mouse-set-point)
        ([M-mouse-1] . lsp-find-definition)
        ([M-mouse-3] . xref-go-back)))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-metals
  :after (lsp-mode scala)
  :bind
  (:map scala-mode-map
        ("<f5> I" . lsp-metals-build-import)))

(use-package consult-lsp
  :commands (consult-lsp-diagnostics consult-lsp-file-symbols consult-lsp-symbols)
  :bind
  (:map lsp-mode-map
        ("<f5> d" . consult-lsp-diagnostics)
        ("<f5> s" . consult-lsp-file-symbols)
        ("<f5> S" . consult-lsp-symbols)))

(use-package flycheck
  :custom
  (flycheck-global-modes '(not org-mode))
  :config
  (global-flycheck-mode))

(use-package rg
  :bind
  (("M-s R" . rg-project)))

(defun my-vterm-project-root ()
  (interactive)
  (let* ((project-root (when-let ((project (project-current)))
                         (project-root project)))
         (project-name (when project-root
                          (file-name-nondirectory
                           (directory-file-name project-root))))
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
  (local-unset-key (kbd "M-s"))
  (local-unset-key (kbd "<f8>")))

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
   ("C-x p v" . my-vterm-project-root)
   :map vterm-mode-map
   ("<f4>" . rename-buffer)))

(defun my-gptel-clear ()
  (interactive)
  (erase-buffer)
  (insert "*** "))

(defconst my-gptel-ignore-list
  '("\\:PROPERTIES\\:"
    "\\#\\+STARTUP\\:"
    "\\:END\\:"
    "\\:ANKI_"
    "\\:ID\\:"))

(defun my-gptel-extract-prompt-boundary (directive-text)
  (if (string-match "provided between '\\(.*\\)'" directive-text)
      (match-string 1 directive-text)
    "^^^^^"))

(defun my-gptel-send ()
  (interactive)
  (let* ((directive (intern (completing-read "Select directive: " gptel-directives nil t)))
         (directive-text (alist-get directive gptel-directives))
         (prompt-boundary (my-gptel-extract-prompt-boundary directive-text))
         (directive-addition (read-string "Directive addition: "))
         (system-message (format "%s\n\n%s\n\n%s\n" directive-text directive-addition prompt-boundary))
         (content (my-current-buffer-or-region-substring))
         (temp-buffer (generate-new-buffer "*gptel-temp*")))
    (with-current-buffer temp-buffer
      (text-mode)
      (gptel-mode)
      (setq gptel--system-message system-message)
      (insert content)
      (insert "\n\n")
      (insert prompt-boundary)
      (dolist (regexp my-gptel-ignore-list)
        (flush-lines regexp (point-min) (point-max)))
      (end-of-buffer)
      (gptel-send))
    (pop-to-buffer temp-buffer)))

(defconst my-anki-prompt "You are an Anki expert. Please CRITIQUE the Anki cards provided between '^^^^^'.

Expect the cards to be in the org-mode format, similar to:
```
* TOPIC NAME
** Card 1
*** Front
Question
*** Back
...
```

Check the cards for CORRECTNESS and TYPOS. Suggest if something can be REWORDED in a better way.

Keep in mind the rules of formulating knowledge:
- Build upon the basics
- Minimum information principle
- Use mnemonic techniques
- Avoid sets
- Avoid enumerations
- Optimize wording
- Personalize and provide examples
- Rely on emotional states
- Context cues simplify wording
- Redundancy does not contradict minimum information principle
- Provide sources
- Provide date stamping - time stamping is useful for volatile knowledge that changes in time.

Assume that the cards cover a SINGLE TOPIC. If you think that the given cards don't cover the topic in full, suggest ADDITIONS.

IGNORE ATTACHMENTS.")

(defconst my-prompt-eng-prompt "You are an expert in creating prompts for LARGE LANGUAGE MODELS. Please CRITIQUE the prompt provided between '$$$$$'. The prompt is intended for GPT-4 as a system message. Assume that the user input is expected to be provided later.")

(defconst my-tech-team-msg-prompt "You are a data engineer in a large organization, that comunicates clearly, avoiding prentious and \"corpo-speak\". Please REVIEW and CRITIQUE the message to another team within the organization provided between '^^^^^'. The message should be clear and consise. It's a message in Slack, so it doesn't have to follow the typical email structure.")

(define-derived-mode my-gpt-mode org-mode "GPT")

(add-to-list 'auto-mode-alist '("\\.gpt\\'" . my-gpt-mode))

(defun my-switch-to-gptel-mode ()
  (interactive)
  (org-mode)
  (gptel-mode))

(use-package gptel
  :custom
  (gptel-model "gpt-4-turbo-preview")
  (gptel-default-mode 'org-mode)
  :config
  (push (cons 'anki my-anki-prompt) gptel-directives)
  (push (cons 'prompt-eng my-prompt-eng-prompt) gptel-directives)
  (push (cons 'tech-team-msg my-tech-team-msg-prompt) gptel-directives)
  :hook ((my-gpt-mode . my-switch-to-gptel-mode))
  :bind
  (("C-c SPC" . gptel)
   :map gptel-mode-map
   ("C-c x x" . my-gptel-clear)
   ("C-c x a" . gptel-abort)
   ("C-c C-c" . gptel-send)
   :map text-mode-map
   ("C-c <return>" . gptel-send)))

(use-package ob-restclient)

(use-package hydra)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :custom
  (copilot-idle-delay 0.5)
  :config
  (add-to-list 'warning-suppress-log-types '(copilot copilot-no-mode-indent))
  (add-to-list 'copilot-disable-predicates 'company-tooltip-visible-p)
  (defhydra my-copilot-hydra (copilot-mode-map "C-<tab>")
    "Copilot"
    ("C-<tab>" copilot-accept-completion "Accept" :color blue)
    ("M-f" copilot-accept-completion-by-word "Word")
    ("C-e" copilot-accept-completion-by-line "Line"))
  :hook (prog-mode . copilot-mode))

(use-package js
  :bind
  (:map js-json-mode-map
        ("<f5> = =" . json-pretty-print-buffer)))

(define-derived-mode anki-mode org-mode "Anki")

(add-to-list 'auto-mode-alist '("\\.anki\\'" . anki-mode))

(defun my-org-attach-with-anki-editor-tag-completion-disabled ()
  (interactive)
  (let ((anki-editor-org-tags-as-anki-tags nil))
    (call-interactively 'org-attach)))

(defun my-org-attach-disable-inheritance ()
  (setq-local org-attach-use-inheritance nil))

(use-package anki-editor
  :hook ((anki-mode . anki-editor-mode)
         (anki-mode . my-org-attach-disable-inheritance))
  :bind
  (:map anki-mode-map
        ("C-<return>" . anki-editor-insert-note)
        ("C-c p" . anki-editor-push-notes)
        ("C-c r" . anki-editor-retry-failure-notes)
        ("C-c C-a" . my-org-attach-with-anki-editor-tag-completion-disabled)))

(use-package kubel
  :bind
  (("C-c K" . kubel)
   :map kubel-mode-map
   ("n" . next-line)
   ("p" . previous-line)
   ("N" . kubel-set-namespace)))

(use-package ox-slack
  :commands (org-slack-export-to-clipboard-as-slack org-slack-export-as-slack org-slack-export-to-slack))

(use-package link-hint
  :bind
  (("C-c f" . link-hint-open-link)
   ("C-c y" . link-hint-copy-link)))

(use-package emms
  :defer t
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-player-mpv-update-metadata t)
  (emms-streams-file (expand-file-name "streams.emms" user-emacs-directory))
  :config
  (emms-all)
  :bind
  (("C-c r r" . 'emms-streams)
   ("C-c r p" . 'emms-pause)
   ("C-c r s" . 'emms-stop)))

(defun my-grab-java-package-name ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^package \\(.*\\)" nil t)
        (let* ((package-path (match-string 1))
               (package-components (split-string package-path "\\.")))
          (car (last package-components)))
      (error "Package declaration not found"))))

(use-package yasnippet
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-reload-all)
  :hook
  (prog-mode . yas-minor-mode)
  (org-mode . yas-minor-mode))
