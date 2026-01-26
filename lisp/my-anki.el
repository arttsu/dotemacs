;;; my-anki.el --- Custom Anki Mode and Helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(define-derived-mode my-anki-mode org-mode "Anki")

(add-to-list 'auto-mode-alist '("\\.anki\\'" . my-anki-mode))

(defun my-anki-mode-init ()
  "Initialize my-anki-mode in a new buffer."
  ;; Anki Editor requires Org mode.
  (let ((major-mode 'org-mode))
    (anki-editor-mode +1)))

(defun my-anki-region-no-clozes (start end)
  "Return region text with all clozes replaced by their answers.

START and END are region bounds.

If called interactively, copy the text to the kill ring instead."
  (interactive "r")
  (let* ((raw-text (buffer-substring-no-properties start end))
         (clean-text (replace-regexp-in-string
                      (rx
                      "{{"
                      "c" (one-or-more digit) "::" ; Cloze number
                      (group                       ; The answer
                       (minimal-match
                        (zero-or-more any)))
                      (optional                    ; Optional hint section
                       "::"
                       (minimal-match
                        (zero-or-more any)))
                      "}}")
                     "\\1" ; Replace match with group 1 (the answer)
                     raw-text)))
    (if (called-interactively-p 'interactive)
        (progn
          (kill-new clean-text)
          (message "Copied text without clozes"))
      clean-text)))

(defconst my-anki-voices '(("German" . ("de-DE-KatjaNeural"
                                        "de-DE-AmalaNeural"
                                        "de-DE-ConradNeural"
                                        "de-DE-KillianNeural"))
                           ("Spanish" . ("es-ES-ElviraNeural"
                                         "es-ES-AbrilNeural"
                                         "es-ES-AlvaroNeural"
                                         "es-ES-ArnauNeural"
                                         "es-MX-DaliaNeural"
                                         "es-MX-JorgeNeural"))))

(defun my-anki-random-voice (language)
  "Return random voice from the list defined in 'my-anki-voices' for LANGUAGE."
  (if-let ((voices (cdr (assoc language my-anki-voices))))
      (nth (random (length voices)) voices)
    (user-error "Unknown language: %s" language)))

(defun my-anki-cloze-generate-audio ()
  "Generate audio for a Cloze note at point."
  (interactive)
  (let ((language (org-entry-get nil "ANKI_LANG" t))
        (note-type (org-entry-get nil "ANKI_NOTE_TYPE"))
        (note-title (org-entry-get nil "ITEM")))
    (unless language (user-error "ANKI_LANG property is missing"))
    (unless (string= note-type "Cloze") ("ANKI_NOTE_TYPE must be 'Cloze'"))
    (let* ((dir (org-attach-dir-get-create))
           (file (expand-file-name "audio.mp3" dir))
           (voice (my-anki-random-voice language)))
      (org-node-add-tags-here '("ATTACH"))
      (org-set-property "ROAM_EXCLUDE" "t")
      (save-excursion
        (save-restriction
          (org-narrow-to-subtree)
          (re-search-forward (rx (>= 1 "*") (+ blank) "Text" line-end))
          (let ((text-start (point)))
            (re-search-forward (rx (>= 1 "*") (+ blank) "Back Extra" line-end))
            (beginning-of-line)
            (let* ((text (my-anki-region-no-clozes text-start (point)))
                   (shell-text (replace-regexp-in-string "\"" "\\\\\"" text)))
              (message "Generating audio for note '%s' using voice '%s'" note-title voice)
              (call-process "edge-tts" nil nil nil "--text" shell-text "--write-media" file "--voice" voice)
              (org-end-of-subtree)
              (insert "\n")
              (insert "[[attachment:audio.mp3]]"))))))))

(defun anki-editor-tts--skip-over-non-notes ()
  "Return position to continue from if the entry at point is not an Anki note.

Meant to be used as the \"skip function\" argument for 'org-map-entries'."
  (let ((org-trust-scanner-tags t))
    (let ((note-type (cdr (assoc "ANKI_NOTE_TYPE" (org-entry-properties nil "ANKI_NOTE_TYPE")))))
      (unless note-type
        (save-excursion (org-end-of-subtree t))))))

(defun anki-editor-tts-generate ()
  "Generate audio for the Anki note at point or each note in the active region if one is active."
  (interactive)
  (if (use-region-p)
      (org-map-entries #'my-anki-cloze-generate-audio nil 'region #'anki-editor-tts--skip-over-non-notes)
    (my-anki-cloze-generate-audio)))

(defun anki-editor-tts-play ()
  "Play the audio of the note at point."
  (interactive)
  (if-let ((dir (org-attach-dir)))
      (if-let ((file (car (directory-files dir t (rx string-start "audio.mp3" string-end)))))
          (make-process :name "anki-editor-tts-play"
                        :buffer nil
                        :command (list "mpv" "--terminal=no" file)
                        :noquery t ; Don't ask before exiting Emacs.
                        :connection-type 'pipe) ; Supposed to be more efficient/cleaner for non-interactive tools.
        (message "No audio file"))
    (message "No attach dir")))

(provide 'my-anki)

;;; my-anki.el ends here.
