;;; my-ui.el --- UI helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun my-ui-set-theme-and-font (modus-theme font font-height &optional emoji-font)
  "Set Modus theme and font on startup.

MODUS-THEME is the Modus theme symbol to load.

FONT is the font family name, and FONT-HEIGHT is its height in 1/10pt
units.

If EMOJI-FONT is non-nil, use it for Unicode emoji glyphs (useful on
Windows)."
  (modus-themes-load-theme modus-theme)
  (set-face-attribute 'default nil :font font :height font-height)
  (set-frame-font font nil t)
  (when emoji-font
    (set-fontset-font t 'unicode emoji-font nil 'append)))

(provide 'my-ui)

;;; my-ui.el ends here
