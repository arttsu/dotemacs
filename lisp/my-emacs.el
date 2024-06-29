(defun repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

(defvar-keymap tab-bar-history-repeat-map
  :repeat t
  "<left>" #'tab-bar-history-back
  "<right>" #'tab-bar-history-forward)

(defvar-keymap tab-bar-reapeat-map
  :repeat t
  "<left>" #'tab-bar-switch-to-prev-tab
  "<right>" #'tab-bar-switch-to-next-tab
  "m" #'tab-bar-move-tab)

(provide 'my-emacs)
