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

(defvar-keymap horizontal-scroll-repeat-map
  :repeat t
  ">" #'scroll-right
  "<" #'scroll-left)

(defun my-split-window-sensibly (&optional window)
    (interactive)
    (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
                 (with-selected-window window
                     (split-window-right)))
            (and (window-splittable-p window)
                 (with-selected-window window
                     (split-window-below))))))

(setq split-window-preferred-function #'my-split-window-sensibly)

(defun my/kill-forward-to-word ()
  (interactive)
  (kill-region (point) (progn (forward-to-word 1) (point))))

(defun my/kill-backward-to-word ()
  (interactive)
  (kill-region (point) (progn (backward-to-word 1) (point))))

(provide 'my-emacs)
