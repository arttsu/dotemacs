(straight-use-package 'hydra)

(defhydra my-copilot-accept-completion (copilot-mode-map "C-M-<tab>")
  "Accept Copilot completion"
  ("C-M-<tab>" copilot-accept-completion "Accept" :color blue)
  ("M-f" copilot-accept-completion-by-word "By word")
  ("C-e" copilot-accept-completion-by-line "By line"))

(provide 'my-copilot)
