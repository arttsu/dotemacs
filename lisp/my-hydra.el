;;; my-hydra.el --- Hydras -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defhydra my-hydra-jump (:exit t)
  "jump"
  ("h" (lambda () (interactive) (find-file "~/")) "home" :column "Directories")
  ("l" (lambda () (interactive) (find-file "~/lib")) "lib")
  ("o" (lambda () (interactive) (find-file "~/org")) "org")
  ("p" (lambda () (interactive) (find-file "~/org/gtd/projects")) "projects")
  ("a" (lambda () (interactive) (find-file "~/org/gtd/areas")) "areas")
  ("s" (lambda () (interactive) (find-file "~/stash")) "stash")
  ("x" scratch-buffer "scratch" :column "Files")
  ("c" (lambda () (interactive) (find-file my-custom-file)) "custom file"))

(provide 'my-hydra)
;;; my-hydra.el ends here.
