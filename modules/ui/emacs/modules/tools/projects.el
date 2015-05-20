;;; projects.el --- Configuration for projects
;;;
;;; Commentary:
;;;
;;; Configurations here apply to tools for working with projects.
;;; Mostly this means projectile.
;;;
;;; Code:

;; Enable projectile
(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name) " "))
  :init (setq projectile-completion-system 'ivy)
  :config (projectile-mode))

;; And enable counsel-projectile, for better ivy integration
(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode)
  :general
  ('(normal motion emacs)
   "SPC sp" '(counsel-projectile-ag :which-key "search in project"))
  ('(normal motion emac)
   :prefix "SPC p"
   "a" '(counsel-projectile-org-agenda :which-key "agenda")
   "f" '(counsel-projectile-find-file :which-key "find file")
   "g" '(counsel-projectile-switch-project :which-key "go to project")
   "o" '(counsel-projectile-org-capture :which-key "capture note")
   "s" '(counsel-projectile-ag :which-key "search")))

(provide 'projects)
;;; projects.el ends here
