;;; projects.el --- Configuration for projects
;;;
;;; Commentary:
;;;
;;; Configurations here apply to tools for working with projects.
;;; Mostly this means projectile.
;;;
;;; Code:

(defun git-cmd (args)
  "Return a string representing the git command ARGS."
  (concat (gethash "git" (gethash "paths" nix-config)) " " args))

;; Enable projectile
(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name) " "))
  :init
  (setq projectile-completion-system 'ivy
        projectile-git-command (git-cmd "ls-files -zco --exclude-standard")
        projectile-git-submodule-command (git-cmd "submodule --quiet foreach 'echo $path' | tr '\\n' '\\0'")
        projectile-git-ignored-command (git-cmd "ls-files -zcoi --exclude-standard"))
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
   "d" '(counsel-projectile-find-dir :which-key "find directory")
   "f" '(counsel-projectile-find-file :which-key "find file")
   "g" '(counsel-projectile-switch-project :which-key "go to project")
   "o" '(counsel-projectile-org-capture :which-key "capture note")
   "s" '(counsel-projectile-ag :which-key "search")))

(provide 'projects)
;;; projects.el ends here
