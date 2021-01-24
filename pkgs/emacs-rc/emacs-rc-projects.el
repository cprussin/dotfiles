;;; emacs-rc-projects.el --- Configuration for project management tools
;;;
;;; Commentary:
;;;
;;; Configuration for tools for working with projects and source trees.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-keybindings)

(defun git-cmd (args)
  "Return a string representing the git command ARGS."
  (concat emacs-rc-git-path " " args))

(use-package direnv
  :demand
  :commands direnv-mode
  :after diff-mode
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

;; Set up editorconfig
(use-package editorconfig
  :demand
  :commands editorconfig-mode
  :after delight
  :delight
  :config
  (setq editorconfig-exec-path emacs-rc-editorconfig-path
        editorconfig-get-properties-function 'editorconfig-get-properties-from-exec)
  (editorconfig-mode))

;; Enable projectile
(use-package projectile
  :demand
  :commands projectile-mode
  :delight '(:eval (concat " " (projectile-project-name) " "))
  :after delight
  :config
  (setq projectile-completion-system 'ivy
        projectile-git-command (git-cmd "ls-files -zco --exclude-standard")
        projectile-git-submodule-command (git-cmd "submodule --quiet foreach 'echo $path' | tr '\\n' '\\0'")
        projectile-git-ignored-command (git-cmd "ls-files -zcoi --exclude-standard"))
  (projectile-mode))

;; And enable counsel-projectile, for better ivy integration
(use-package counsel-projectile
  :demand
  :commands counsel-projectile-mode
  :after counsel projectile
  :config (counsel-projectile-mode)
  :general
  ('(normal motion emacs)
   "SPC sp" '(counsel-projectile-rg :which-key "search in project"))
  ('(normal motion emac)
   :prefix "SPC p"
   "a" '(counsel-projectile-org-agenda :which-key "agenda")
   "d" '(counsel-projectile-find-dir :which-key "find directory")
   "f" '(counsel-projectile-find-file :which-key "find file")
   "g" '(counsel-projectile-switch-project :which-key "go to project")
   "o" '(counsel-projectile-org-capture :which-key "capture note")
   "s" '(counsel-projectile-rg :which-key "search")))

(provide 'emacs-rc-projects)
;;; emacs-rc-projects.el ends here
