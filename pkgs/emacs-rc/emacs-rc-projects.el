;;; emacs-rc-projects.el --- Configuration for project management tools
;;;
;;; Commentary:
;;;
;;; Configuration for tools for working with projects and source trees.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package emacs-rc-keybindings
  :demand
  :commands general-define-key)

(use-package direnv
  :demand
  :commands direnv-mode
  :after diff-mode
  :init (setq direnv-always-show-summary nil)
  :config (direnv-mode))

;; Set up editorconfig
(use-package editorconfig
  :demand
  :commands editorconfig-mode
  :after delight
  :delight
  :config (editorconfig-mode))

;; Enable projectile
(use-package projectile
  :demand
  :commands projectile-mode
  :init (setq projectile-completion-system 'ivy)
  :config (projectile-mode)
  :general
  (project-menu-def
    "c" '(projectile-compile-project :which-key "compile project")
    "S" '(projectile-run-eshell :which-key "run a shell")))

;; And enable counsel-projectile, for better ivy integration
(use-package counsel-projectile
  :demand
  :commands counsel-projectile-mode
  :after counsel projectile
  :config (counsel-projectile-mode)
  :general
  (search-menu-def "p" '(counsel-projectile-rg :which-key "search in project"))
  (project-menu-def
    "a" '(counsel-projectile-org-agenda :which-key "agenda")
    "d" '(counsel-projectile-find-dir :which-key "find directory")
    "f" '(counsel-projectile-find-file :which-key "find file")
    "g" '(counsel-projectile-switch-project :which-key "go to project")
    "o" '(counsel-projectile-org-capture :which-key "capture note")
    "s" '(counsel-projectile-rg :which-key "search")))

(provide 'emacs-rc-projects)
;;; emacs-rc-projects.el ends here
