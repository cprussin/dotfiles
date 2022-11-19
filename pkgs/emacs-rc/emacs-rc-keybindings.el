;;; emacs-rc-keybindings.el --- Configuration for keybindings
;;;
;;; Commentary:
;;;
;;; Configuration for system-wide keybindings (not including mode-specific
;;; binds).
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package general
  :demand
  :commands general-define-key
  :preface
  (general-create-definer main-menu-def
    :states '(normal insert emacs visual motion operator outer-tex-objects inner-text-objects replace-state)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer apps-menu-def
    :wrapping main-menu-def
    :infix "a")
  (general-create-definer buffer-menu-def
    :wrapping main-menu-def
    :infix "b")
  (general-create-definer edit-menu-def
    :wrapping main-menu-def
    :infix "e")
  (general-create-definer git-menu-def
    :wrapping main-menu-def
    :infix "g")
  (general-create-definer help-menu-def
    :wrapping main-menu-def
    :infix "h")
  (general-create-definer major-mode-menu-def
    :wrapping main-menu-def
    :infix "m")
  (general-create-definer project-menu-def
    :wrapping main-menu-def
    :infix "p")
  (general-create-definer search-menu-def
    :wrapping main-menu-def
    :infix "s")
  :config
  (main-menu-def "" nil
                 "w" '(evil-window-map :which-key "Window"))
  (apps-menu-def "" '(:ignore t :which-key "Apps")
                 "c" '(calc :which-key "Calculator"))
  (buffer-menu-def  "" '(:ignore t :which-key "Buffers")
                    "m" '(buffer-menu :which-key "menu")
                    "n" '(next-buffer :which-key "next")
                    "p" '(previous-buffer :which-key "previous")
                    "s" '(switch-to-buffer :which-key "switch")
                    "x" '(kill-current-buffer :which-key "close"))
  (edit-menu-def "" '(:ignore t :which-key "Editing"))
  (git-menu-def "" '(:ignore t :which-key "Git"))
  (help-menu-def "" '(:ignore t :which-key "Help"))
  (major-mode-menu-def "" '(:ignore t :which-key "Major Mode"))
  (project-menu-def "" '(:ignore t :which-key "Project"))
  (search-menu-def "" '(:ignore t :which-key "Search")))

;; Silence compile-time errors about evil-want-keybinding needing to be set
;; before loading evil or evil-collection.
(eval-when-compile (defvar evil-want-keybinding nil))
(use-package evil
  :demand
  :commands evil-mode
  :init (setq evil-want-keybinding nil
              evil-want-C-u-scroll t
              evil-want-C-i-jump t
              evil-want-integration t
              evil-undo-system 'undo-tree)
  :config (evil-mode))

(use-package evil-collection
  :demand
  :commands evil-collection-init
  :after evil company-tng
  :config
  (setq evil-collection-key-blacklist '("SPC"))
  (evil-collection-init))

(use-package evil-goggles
  :demand
  :commands evil-goggles-mode evil-goggles-use-diff-faces
  :after evil delight
  :delight
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(provide 'emacs-rc-keybindings)
;;; emacs-rc-keybindings.el ends here
