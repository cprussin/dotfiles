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
  ;; This has to load after evil so that we can correctly override the `SPC'
  ;; keybinding
  :after evil
  :commands general-define-key
  :config
  (general-def '(normal motion emacs)
    :prefix "SPC"
    "" nil
    "a" '(:ignore t :which-key "Apps")
    "ac" '(calc :which-key "Calculator")
    "b" '(:ignore t :which-key "Buffers")
    "bm" '(buffer-menu :which-key "menu")
    "bn" '(next-buffer :which-key "next")
    "bp" '(previous-buffer :which-key "previous")
    "bs" '(ivy-switch-buffer :which-key "switch")
    "bx" '(kill-current-buffer t :which-key "close")
    "e" '(:ignore t :which-key "Editing")
    "g" '(:ignore t :which-key "Git")
    "h" '(:ignore t :which-key "Help")
    "p" '(:ignore t :which-key "Project")
    "s" '(:ignore t :which-key "Search")
    "w" '(evil-window-map :which-key "Window")))

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
