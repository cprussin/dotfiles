;;; keybindings.el --- Keybinding configuration
;;;
;;; Commentary:
;;;
;;; This module sets up all the keybindings.
;;;
;;; Code:

;; Set up evil
(use-package evil
  :init (setq evil-want-C-u-scroll t
              evil-want-integration nil)
  :config (evil-mode))
(use-package evil-collection
  :after evil
  :init (setq evil-collection-key-blacklist '("SPC"))
  :config (evil-collection-init))
(use-package evil-goggles
  :after evil
  :delight
  :config (evil-goggles-mode))

;; Now, set up all custom keybindings
(use-package general
  :after evil
  :config

  ;; Make esc abort
  ;;(general-def
  ;;  "<escape>" #'keyboard-escape-quit)

  ;; Set up SPC-leader keys
  (general-def '(normal motion emacs)
    :prefix "SPC"
    "" nil
    "a" '(:ignore t :which-key "Apps")
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

(provide 'keybindings)
;;; keybindings.el ends here
