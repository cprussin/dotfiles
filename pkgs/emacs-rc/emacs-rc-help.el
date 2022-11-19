;;; emacs-rc-help.el --- Configuration for helpful tools
;;;
;;; Commentary:
;;;
;;; Configuration for various help tools.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-util)
(require 'emacs-rc-keybindings)
(require 'emacs-rc-completion)

;; Set up which-key to discover keybindings
(use-package which-key
  :demand
  :commands which-key-mode
  :delight
  :after general delight
  :config
  (setq which-key-idle-delay 0.25)
  (which-key-mode)
  :general
  (main-menu-def "?" '(which-key-show-top-level
                       :which-key "Show all keybindings"))
  ('(normal motion emacs) "?" #'which-key-show-top-level))

;; Turn on helpful for more helpful description buffers
(use-package helpful
  :after general ivy
  :config
  (ivy-configure 'helpful-callable :display-transformer-fn #'emacs-rc--transform-func)
  (ivy-configure 'helpful-variable :display-transformer-fn #'emacs-rc--transform-var)
  :general
  (help-menu-def "p" '(helpful-at-point
                       :which-key "describe thing at point")
                 "f" '(helpful-callable :which-key "describe function")
                 "v" '(helpful-variable :which-key "describe variable")))

(use-package pkg-info)

(provide 'emacs-rc-help)
;;; emacs-rc-help.el ends here
