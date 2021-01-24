;;; emacs-rc-help.el --- Configuration for helpful tools
;;;
;;; Commentary:
;;;
;;; Configuration for various help tools.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-util)

;; Set up which-key to discover keybindings
(use-package which-key
  :demand
  :commands which-key-mode
  :delight
  :after general delight
  :config
  (setq which-key-idle-delay 0.25)
  (which-key-mode)
  :general ('(normal motion emacs)
            "?" #'which-key-show-top-level
            "SPC ?" '(which-key-show-top-level
                      :which-key "Show all keybindings")))

;; Turn on helpful for more helpful description buffers
(use-package helpful
  :config
  (ivy-configure 'helpful-callable :display-transformer-fn #'transform-func)
  (ivy-configure 'helpful-variable :display-transformer-fn #'transform-var)
  :general ('(normal motion emacs)
            :prefix "SPC h"
            "p" '(helpful-at-point
                  :which-key "describe thing at point")
            "f" '(helpful-callable :which-key "describe function")
            "v" '(helpful-variable :which-key "describe variable")))

(use-package pkg-info)

(provide 'emacs-rc-help)
;;; emacs-rc-help.el ends here
