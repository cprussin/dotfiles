;;; modeline.el --- Modeline configuration
;;;
;;; Commentary:
;;;
;;; This module sets up a modeline.
;;;
;;; Code:

;; Turn on powerline
(use-package powerline
  :init (setq powerline-height 25))

;; Turn on powerline-evil-center-color theme
(use-package powerline-evil
  :after powerline
  :init (setq powerline-evil-tag-style 'verbose)
  :config (powerline-evil-center-color-theme))

;; Use icons for most modes
(use-package mode-icons
  :after undo-tree
  :config (mode-icons-mode))

(provide 'modeline)
;;; modeline.el ends here
