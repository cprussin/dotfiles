;;; imenu-list.el --- Right context sidebar configuration
;;;
;;; Commentary:
;;;
;;; This module sets up imenu-list, which shows a right-side sidebar
;;; containing imenu entries.
;;;
;;; Code:

(use-package imenu-list
  :init (setq imenu-list-focus-after-activation t
              imenu-list-auto-resize t)
  :general ('(normal motion emacs)
            "SPC ei" '(imenu-list-smart-toggle :which-key "Imenu")))

(provide 'imenu-list)
;;; imenu-list.el ends here
