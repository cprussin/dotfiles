;;; undo-tree.el --- A better undo
;;;
;;; Commentary:
;;;
;;; This module sets up a better undo system.
;;;
;;; Code:

;; Turn on undo-tree
(use-package undo-tree
  :delight
  :init (setq undo-tree-visualizer-timestamps t)
  :config (global-undo-tree-mode)
  :general ('(normal motion emacs)
            "SPC et" '(undo-tree-visualize
                       :which-key "undo/redo tree")))

(provide 'undo-tree)
;;; undo-tree.el ends here
