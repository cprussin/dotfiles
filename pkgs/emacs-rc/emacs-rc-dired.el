;;; emacs-rc-dired.el --- Configuration for directories
;;;
;;; Commentary:
;;;
;;; Configuration for directory buffers.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package dired
  :general
  (major-mode-menu-def
    :keymaps 'dired-mode-map
    "" '(:ignore t :which-key "Major Mode (Dired)")
    "t" '(dired-omit-mode :which-key "Toggle hiding uninteresting files")))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :config (setq dired-omit-files "^\\.[^\.]"))

(provide 'emacs-rc-dired)
;;; emacs-rc-dired.el ends here
