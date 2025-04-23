;;; emacs-rc-shell.el --- Configuration for shells
;;;
;;; Commentary:
;;;
;;; Configuration for working in shells inside of Emacs.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package emacs-rc-keybindings
  :demand
  :commands general-define-key)

(use-package eshell
  :after general
  :general (apps-menu-def "s" '(eshell :which-key "shell")))

(provide 'emacs-rc-shell)
;;; emacs-rc-shell.el ends here
