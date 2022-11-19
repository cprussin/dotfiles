;;; emacs-rc-shell.el --- Configuration for shells
;;;
;;; Commentary:
;;;
;;; Configuration for working in shells inside of Emacs.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package emacs-rc-custom)
(use-package emacs-rc-keybindings
  :demand
  :commands general-define-key)

(setq shell-file-name emacs-rc-shell-path)

(use-package shell
  :after general
  :hook (shell-mode . ansi-color-for-comint-mode-on)
  :general
  (shell-mode-map
   "C-c" #'comint-interrupt-subjob
   "C-n" #'comint-next-input
   "C-p" #'comint-previous-input)
  (apps-menu-def "s" '(shell :which-key "shell"))
  (major-mode-menu-def
   :keymaps 'shell-mode-map
   "" '(:ignore t :which-key "Major Mode (Shell)")
   "q" '(comint-kill-subjob :which-key "quit")
   "w" '(comint-write-output :which-key "save")))

(provide 'emacs-rc-shell)
;;; emacs-rc-shell.el ends here
