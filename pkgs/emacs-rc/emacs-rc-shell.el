;;; emacs-rc-shell.el --- Configuration for shells
;;;
;;; Commentary:
;;;
;;; Configuration for working in shells inside of Emacs.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-custom)

(setq shell-file-name shell-path)

(use-package shell
  :hook (shell-mode . ansi-color-for-comint-mode-on)
  :general
  ('(normal motion emacs)
   "SPC as" '(shell :which-key "shell"))
  (shell-mode-map
   "C-c" #'comint-interrupt-subjob
   "C-n" #'comint-next-input
   "C-p" #'comint-previous-input)
  ('(normal motion emacs) shell-mode-map
   :prefix "SPC m"
   "" '(:ignore t :which-key "Major Mode (Shell)")
   "q" '(comint-kill-subjob :which-key "quit")
   "w" '(comint-write-output :which-key "save")))

(provide 'emacs-rc-shell)
;;; emacs-rc-shell.el ends here
