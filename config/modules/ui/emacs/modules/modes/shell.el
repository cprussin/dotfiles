;;; shell.el --- Configurations for shells
;;;
;;; Commentary:
;;;
;;; Configurations here apply to shells
;;;
;;; Code:

;; Enable colors!
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

(provide 'shell)
;;; shell.el ends here
