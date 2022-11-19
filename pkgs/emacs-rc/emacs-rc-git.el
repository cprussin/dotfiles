;;; emacs-rc-git.el --- Configuration for git
;;;
;;; Commentary:
;;;
;;; Configuration for tools for working with git.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-custom)
(require 'emacs-rc-keybindings)

(use-package magit
  :after general
  :config (setq magit-git-executable emacs-rc-git-path)
  :general
  (git-menu-def "c" '(magit-clone :which-key "git clone")
                "b" '(magit-blame :which-key "git blame")
                "l" '(magit-log :which-key "git log")
                "s" '(magit-status :which-key "git status")))

;; Show git status in the gutter
(use-package git-gutter
  :demand
  :commands global-git-gutter-mode
  :delight
  :after delight
  :config (global-git-gutter-mode))

(use-package git-timemachine
  :after general
  :general
  (git-menu-def "t" '(git-timemachine-toggle :which-key "Toggle Time Machine"))
  (major-mode-menu-def
   :keymaps 'git-timemachine-mode-map
   "" '(:ignore t :which-key "Major Mode (Time Machine)")
   "p" '(git-timemachine-show-previous-revision :which-key "Previous Revision")
   "n" '(git-timemachine-show-next-revision :which-key "Next Revision")
   "h" '(git-timemachine-kill-abbreviated-revision :which-key "Copy Abbreviated Revision Hash")
   "H" '(git-timemachine-kill-revision :which-key "Copy Full Revision Hash")
   "g" '(git-timemachine-show-nth-revision :which-key "Goto Nth Revision")
   "m" '(git-timemachine-show-revision-fuzzy :which-key "Find Revision By Message")
   "q" '(git-timemachine-quit :which-key "Quit")
   "b" '(git-timemachine-blame :which-key "Blame")
   "c" '(git-timemachine-show-commit :which-key "Show Commit")))

(provide 'emacs-rc-git)
;;; emacs-rc-git.el ends here
