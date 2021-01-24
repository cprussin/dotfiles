;;; emacs-rc-git.el --- Configuration for git
;;;
;;; Commentary:
;;;
;;; Configuration for tools for working with git.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-custom)

(use-package magit
  :after general
  :config (setq magit-git-executable emacs-rc-git-path)
  :general
  (magit-mode-map
   "SPC" nil
   "?" nil)
  ('(normal motion emacs)
   :prefix "SPC g"
   "c" '(magit-clone :which-key "git clone")
   "b" '(magit-blame :which-key "git blame")
   "l" '(magit-log :which-key "git log")
   "s" '(magit-status :which-key "git status")))

(use-package evil-magit
  :demand
  :commands evil-magit-init
  :after magit evil
  :config (evil-magit-init))

;; Show git status in the gutter
(use-package git-gutter
  :demand
  :commands global-git-gutter-mode
  :delight
  :after delight
  :config (global-git-gutter-mode))

(provide 'emacs-rc-git)
;;; emacs-rc-git.el ends here
