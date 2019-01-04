;;; git.el --- Configuration for tooling around using git
;;;
;;; Commentary:
;;;
;;; Configurations here apply to tools for working with git
;;;
;;; Code:

;; Magit!
(use-package magit
  :init
  (setq magit-git-executable (cdr (assoc "git" paths)))
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
  :after (magit evil)
  :config (evil-magit-init))

(provide 'git)
;;; git.el ends here
