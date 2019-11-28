;;; links.el --- Configuration for interacting with links
;;;
;;; Commentary:
;;;
;;; This module sets up tools for working with links in files
;;;
;;; Code:

(use-package link-hint
  :ensure t
  :general ('(normal motion emacs)
            :prefix "SPC el"
            "" '(:ignore t :which-key "Links")
            "o" '(link-hint-open-link :which-key "open")
            "y" '(link-hint-copy-link :which-key "copy")))

(provide 'links)
;;; links.el ends here
