;;; purescript.el --- Configuration for purescript files
;;;
;;; Commentary:
;;;
;;; This module sets up configs for purescript files.
;;;
;;; Code:

(use-package purescript-mode
  :hook (purescript-mode . turn-on-purescript-indentation)
  :general ('(normal motion emacs) purescript-mode-map
            :prefix "SPC m"
            "" '(:ignore t :which-key "Major Mode (Purescript)")
            "." '(purescript-mode-format-imports
                  :which-key "format imports")))

(use-package psc-ide
  :hook (purescript-mode . psc-ide-mode)
  :general
  ('(normal motion emacs) psc-ide-mode-map
   :jump t
   "C-]" #'psc-ide-goto-definition)
  ('(normal motion emacs) psc-ide-mode-map
   :prefix "SPC m"
   "a" '(psc-ide-add-clause :which-key "add clause")
   "b" '(psc-ide-rebuild :which-key "rebuild")
   "c" '(psc-ide-case-split :which-key "split cases")
   "i" '(psc-ide-add-import :which-key "add import")
   "l" '(psc-ide-load-all :which-key "load modules")
   "q" '(psc-ide-server-quit :which-key "quit server")
   "s" '(psc-ide-server-start :which-key "start server")
   "t" '(psc-ide-show-type :which-key "show type")))

(provide 'purescript)
;;; purescript.el ends here
