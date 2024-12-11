;;; emacs-rc-web.el --- Configuration for web development tools
;;;
;;; Commentary:
;;;
;;; Configuration for web development tools -- javascript and languages that
;;; compile to it, css, html, etc.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package emacs-rc-check
  :demand
  :commands flycheck-add-mode flycheck-add-next-checker)
(use-package emacs-rc-keybindings
  :demand
  :commands general-define-key)
(use-package emacs-rc-projects
  :demand)

(defun emacs-rc--find-in-node-modules (program)
  "Find PROGRAM either in node_modules or as a global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (global (executable-find program))
         (local (expand-file-name (concat "node_modules/.bin/" program)
                                  root)))
    (if (file-executable-p local) local global)))

(defun emacs-rc--setup-js-files ()
  "Prettify js code, and set relevant bin paths for flycheck."
  (push '("function" . ?λ) prettify-symbols-alist)
  (push '("require" . ?℞) prettify-symbols-alist)
  (push '("return" . ?←) prettify-symbols-alist)
  (push '("null" . ?∅) prettify-symbols-alist)
  (push '("undefined" . ?�) prettify-symbols-alist)
  (push '("=>" . ?→) prettify-symbols-alist)
  (prettify-symbols-mode)

  (setq-local flycheck-javascript-eslint-executable (emacs-rc--find-in-node-modules "eslint")
              flycheck-css-stylelint-executable (emacs-rc--find-in-node-modules "stylelint")))

(use-package typescript-ts-mode
  :demand t
  :after general
  :hook (typescript-ts-base-mode . emacs-rc--setup-js-files)
  :mode (("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode))
  :general
  (major-mode-menu-def
    :keymaps 'typescript-ts-base-mode-map
    "" '(:ignore t :which-key "Major Mode (Typescript)")))

(use-package tide
  :after typescript-ts-mode company flycheck
  :hook ((typescript-ts-base-mode . tide-setup)
         (tide-mode . tide-hl-identifier-mode))
  :config
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

(use-package jest
  :after general (:or js typescript-ts-mode)
  :hook ((js-mode typescript-ts-base-mode) . emacs-rc--use-jest-from-node-modules)
  :commands jest-popup
  :config
  (defun emacs-rc--use-jest-from-node-modules ()
    "Set the jest to use ./.node_modules/.bin/jest if such a path exists."
    (setq-local jest-executable (emacs-rc--find-in-node-modules "jest")))
  (setq jest-arguments '("--colors"))
  :general
  (major-mode-menu-def
    :keymaps '(js-mode-map typescript-ts-base-mode-map)
    "j" '(jest-popup :which-key "Jest")))

(use-package purescript-mode
  :demand
  :hook (purescript-mode . turn-on-purescript-indentation)
  :general
  (major-mode-menu-def
    :keymaps 'purescript-mode-map
    "" '(:ignore t :which-key "Major Mode (Purescript)")
    "." '(purescript-mode-format-imports
          :which-key "format imports")))

(use-package psc-ide
  :hook (purescript-mode . psc-ide-mode)
  :general
  (psc-ide-mode-map
   :jump t
   "C-]" #'psc-ide-goto-definition)
  (major-mode-menu-def
    :keymaps 'psc-ide-mode-map
    "a" '(psc-ide-add-clause :which-key "add clause")
    "b" '(psc-ide-rebuild :which-key "rebuild")
    "c" '(psc-ide-case-split :which-key "split cases")
    "i" '(psc-ide-add-import :which-key "add import")
    "l" '(psc-ide-load-all :which-key "load modules")
    "q" '(psc-ide-server-quit :which-key "quit server")
    "s" '(psc-ide-server-start :which-key "start server")
    "t" '(psc-ide-show-type :which-key "show type")))

(provide 'emacs-rc-web)
;;; emacs-rc-web.el ends here
