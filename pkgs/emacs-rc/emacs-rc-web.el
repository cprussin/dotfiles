;;; emacs-rc-web.el --- Configuration for web development tools
;;;
;;; Commentary:
;;;
;;; Configuration for web development tools -- javascript and languages that
;;; compile to it, css, html, etc.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-check)
(require 'emacs-rc-keybindings)

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

(use-package js
  :demand
  :after flycheck general
  :hook ((js-mode . emacs-rc--setup-js-files)
         (js-mode . emacs-rc--set-checker-eslint))
  :general
  (major-mode-menu-def
   :keymaps 'js-mode-map
   "" '(:ignore t :which-key "Major Mode (JS)"))
  :config
  (defun emacs-rc--set-checker-eslint ()
    "Set `javascript-eslint' as the current flycheck checker."
    (setq-local flycheck-checker 'javascript-eslint))
  (flycheck-add-mode 'css-stylelint 'js-mode)
  (flycheck-add-next-checker 'javascript-eslint '(t . css-stylelint)))

(use-package typescript-mode
  :demand
  :mode "\\.tsx\\'"
  :hook (typescript-mode . emacs-rc--setup-js-files)
  :after general
  :general
  (major-mode-menu-map
   :keymaps 'typescript-mode-map
   "" '(:ignore t :which-key "Major Mode (Typescript)"))
  :config
  (flycheck-add-mode 'css-stylelint 'typescript-mode))

(use-package prettier
  :demand
  :commands global-prettier-mode
  :config (global-prettier-mode))

(use-package tide
  :after typescript-mode company flycheck
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (tide-mode . emacs-rc--set-checker-tide))
  :config
  (defun emacs-rc--set-checker-tide ()
    "Set `typescript-tide' or `tsx-tide' as the current flycheck checker."
    (setq-local flycheck-checker (if (string-equal "tsx" (file-name-extension buffer-file-name))
                                     'tsx-tide
                                   'typescript-tide)))
  (flycheck-add-mode 'tsx-tide 'typescript-mode)
  (flycheck-add-next-checker 'typescript-tide '(t . javascript-eslint))
  (flycheck-add-next-checker 'tsx-tide '(t . javascript-eslint)))

(use-package jest
  :after general (:or js typescript-mode)
  :hook ((js-mode typescript-mode) . emacs-rc--use-jest-from-node-modules)
  :commands jest-popup
  :config
  (defun emacs-rc--use-jest-from-node-modules ()
    "Set the jest to use ./.node_modules/.bin/jest if such a path exists."
    (setq-local jest-executable (emacs-rc--find-in-node-modules "jest")))
  (setq jest-arguments '("--colors"))
  :general
  (major-mode-menu-map
   :keymaps '(js-mode-map typescript-mode-map)
   "j" '(jest-popup :which-key "Jest")))

(use-package purescript-mode
  :demand
  :hook (purescript-mode . turn-on-purescript-indentation)
  :general
  (major-mode-menu-map
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
  (major-mode-menu-map
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
