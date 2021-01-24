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

(defun find-in-node-modules (program)
  "Find PROGRAM either in node_modules or as a global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (global (executable-find program))
         (local (expand-file-name (concat "node_modules/.bin/" program)
                                  root)))
    (if (file-executable-p local) local global)))

(defun setup-js-files ()
  "Prettify js code, and set relevant bin paths for flycheck."
  (push '("function" . ?λ) prettify-symbols-alist)
  (push '("require" . ?℞) prettify-symbols-alist)
  (push '("return" . ?←) prettify-symbols-alist)
  (push '("null" . ?∅) prettify-symbols-alist)
  (push '("undefined" . ?�) prettify-symbols-alist)
  (push '("=>" . ?→) prettify-symbols-alist)
  (prettify-symbols-mode)

  (setq-local flycheck-javascript-eslint-executable (find-in-node-modules "eslint")
              flycheck-css-stylelint-executable (find-in-node-modules "stylelint")))

(use-package js
  :demand
  :after flycheck general
  :hook ((js-mode . setup-js-files)
         (js-mode . set-checker-eslint))
  :general
  ('(normal motion emacs) js-mode-map
   :prefix "SPC m"
   "" '(:ignore t :which-key "Major Mode (JS)"))
  :config
  (defun set-checker-eslint ()
    (setq-local flycheck-checker 'javascript-eslint))
  (flycheck-add-mode 'css-stylelint 'js-mode)
  (flycheck-add-next-checker 'javascript-eslint '(t . css-stylelint)))

(use-package typescript-mode
  :demand
  :mode "\\.tsx\\'"
  :hook (typescript-mode . setup-js-files)
  :after general
  :general
  ('(normal motion emacs) typescript-mode-map
   :prefix "SPC m"
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
         (tide-mode . set-checker-tide))
  :config
  (defun set-checker-tide ()
    (setq-local flycheck-checker (if (string-equal "tsx" (file-name-extension buffer-file-name))
                                     'tsx-tide
                                   'typescript-tide)))
  (flycheck-add-mode 'tsx-tide 'typescript-mode)
  (flycheck-add-next-checker 'typescript-tide '(t . javascript-eslint))
  (flycheck-add-next-checker 'tsx-tide '(t . javascript-eslint)))

(use-package jest
  :after general (:or js typescript-mode)
  :hook ((js-mode typescript-mode) . use-jest-from-node-modules)
  :commands jest-popup
  :config
  (setq jest-arguments '("--colors"))
  (defun use-jest-from-node-modules ()
    (setq-local jest-executable (find-in-node-modules "jest")))
  :general
  ('(normal motion emacs) (js-mode-map typescript-mode-map)
   "SPC m j" '(jest-popup :which-key "Jest")))

(use-package purescript-mode
  :demand
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

(provide 'emacs-rc-web)
;;; emacs-rc-web.el ends here
