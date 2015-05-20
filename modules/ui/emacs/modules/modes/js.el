;;; js.el --- Configurations for javascript files
;;;
;;; Commentary:
;;;
;;; This module sets up configs for javascript files.
;;;
;;; Code:

(use-package web-mode
  :config
  (defun prettify-js ()
    (push '("function" . ?λ) prettify-symbols-alist)
    (push '("require" . ?℞) prettify-symbols-alist)
    (push '("return" . ?←) prettify-symbols-alist)
    (push '("null" . ?∅) prettify-symbols-alist)
    (push '("undefined" . ?�) prettify-symbols-alist)
    (push '("=>" . ?→) prettify-symbols-alist)
    (prettify-symbols-mode))

  ;; Make js2 syntax highlighting better
  (setq-default js2-highlight-level 3
                js2-include-browser-externs t
                js2-include-node-externs t)

  ;;;; Prettify js and jsx files
  (add-hook 'web-mode-hook 'prettify-js)
  (add-hook 'web-mode-hook 'turn-off-fci-mode)

  ;; Use rjsx-mode on .js files since it handles flow syntax better and to
  ;; keep consistency
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  )

;;(defun use-flow-from-node-modules ()
;;  (let* ((root (locate-dominating-file
;;                (or (buffer-file-name) default-directory)
;;                "node_modules"))
;;         (global-flow (executable-find "flow"))
;;         (local-flow (expand-file-name "node_modules/.bin/flow" root))
;;         (flow (if (file-executable-p local-flow)
;;                   local-flow
;;                 global-flow)))
;;    (setq-local flycheck-javascript-flow-executable flow)))
;;
;;(defun use-stylelint-from-node-modules ()
;;  (let* ((root (locate-dominating-file
;;                (or (buffer-file-name) default-directory)
;;                "node_modules"))
;;         (global-stylelint (executable-find "stylelint"))
;;         (local-stylelint (expand-file-name "node_modules/.bin/stylelint"
;;                                            root))
;;         (stylelint (if (file-executable-p local-stylelint)
;;                        local-stylelint
;;                      global-stylelint)))
;;    (setq-local flycheck-css-stylelint-executable stylelint)))
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defun setup-flow-file()
;;  (flycheck-select-checker 'javascript-flow))
;;
;;(defun check-for-buffer-text(buffer-text pattern)
;;  (string-match pattern buffer-text))
;;
;;(defun check-for-buffer-text-with-callback(buffer-text pattern callback)
;;  (if
;;      (check-for-buffer-text pattern buffer-text)
;;      (funcall callback)))
;;
;;(defun setup-flow-files(callback)
;;  (check-for-buffer-text
;;   (buffer-substring-no-properties (point-min) (point-max)) "@flow" callback))
;;
;;(defun javascript-flow-predicate()
;;  (and
;;   buffer-file-name
;;   (file-exists-p buffer-file-name)
;;   (check-for-buffer-text
;;    (buffer-substring-no-properties (point-min) (point-max))
;;    "@flow")
;;   (locate-dominating-file buffer-file-name ".flowconfig")))
;;
;;
;;
;;                                        ; Use eslint, stylelint, and flow from node_modules if it exists
;;(add-hook 'js2-mode-hook #'spacemacs//react-use-eslint-from-node-modules)
;;(add-hook 'js2-mode-hook #'use-flow-from-node-modules)
;;(add-hook 'react-mode-hook #'use-flow-from-node-modules)
;;
;;                                        ; Make flow work properly
;;(flycheck-def-args-var flycheck-javascript-flow-args javascript-flow)
;;(customize-set-variable 'flycheck-javascript-flow-args '())
;;(flycheck-define-checker javascript-flow
;;  "A JavaScript syntax and style checker using Flow. See URL `http://flowtype.org/'."
;;  :command (
;;            "flow"
;;            "check-contents"
;;            (eval flycheck-javascript-flow-args)
;;            "--from" "emacs"
;;            "--color=never"
;;            source-original)
;;  :standard-input t
;;  :predicate javascript-flow-predicate
;;  :next-checkers (javascript-eslint)
;;  :error-patterns
;;  ((error line-start
;;          (file-name)
;;          ":"
;;          line
;;          "\n"
;;          (message (minimal-match (and (one-or-more anything) "\n")))
;;          line-end))
;;  :modes (js-mode js2-mode js3-mode react-mode))
;;(add-to-list 'flycheck-checkers 'javascript-flow)
;;
;;; js.el ends here
