;;; direnv.el --- Configuration for direnv
;;;
;;; Commentary:
;;;
;;; Enable direnv integration.
;;;
;;; Code:

(use-package diff-mode)

;; Enable direnv
(use-package direnv
  :after diff-mode
  :config (direnv-mode))

(provide 'direnv)
;;; direnv.el ends here
