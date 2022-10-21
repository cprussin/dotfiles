;;; emacs-rc-check.el --- Configuration for checking tools
;;;
;;; Commentary:
;;;
;;; Configuration for tools that check contents of a buffer -- flycheck and
;;; flyspell, etc.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-custom)

;; Enable flycheck for source code checks
(use-package flycheck
  :demand
  :commands global-flycheck-mode flycheck-add-mode flycheck-add-next-checker
  :init (setq flycheck-emacs-lisp-load-path 'inherit)
  :config (global-flycheck-mode))

(use-package flycheck-pos-tip
  :demand
  :commands flycheck-pos-tip-mode
  :config
  (setq flycheck-pos-tip-timeout 0)
  (flycheck-pos-tip-mode))

;; Turn on spell checking
(use-package flyspell
  :config (setq flyspell-issue-message-flag nil
                ispell-program-name emacs-rc-ispell-path)
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(provide 'emacs-rc-check)
;;; emacs-rc-check.el ends here
