;;; emacs-rc-check.el --- Configuration for checking tools
;;;
;;; Commentary:
;;;
;;; Configuration for tools that check contents of a buffer -- flycheck and
;;; flyspell, etc.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package emacs-rc-custom)

;; Enable flycheck for source code checks
(use-package flycheck
  :demand
  :commands global-flycheck-mode flycheck-add-mode flycheck-add-next-checker
  :init (setq flycheck-emacs-lisp-load-path 'inherit)
  :config (global-flycheck-mode))

(use-package flycheck-popup-tip
  :demand
  :commands flycheck-popup-tip-mode
  :hook (flycheck-mode . flycheck-popup-tip-mode))

;; Turn on spell checking
(use-package flyspell
  :config (setq flyspell-issue-message-flag nil
                ispell-program-name emacs-rc-ispell-path)
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

(provide 'emacs-rc-check)
;;; emacs-rc-check.el ends here
