;;; emacs-rc-hledger.el --- Set up hledger tooling  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; This module sets up hledger-mode and related hledger tooling.
;;;
;;; Code:

(use-package emacs-rc-completion)

(use-package hledger-mode
  :mode ("\\.journal\\'" "\\.hledger\\'")
  :after company
  :demand
  :init
  (setq hledger-jfile (expand-file-name "~/Notes/ledger.journal")))

(use-package hledger-input
  :commands hledger-shell-command-to-string hledger-show-new-balances
  :demand
  :preface
  (defun popup-balance-at-point ()
    "Show balance for account at point in a popup."
    (interactive)
    (if-let ((account (thing-at-point 'hledger-account)))
        (message (hledger-shell-command-to-string (format " balance -N %s "
                                                          account)))
      (message "No account at point")))

  :config
  (setq hledger-input-buffer-height 20)
  (add-hook 'hledger-input-post-commit-hook #'hledger-show-new-balances)
  (add-hook 'hledger-input-mode-hook #'auto-fill-mode))

(use-package flycheck-hledger
  :after (flycheck hledger-mode)
  :demand)

;;; emacs-rc-hledger.el ends here
