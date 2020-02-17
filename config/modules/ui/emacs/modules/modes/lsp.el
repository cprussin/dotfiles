;;; lsp.el --- Configurations for lang servers
;;;
;;; Commentary:
;;;
;;; This module sets up configs for language servers.
;;;
;;; Code:

(use-package lsp-mode
  :hook (sh-mode . lsp-deferred)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

;;; lsp.el ends here
