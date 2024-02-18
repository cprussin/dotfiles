;;; emacs-rc-lsp.el --- Configuration for LSP
;;;
;;; Commentary:
;;;
;;; Configuration for language server protocol integration.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package lsp-mode
  :commands lsp
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-peek-always-show t
        lsp-ui-sideline-show-hover t
        lsp-ui-doc-enable t))

(provide 'emacs-rc-lsp)
;;; emacs-rc-lsp.el ends here
