;;; emacs-rc-completion.el --- Configuration for completion tools
;;;
;;; Commentary:
;;;
;;; Configuration for completion systems -- company and ivy and related tools.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package emacs-rc-custom)
(use-package emacs-rc-keybindings
  :demand
  :commands general-define-key)
(use-package emacs-rc-util
  :demand
  :commands emacs-rc--transform-func)
(use-package emacs-rc-ui)

(use-package company
  :demand
  :commands global-company-mode
  :after general delight
  :delight
  :general ("C-SPC" 'company-complete)
  :config
  (global-company-mode)
  (setq company-tooltip-align-annotations t))

(use-package company-tng
  :after company
  :demand
  :commands company-tng-mode
  :config (company-tng-mode))

(use-package company-quickhelp
  :demand
  :commands company-quickhelp-mode
  :delight
  :after company delight
  :config
  (setq company-quickhelp-use-propertized-text t
        company-quickhelp-delay 0.2)
  (company-quickhelp-mode))

(use-package company-emoji
  :after company
  :config (add-to-list 'company-backends 'company-emoji))

(use-package ivy
  :demand
  :commands ivy-mode ivy-configure
  :delight
  :after delight
  :config
  (setq ivy-use-virtual-buffers t
        ivy-format-functions-alist '((t . ivy-format-function-line))
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-plus))
        ivy-use-selectable-prompt t)
  (ivy-mode))

;; Turn on counsel for better ivy integration for M-x,
;; describe-function, and describe-variable
(use-package counsel
  :demand
  :commands counsel-mode
  :delight
  :after general ivy delight
  :config
  (setq counsel-rg-base-command (concat emacs-rc-rg-path
                                        " -M 240 --hidden --with-filename --no-heading --line-number --color never %s"))
  (ivy-configure 'counsel-M-x :display-transformer-fn #'emacs-rc--transform-func)
  (counsel-mode)
  :general
  (main-menu-def "SPC" '(counsel-M-x :which-key "Run"))
  (edit-menu-def "p" '(counsel-yank-pop :which-key "kill ring")))

(provide 'emacs-rc-completion)
;;; emacs-rc-completion.el ends here
