;;; emacs-rc-completion.el --- Configuration for completion tools
;;;
;;; Commentary:
;;;
;;; Configuration for completion systems -- company and ivy and related tools.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-custom)
(require 'emacs-rc-keybindings)
(require 'emacs-rc-util)

(use-package company
  :demand
  :commands global-company-mode
  :after general delight
  :delight
  :general ("C-SPC" 'company-complete)
  :config (global-company-mode))

(use-package company-tng
  :after company
  :demand
  :commands company-tng-configure-default
  :config
  (company-tng-configure-default))

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
        ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-mode))

;; Turn on counsel for better ivy integration for M-x,
;; describe-function, and describe-variable
(use-package counsel
  :demand
  :commands counsel-mode
  :delight
  :after ivy delight
  :config
  (setq counsel-rg-base-command (concat emacs-rc-rg-path
                                        " -M 240 --with-filename --no-heading --line-number --color never %s"))
  (ivy-configure 'counsel-M-x :display-transformer-fn #'emacs-rc--transform-func)
  (counsel-mode)
  :general ('(normal motion emacs)
            :prefix "SPC"
            "SPC" '(counsel-M-x :which-key "Run")
            "ep" '(counsel-yank-pop :which-key "kill ring")))

(provide 'emacs-rc-completion)
;;; emacs-rc-completion.el ends here
