;;; code.el --- Configurations for all code types
;;;
;;; Commentary:
;;;
;;; Configurations here apply to any coding mode; e.g., anything under
;;; `prog-mode'.
;;;
;;; Code:

;; Turn on rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight matching parens
(use-package paren
  :hook (prog-mode . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t))

;; Highlight hardcoded numbers and the likes
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; Set up editorconfig
(use-package editorconfig
  :delight
  :config (editorconfig-mode))

;; Aggressively re-indent
(use-package aggressive-indent
  :delight
  :config
  (global-aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'purescript-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode))

;; Show indentation guide
(use-package indent-guide
  :delight
  :config (indent-guide-global-mode))

;; Enable flycheck for source code checks
(use-package flycheck
  :config
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (defun use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (global-eslint (executable-find "eslint"))
           (local-eslint (expand-file-name "node_modules/.bin/eslint" root))
           (eslint (if (file-executable-p local-eslint)
                       local-eslint
                     global-eslint)))
      (setq-local flycheck-javascript-eslint-executable eslint)))
  (add-hook 'web-mode-hook 'use-eslint-from-node-modules)
  (add-hook 'js-jsx-mode-hook 'use-eslint-from-node-modules)
  )
(use-package flycheck-pos-tip
  :ensure t
  :init
  (setq flycheck-pos-tip-timeout 0)
  (eval-after-load 'flycheck (flycheck-pos-tip-mode)))

;; Enable smarter surrounding pairs
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))
(use-package evil-smartparens
  :after smartparens
  :delight
  :hook (smartparens-enabled . evil-smartparens-mode))

;; Highlight TODO comments
(use-package hl-todo
  :config (global-hl-todo-mode))

(provide 'code)
;;; code.el ends here
