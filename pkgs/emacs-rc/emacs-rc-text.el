;;; emacs-rc-text.el --- Configuration for all text buffers
;;;
;;; Commentary:
;;;
;;; Configuration for various tools for working with all text buffers.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package emacs-rc-custom)
(use-package emacs-rc-keybindings
  :demand
  :commands general-define-key)

(setq-default indent-tabs-mode nil
              fill-column 80)

(use-package paren
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode))

(use-package browse-url
  :config (setq browse-url-generic-program emacs-rc-browse-path
                browse-url-browser-function 'browse-url-generic))

(use-package hl-line
  :config (global-hl-line-mode))

;; Turn on swiper for a better search
(use-package swiper
  :after ivy
  :general
  ('(normal emacs visual) "/" '(swiper :which-key "search"))
  (search-menu-def "f" '(swiper :which-key "search in file")))

;; Turn on rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight hardcoded numbers and the likes
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; Aggressively re-indent
(use-package aggressive-indent
  :demand
  :commands global-aggressive-indent-mode
  :delight
  :after delight
  :config
  (global-aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'purescript-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'typescript-mode))

;; Show indentation guide
(use-package indent-guide
  :demand
  :commands indent-guide-global-mode
  :delight
  :after delight
  :config (indent-guide-global-mode))

;; Enable smarter surrounding pairs
(use-package smartparens
  :demand
  :commands smartparens-global-mode
  :config
  (use-package smartparens-config)
  (smartparens-global-mode))
(use-package evil-smartparens
  :after smartparens delight
  :delight
  :hook (smartparens-enabled . evil-smartparens-mode))

;; Highlight TODO comments
(use-package hl-todo
  :demand
  :commands global-hl-todo-mode
  :config (global-hl-todo-mode))

;; Turn on URL discovery
(use-package goto-addr
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)))

;; Intelligently clean up whitespace
(use-package ws-butler
  :demand
  :commands ws-butler-global-mode
  :delight
  :after delight
  :config (ws-butler-global-mode))

;; Show emojis!
(use-package emojify
  :preface
  ;; Suppress compile-time warnings by setting this to some path that is guaranteed to exist
  (eval-when-compile (setq emojify-emojis-dir "/"
                           emojify-emoji-set "/"))
  :demand
  :commands global-emojify-mode global-emojify-mode-line-mode
  :config
  (setq emojify-emojis-dir emacs-rc-emoji-sets-path)
  (global-emojify-mode)
  (global-emojify-mode-line-mode))

(use-package imenu-list
  :after general
  :config (setq imenu-list-focus-after-activation t
                imenu-list-auto-resize t)
  :general (edit-menu-def "i" '(imenu-list-smart-toggle :which-key "Imenu")))

(use-package link-hint
  :after general
  :general
  (edit-menu-def
    "l" '(:ignore t :which-key "Links")
    "lo" '(link-hint-open-link :which-key "open")
    "ly" '(link-hint-copy-link :which-key "copy")))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(provide 'emacs-rc-text)
;;; emacs-rc-text.el ends here
