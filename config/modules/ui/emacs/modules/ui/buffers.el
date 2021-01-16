;;; buffers.el --- Configurations for buffer UI
;;;
;;; Commentary:
;;;
;;; Configurations here apply to the UI around buffers--fringes, etc
;;;
;;; Code:

;; Display a tilde after buffer end like in vim
(define-fringe-bitmap 'tilde [#b00000000
                              #b00000000
                              #b00000000
                              #b01110001
                              #b11011011
                              #b10001110
                              #b00000000
                              #b00000000])
(setq-default indicate-empty-lines t
              fringe-indicator-alist '((empty-line . tilde)))

;; Scroll one line at a time
(setq scroll-step 1)

;; Turn on line numbers
(use-package display-line-numbers
  :config (global-display-line-numbers-mode))

;; Highlight current line
(use-package hl-line
  :config (global-hl-line-mode))

;; Show git status in the gutter
(use-package git-gutter
  :delight
  :config (global-git-gutter-mode))

;; Show emojis!
(use-package emojify
  :init (setq emojify-download-emojis-p t)
  :config
  (global-emojify-mode)
  (global-emojify-mode-line-mode))

(provide 'buffers)
;;; buffers.el ends here
