;;; text.el --- Configurations for text files
;;;
;;; Commentary:
;;;
;;; Configurations here apply to all text-based files
;;;
;;; Code:

(setq-default

 ;; Stop mixing tabs and spaces
 indent-tabs-mode nil

 ;; Fill to 80 characters instead of the default 70
 fill-column 80)

;; Turn on spell checking
(use-package flyspell
  :init (setq flyspell-issue-message-flag nil)
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

;; Browse with my browse script
(use-package browse-url
  :init (setq browse-url-generic-program "browse"
              browse-url-browser-function 'browse-url-generic))

;; Turn on URL discovery
(use-package goto-address
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)))

;; Intelligently clean up whitespace
(use-package ws-butler
  :delight
  :config (ws-butler-global-mode))

(provide 'text)
;;; text.el ends here
