;;; theme.el --- Theme configuration
;;;
;;; Commentary:
;;;
;;; This module defines the color theme.
;;;
;;; Code:

;; Use solarized
(use-package solarized-theme
  :init (setq x-underline-at-descent-line t)
  :config
  (load-theme 'solarized-dark t)
  (solarized-with-color-variables 'dark
    (custom-theme-set-faces
     'solarized-dark

     ;; Make popups more visible
     `(popup-tip-face ((t (:background ,base3 :foreground ,base01))))
     `(tooltip ((t (:background ,base3 :foreground ,base01))))
     `(company-tooltip ((t (:background ,base3 :foreground ,base01))))
     `(company-tooltip-selection ((t (:background ,base01 :foreground ,base3))))
     `(company-scrollbar-bg ((,class (:background ,base2))))
     `(company-scrollbar-fg ((,class (:foreground ,base3 :background ,base00))))
     `(company-tooltip-common ((,class (:foreground ,magenta))))

     ;; Make org-mode tags look like tags
     `(org-tag ((t (:foreground ,violet :box t :height 0.8))))

     ;; Fix colors on modeline
     `(powerline-evil-normal-face ((t (:weight bold :inherit 'menu))))
     `(powerline-evil-insert-face ((t (:weight bold :inherit 'region))))
     `(powerline-evil-visual-face ((t (:weight bold :inherit 'lazy-highlight))))
     `(powerline-evil-operator-face ((t (:weight bold :inherit 'menu))))
     `(powerline-evil-replace-face ((t (:weight bold :inherit 'isearch))))
     `(powerline-evil-motion-face ((t (:weight bold :inherit 'menu))))
     `(powerline-evil-emacs-face ((t (:weight bold :inherit 'menu))))

     ;; Don't override shell colors
     `(comint-highlight-prompt (())))

    (custom-theme-set-variables
     'solarized-dark

     ;; Fix pos-tip popup colors
     `(pos-tip-foreground-color ,base01)
     `(pos-tip-background-color ,base3))))

(provide 'theme)
;;; theme.el ends here
