;;; emacs-rc-ui.el --- Configuration for ui elements
;;;
;;; Commentary:
;;;
;;; Configuration for various ui elements -- theme, bars, etc.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package emacs-rc-keybindings
  :demand
  :commands general-define-key)

(define-fringe-bitmap 'tilde [#b00000000
                              #b00000000
                              #b00000000
                              #b01110001
                              #b11011011
                              #b10001110
                              #b00000000
                              #b00000000])

(setq x-gtk-use-system-tooltips nil
      inhibit-startup-screen t
      inhibit-startup-message t
      scroll-conservatively 101
      scroll-margin 6)

(setq-default indicate-empty-lines t
              fringe-indicator-alist '((empty-line . tilde)))

(use-package frame
  :config (add-to-list 'default-frame-alist
                       '(font . "monospace-10")))

(use-package menu-bar
  :config (menu-bar-mode -1))

(use-package tool-bar
  :config (tool-bar-mode -1))

(use-package scroll-bar
  :config (scroll-bar-mode -1))

(use-package powerline
  :config (setq powerline-height 25))

(use-package powerline-evil
  :demand
  :after powerline evil
  :config
  ;; This is a fork of `powerline-evil-center-color-theme' which uses
  ;; `mode-line-position' instead of using `%4l : %3c' to display the line &
  ;; column.  We use this fork because for some modes (e.g. dired mode or
  ;; pdf-view mode), displaying line & column doesn't make sense and
  ;; `mode-line-position' has a more sensical display.  It is a mystery to me why this is not the default
  (defun emacs-rc--powerline-evil-center-color-theme-with-mode-line-position ()
    "Powerline's center-evil them with the evil state in color."
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list (powerline-raw "%*" nil 'l)
                                       (powerline-buffer-size nil 'l)
                                       (powerline-buffer-id nil 'l)
                                       (powerline-raw " ")
                                       (funcall separator-left mode-line face1)
                                       (powerline-narrow face1 'l)
                                       (powerline-vc face1)))
                            (rhs (list (powerline-raw global-mode-string face1 'r)
                                       (funcall separator-right face1 mode-line)
                                       (powerline-raw " ")
                                       (powerline-raw mode-line-position nil 'r)
                                       (powerline-hud face2 face1)))
                            (center (append (list (powerline-raw " " face1)
                                                  (funcall separator-left face1 face2)
                                                  (when (boundp 'erc-modified-channels-object)
                                                    (powerline-raw erc-modified-channels-object face2 'l))
                                                  (powerline-major-mode face2 'l)
                                                  (powerline-process face2)
                                                  (powerline-raw " " face2))
                                            (let ((evil-face (powerline-evil-face)))
                                              (if (split-string (format-mode-line minor-mode-alist))
                                                  (append (if evil-mode
                                                              (list (funcall separator-right face2 evil-face)
                                                                    (powerline-raw (powerline-evil-tag) evil-face 'l)
                                                                    (powerline-raw " " evil-face)
                                                                    (funcall separator-left evil-face face2)))
                                                          (list (powerline-minor-modes face2 'l)
                                                                (powerline-raw " " face2)
                                                                (funcall separator-right face2 face1)))
                                                (list (powerline-raw (powerline-evil-tag) evil-face)
                                                      (funcall separator-right evil-face face1)))))))
                       (concat (powerline-render lhs)
                               (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                               (powerline-render center)
                               (powerline-fill face1 (powerline-width rhs))
                               (powerline-render rhs)))))))
  (setq powerline-evil-tag-style 'verbose)
  (emacs-rc--powerline-evil-center-color-theme-with-mode-line-position))

(use-package solarized-theme
  :demand
  :commands solarized-color-blend
  :config
  (setq x-underline-at-descent-line t)
  (deftheme solarized-dark-with-fixes)
  (use-package solarized-palettes)
  (solarized-with-color-variables
    'dark
    'solarized-dark-with-fixes
    solarized-dark-color-palette-alist
    '(

      (custom-theme-set-faces
       theme-name

       ;; Make popups more visible
       `(tooltip ((t (:background ,base3 :foreground ,base01))))
       `(company-tooltip ((t (:background ,base3 :foreground ,base01))))
       `(company-tooltip-selection ((t (:background ,base01 :foreground ,base3))))
       `(company-scrollbar-bg ((,class (:background ,base2))))
       `(company-scrollbar-fg ((,class (:foreground ,base3 :background ,base00))))
       `(company-tooltip-common ((,class (:foreground ,magenta))))

       ;; Make org-mode tags look like tags
       `(org-tag ((t (:foreground ,violet :box t :height 0.8))))
       `(org-headline-done ((t (:strike-through t :foreground ,base01))))
       `(org-checkbox ((t (:box nil :foreground ,blue))))

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
       theme-name
       ;; Fix pos-tip popup colors
       `(pos-tip-foreground-color ,base01)
       `(pos-tip-background-color ,base3))))

  (enable-theme 'solarized-dark-with-fixes))

(use-package display-line-numbers
  :config
  (defvar display-line-numbers-exempt-modes
    '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode pdf-view-mode))

  (defun display-line-numbers--turn-on ()
    "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
    (unless (or (minibufferp)
                (member major-mode display-line-numbers-exempt-modes))
      (display-line-numbers-mode)))

  (global-display-line-numbers-mode))

(use-package zoom-frm
  :after general
  :general ("C-+" #'zoom-frm-in
            "C--" #'zoom-frm-out
            "C-*" #'zoom-frm-unzoom))

(use-package mode-icons
  :demand
  :commands mode-icons-mode
  :after undo-tree
  :config (mode-icons-mode))

(use-package unicode-fonts
  :demand
  :commands unicode-fonts-setup
  :config (unicode-fonts-setup))

(use-package undo-tree
  :demand
  :commands global-undo-tree-mode
  :delight
  :after delight general
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode)
  :general (edit-menu-def "t" '(undo-tree-visualize
                                :which-key "undo/redo tree")))

(use-package good-scroll
  :commands good-scroll-mode
  :config (good-scroll-mode))

(use-package delight)

(provide 'emacs-rc-ui)
;;; emacs-rc-ui.el ends here
