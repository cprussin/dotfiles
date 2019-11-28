;;; gui.el --- GUI configuration
;;;
;;; Commentary:
;;;
;;; This module disables pretty much all GUI elements.
;;;
;;; Code:

;; Turn off GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(setq x-gtk-use-system-tooltips nil)

(provide 'gui)
;;; gui.el ends here
