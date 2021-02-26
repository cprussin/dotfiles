;;; emacs-rc.el --- Emacs configuration
;;;
;;; Commentary:
;;;
;;; Emacs configuration
;;;
;;; Code:

(require 'cl-macs)

(cl-eval-when (eval load)
  (require 'emacs-rc-check)
  (require 'emacs-rc-completion)
  (require 'emacs-rc-custom)
  (require 'emacs-rc-files)
  (require 'emacs-rc-git)
  (require 'emacs-rc-help)
  (require 'emacs-rc-image)
  (require 'emacs-rc-keybindings)
  (require 'emacs-rc-markdown)
  (require 'emacs-rc-mmm)
  (require 'emacs-rc-modes)
  (require 'emacs-rc-org)
  (require 'emacs-rc-pdf)
  (require 'emacs-rc-projects)
  (require 'emacs-rc-shell)
  (require 'emacs-rc-text)
  (require 'emacs-rc-ui)
  (require 'emacs-rc-web))

(provide 'emacs-rc)
;;; emacs-rc.el ends here
