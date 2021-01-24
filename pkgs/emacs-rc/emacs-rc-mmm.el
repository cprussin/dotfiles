;;; emacs-rc-mmm.el --- Configuration for multi-mode buffers
;;;
;;; Commentary:
;;;
;;; Configuration for buffers that have different modes embedded inside each
;;; other.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package mmm-mode
  :config
  (setq mmm-parse-when-idle t
        mmm-global-mode 'buffers-with-submode-classes)
  (require 'mmm-auto))

(provide 'emacs-rc-mmm)
;;; emacs-rc-mmm.el ends here
