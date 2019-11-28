;;; mmm.el --- Allow multiple major modes per file
;;;
;;; Commentary:
;;;
;;; This module enables `mmm-mode' to allow files to have embedded
;;; modes.
;;;
;;; Code:

;; Enable mmm for files that have embedded code in other modes
(use-package mmm-mode
  :init (setq mmm-parse-when-idle t
              mmm-global-mode 'buffers-with-submode-classes)
  :config (require 'mmm-auto))

(provide 'mmm)
;;; mmm.el ends here
