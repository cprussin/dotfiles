;;; emacs-rc-image.el --- Configuration for image buffers
;;;
;;; Commentary:
;;;
;;; Configuration for image buffers.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package image
  :config (imagemagick-register-types))

(provide 'emacs-rc-image)
;;; emacs-rc-image.el ends here
