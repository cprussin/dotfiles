;;; emacs-rc-image.el --- Configuration for image buffers
;;;
;;; Commentary:
;;;
;;; Configuration for image buffers.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package emacs-rc-custom)

(use-package image
  :config (imagemagick-register-types))

(use-package image-dired-external
  :config (setq image-dired-cmd-rotate-original-program emacs-rc-jpegtran-path
                image-dired-cmd-pngnq-program emacs-rc-pngnq-path
                image-dired-cmd-write-exif-data-program emacs-rc-exiftool-path
                image-dired-cmd-pngcrush-program emacs-rc-pngcrush-path
                image-dired-cmd-create-thumbnail-program emacs-rc-convert-path
                image-dired-cmd-optipng-program emacs-rc-optipng-path))

(provide 'emacs-rc-image)
;;; emacs-rc-image.el ends here
