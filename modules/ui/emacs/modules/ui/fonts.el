;;; fonts.el --- Font configuration
;;;
;;; Commentary:
;;;
;;; This module enables unicode fonts
;;;
;;; Code:

(use-package unicode-fonts
  :config (unicode-fonts-setup))

(add-to-list 'default-frame-alist
             `(font . ,(let ((font (gethash "primaryFont" nix-config)))
                         (concat (gethash "face" font) "-" (gethash "size" font)))))

(provide 'fonts)
;;; fonts.el ends here
