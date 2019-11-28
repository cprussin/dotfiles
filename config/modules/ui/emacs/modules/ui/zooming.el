;;; zooming.el --- Zoom!
;;;
;;; Commentary:
;;;
;;; This module sets up frame zooming
;;;
;;; Code:

(use-package zoom-frm
  :general ("C-+" #'zoom-frm-in
            "C--" #'zoom-frm-out
            "C-*" #'zoom-frm-unzoom))

(provide 'zooming)
;;; zooming.el ends here
