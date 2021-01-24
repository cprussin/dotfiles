;;; emacs-rc-pdf.el --- Configuration for pdfs
;;;
;;; Commentary:
;;;
;;; Configuration for pdf buffers
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package pdf-tools
  :commands pdf-tools-install
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (require 'pdf-occur)
  (pdf-tools-install t)
  :general
  (pdf-view-mode-map
   "SPC" nil)
  (pdf-view-mode-map
   :prefix "SPC m"
   "" '(:ignore t :which-key "Major Mode (PDF)")
   "f" '(:ignore t :which-key "Fit")
   "fh" '(pdf-view-fit-height-to-window :which-key "height")
   "fp" '(pdf-view-fit-page-to-window :which-key "page")
   "fw" '(pdf-view-fit-width-to-window :which-key "width")
   "l" '(pdf-view-goto-label :which-key "goto label")
   "r" '(:ignore t :which-key "Rendering")
   "rd" '(pdf-view-dark-minor-mode :which-key "dark")
   "rm" '(pdf-view-midnight-minor-mode :which-key "midnight")
   "rp" '(pdf-view-printer-minor-mode :which-key "printer"))
  (pdf-annot-minor-mode-map
   :prefix "SPC ma"
   "" '(:ignore t :which-key "Annotate")
   "a" '(pdf-annot-attachment-dired :which-key "attach")
   "d" '(pdf-annot-delete :which-key "delete")
   "l" '(pdf-annot-list-annotations :which-key "list")
   "m" '(:ignore t :which-key "Markup")
   "mh" '(pdf-annot-add-highlight-markup-annotation
          :which-key "add highlight markup")
   "mm" '(pdf-annot-add-markup-annotation :which-key "add markup")
   "ms" '(pdf-annot-add-squiggly-markup-annotation
          :which-key "add squiggly markup")
   "mt" '(pdf-annot-add-strikeout-markup-annotation
          :which-key "add strikeout markup")
   "mu" '(pdf-annot-add-underline-markup-annotation
          :which-key "add underline markup")
   "t" '(pdf-annot-add-text-annotation :which-key "add text"))
  (pdf-misc-minor-mode-map
   "SPC mm" '(pdf-misc-display-metadata :which-key "show metadata")))

(provide 'emacs-rc-pdf)
;;; emacs-rc-pdf.el ends here
