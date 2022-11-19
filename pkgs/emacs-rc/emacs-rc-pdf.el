;;; emacs-rc-pdf.el --- Configuration for pdfs
;;;
;;; Commentary:
;;;
;;; Configuration for pdf buffers
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-keybindings)

(use-package pdf-tools
  :after general
  :commands pdf-tools-install pdf-view-mode
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (pdf-tools-install t)
  (require 'pdf-occur)
  :general
  (major-mode-menu-def
   :keymaps 'pdf-view-mode-map
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
  (major-mode-menu-def
   :keymaps 'pdf-annot-minor-mode-map
   "a" '(:ignore t :which-key "Annotate")
   "aa" '(pdf-annot-attachment-dired :which-key "attach")
   "ad" '(pdf-annot-delete :which-key "delete")
   "al" '(pdf-annot-list-annotations :which-key "list")
   "am" '(:ignore t :which-key "Markup")
   "amh" '(pdf-annot-add-highlight-markup-annotation
           :which-key "add highlight markup")
   "amm" '(pdf-annot-add-markup-annotation :which-key "add markup")
   "ams" '(pdf-annot-add-squiggly-markup-annotation
           :which-key "add squiggly markup")
   "amt" '(pdf-annot-add-strikeout-markup-annotation
           :which-key "add strikeout markup")
   "amu" '(pdf-annot-add-underline-markup-annotation
           :which-key "add underline markup")
   "at" '(pdf-annot-add-text-annotation :which-key "add text"))
  (major-mode-menu-def
   :keymaps 'pdf-misc-minor-mode-map
   "m" '(pdf-misc-display-metadata :which-key "show metadata")))

(provide 'emacs-rc-pdf)
;;; emacs-rc-pdf.el ends here
