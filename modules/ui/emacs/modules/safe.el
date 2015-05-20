;;; safe.el --- Set safety values
;;;
;;; Commentary:
;;;
;;; Configurations here defines secure values for Emacs.
;;;
;;; Code:

(setq
 safe-local-variable-values
 '((psc-ide-output-directory . "out/build/")
   (psc-ide-source-globs "src/**/*.purs"
                         "out/components/purescript-*/src/**/*.purs")))

(provide 'safe)
;;; safe.el ends here
