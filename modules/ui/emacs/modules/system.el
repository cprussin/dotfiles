;;; system.el --- Set system config values
;;;
;;; Commentary:
;;;
;;; Configurations here defines system settings that make things work better in
;;; Emacs.
;;;
;;; Code:

(setq shell-file-name (cdr (assoc "sh" paths)))

(provide 'system)
;;; system.el ends here
