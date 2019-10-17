;;; system.el --- Set system config values
;;;
;;; Commentary:
;;;
;;; Configurations here defines system settings that make things work better in
;;; Emacs.
;;;
;;; Code:

(setq shell-file-name (gethash "sh" (gethash "paths" nix-config)))

(provide 'system)
;;; system.el ends here
