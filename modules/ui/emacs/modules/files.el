;;; files.el --- Configurations for file handling
;;;
;;; Commentary:
;;;
;;; Configurations here modify how Emacs deals with temporary files,
;;; autosave files, backup files, etc
;;;
;;; Code:

;; Stop making lockfiles
(setq create-lockfiles nil)

;; Disable backup and autosave files
(use-package files
  :config (setq make-backup-files nil
                auto-save-default nil))

(provide 'files)
;;; files.el ends here
