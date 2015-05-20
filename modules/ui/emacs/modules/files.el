;;; files.el --- Configurations for file handling
;;;
;;; Commentary:
;;;
;;; Configurations here modify how Emacs deals with temporary files,
;;; autosave files, backup files, etc
;;;
;;; Code:

;; Stop making lockfiles and autosave folder
(setq create-lockfiles nil
      auto-save-list-file-prefix nil)

;; Disable backup and autosave files
(use-package files
  :config (setq make-backup-files nil))

(provide 'files)
;;; files.el ends here
