;;; emacs-rc-files.el --- Configuration for auxiliary files
;;;
;;; Commentary:
;;;
;;; Configuration for how Emacs handles auxiliary files, such as backup files,
;;; etc.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(setq create-lockfiles nil)

(use-package files
  :config (setq make-backup-files nil
                auto-save-default nil))

(provide 'emacs-rc-files)
;;; emacs-rc-files.el ends here
