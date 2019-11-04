;;; directories.el --- Configurations for directories
;;;
;;; Commentary:
;;;
;;; Configurations here apply to directory listings.
;;;
;;; Code:

;; Use ranger instead of dired
(use-package ranger
  :demand
  :init (setq ranger-cleanup-on-disable nil
              ranger-cleanup-eagerly nil)
  :config (ranger-override-dired-mode)
  :general
  ('(normal motion emacs)
   "SPC ar" '(ranger :which-key "ranger"))
  (ranger-mode-map
   "C-w" nil
   "?" nil
   "M" #'dired-do-chmod
   "C" #'dired-do-copy))

(provide 'directories)
;;; directories.el ends here
