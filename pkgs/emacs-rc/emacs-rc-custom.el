;;; emacs-rc-custom.el --- Customization settings
;;;
;;; Commentary:
;;;
;;; Customization settings used across the config that should be set by
;;; `init.el' before requiring `emacs-rc'.
;;;
;;; Code:

(defgroup emacs-rc nil
  "Emacs configuration file."
  :group 'environment
  :prefix "emacs-rc-")

(defcustom emacs-rc-browse-path "chromium"
  "Path to the web browser executable."
  :type 'string
  :group 'emacs-rc)

(provide 'emacs-rc-custom)
;;; emacs-rc-custom.el ends here
