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

(defcustom emacs-rc-color-theme-link nil
  "The color theme's link to the variant the desktop shows, or nil.

Its `name' file holds the theme's name, as `<family>/<variant>'.  Without
one the theme stays dark."
  :type '(choice (const nil) directory)
  :group 'emacs-rc)

(provide 'emacs-rc-custom)
;;; emacs-rc-custom.el ends here
