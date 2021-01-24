;;; emacs-rc-custom.el --- Customization settings
;;;
;;; Commentary:
;;;
;;; Customization settings used across the config that should be set by
;;; `init.el' before requiring `emacs-rc'.
;;;
;;; Code:

(defgroup emacs-rc nil
  "Emacs configuration file"
  :group 'environment
  :prefix "emacs-rc-")

(defcustom emacs-rc-rg-path "/usr/bin/rg"
  "Path to the ripgrep executable."
  :type 'string
  :group 'emacs-rc)

(defcustom emacs-rc-git-path "/usr/bin/git"
  "Path to git executable."
  :type 'string
  :group 'emacs-rc)

(defcustom emacs-rc-browse-path "/usr/bin/chromium"
  "Path to the web browser executable."
  :type 'string
  :group 'emacs-rc)

(defcustom emacs-rc-msmtp-path "/usr/bin/msmtp"
  "Path to the msmtp executable."
  :type 'string
  :group 'emacs-rc)

(defcustom emacs-rc-shell-path (getenv "SHELL")
  "Path to the shell executable."
  :type 'string
  :group 'emacs-rc)

(defcustom emacs-rc-ispell-path "/usr/bin/ispell"
  "Path to the Ispell executable."
  :type 'string
  :group 'emacs-rc)

(defcustom emacs-rc-editorconfig-path "/usr/bin/editorconfig"
  "Path to the editorconfig executable."
  :type 'string
  :group 'emacs-rc)

(defcustom emacs-rc-mu-path "/usr/bin/mu"
  "Path to the mu executable."
  :type 'string
  :group 'emacs-rc)

(defcustom emacs-rc-emoji-sets-path "~/.emacs.d/emoji"
  "Path to the emoji sets."
  :type 'string
  :group 'emacs-rc)

(defcustom emacs-rc-font-face "DejaVu Sans Mono"
  "Primary font face."
  :type 'string
  :group 'emacs-rc)

(defcustom emacs-rc-font-size 12
  "Primary font size."
  :type 'int
  :group 'emacs-rc)

(provide 'emacs-rc-custom)
;;; emacs-rc-custom.el ends here
