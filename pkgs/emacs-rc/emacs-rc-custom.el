;;; emacs-rc-custom.el --- Customization settings
;;;
;;; Commentary:
;;;
;;; Customization settings used across the config that should be set by
;;; `init.el' before requiring `emacs-rc'.
;;;
;;; Code:

(defvar rg-path "/usr/bin/rg"
  "Path to the ripgrep executable.")

(defvar git-path "/usr/bin/git"
  "Path to git executable.")

(defvar browse-path "/usr/bin/chromium"
  "Path to the web browser executable.")

(defvar msmtp-path "/usr/bin/msmtp"
  "Path to the msmtp executable.")

(defvar shell-path (getenv "SHELL")
  "Path to the shell executable.")

(defvar ispell-path "/usr/bin/ispell"
  "Path to the Ispell executable.")

(defvar editorconfig-path "/usr/bin/editorconfig"
  "Path to the editorconfig executable.")

(defvar mu-path "/usr/bin/mu"
  "Path to the mu executable.")

(defvar emoji-sets-path "~/.emacs.d/emoji"
  "Path to the emoji sets.")

(defvar font-face "DejaVu Sans Mono"
  "Primary font face.")

(defvar font-size 12
  "Primary font size.")

(provide 'emacs-rc-custom)
;;; emacs-rc-custom.el ends here
