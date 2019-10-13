;;; init.el --- Emacs configuration
;;;
;;; Commentary:
;;;
;;; Emacs configuration entrypoint.
;;;
;;; Code:

(require 'package)
(setq package-archives nil
      package-enable-at-startup nil)
(package-initialize)

(require 'use-package)

(defun load-module (module)
  "Load MODULE from the `user-emacs-directory`."
  (load (concat user-emacs-directory module)))

;; Load keybindings first, because later calls to `use-package' depend
;; on tooling enabled here
(load-module "modules/ui/keybindings")

;; General configuration
(load-module "modules/files")
(load-module "modules/safe")
(load-module "modules/startup")
(load-module "modules/system")

;; Tooling
(load-module "modules/tools/git")
(load-module "modules/tools/help")
(load-module "modules/tools/projects")
(load-module "modules/tools/undo-tree")

;; Filetype or app specific configuration
(load-module "modules/modes/code")
(load-module "modules/modes/directories")
(load-module "modules/modes/email")
(load-module "modules/modes/js")
(load-module "modules/modes/markdown")
(load-module "modules/modes/mmm")
(load-module "modules/modes/org")
(load-module "modules/modes/pdf")
(load-module "modules/modes/purescript")
(load-module "modules/modes/shell")
(load-module "modules/modes/text")

;; UI configuration
(load-module "modules/ui/buffers")
(load-module "modules/ui/completion")
(load-module "modules/ui/fonts")
(load-module "modules/ui/gui")
(load-module "modules/ui/images")
(load-module "modules/ui/imenu-list")
(load-module "modules/ui/links")
(load-module "modules/ui/modeline")
(load-module "modules/ui/theme")
(load-module "modules/ui/zooming")

;;; init.el ends here
