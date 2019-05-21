;;; help.el --- Configuration for help tools
;;;
;;; Commentary:
;;;
;;; This module sets up tooling to help discover keybindings,
;;; functions, variables, etc.
;;;
;;; Code:

;; Set up which-key to discover keybindings
(use-package which-key
  :demand
  :delight
  :init (setq which-key-idle-delay 0.25)
  :config (which-key-mode)
  :general ('(normal motion emacs)
            "?" #'which-key-show-top-level
            "SPC ?" '(which-key-show-top-level
                      :which-key "Show all keybindings")))

;; Use this face for highlighting the current match item in custom ivy
;; transformers.
(defface ivy-thing
  '((t :inherit escape-glyph))
  "Face used by ivy for the thing being matched."
  :group 'ivy-faces)

;; Define a few transformers that will be used to make ivy nicer for
;; counsel and helpful commands.
(defun transform-func (cmd)
  "Add keybinding and doc string to CMD."
  (with-temp-buffer
    (let* ((cmdsym (intern cmd))
           (key (key-description (where-is-internal cmdsym nil t)))
           (doc (or (elisp--docstring-first-line
                     (documentation cmdsym))
                    "")))
      (insert (format "%-15s%-40s%s" key cmd doc))
      (put-text-property 1 15 'face 'counsel-key-binding)
      (put-text-property 15 55 'face 'ivy-thing)
      (buffer-string))))
(defun transform-var (variable)
  "Add doc string to VARIABLE."
  (with-temp-buffer
    (let ((doc (or (elisp--docstring-first-line
                    (documentation-property (intern variable)
                                            'variable-documentation))
                   "")))
      (insert (format "%-40s%s" variable doc))
      (put-text-property 1 40 'face 'ivy-thing)
      (buffer-string))))

;; Turn on counsel for better ivy integration for M-x,
;; describe-function, and describe-variable
(use-package counsel
  :delight
  :demand
  :after ivy
  :config
  (setq counsel-ag-base-command (concat (cdr (assoc "ag" paths))
                                        " --nocolor --nogroup --hidden %s"))
  (ivy-set-display-transformer 'counsel-M-x #'transform-func)
  (counsel-mode)
  :general ('(normal motion emacs)
            :prefix "SPC"
            "SPC" '(counsel-M-x :which-key "Run")
            "ep" '(counsel-yank-pop :which-key "kill ring")))

;; Turn on helpful for more helpful description buffers
(use-package helpful
  :config
  (ivy-set-display-transformer 'helpful-callable #'transform-func)
  (ivy-set-display-transformer 'helpful-variable #'transform-var)
  :general ('(normal motion emacs)
            :prefix "SPC h"
            "p" '(helpful-at-point
                  :which-key "describe thing at point")
            "f" '(helpful-callable :which-key "describe function")
            "v" '(helpful-variable :which-key "describe variable")))

;; Turn on swiper for a better search
(use-package swiper
  :after ivy
  :general ('(normal motion emacs)
            "/" '(swiper :which-key "search")
            "SPC sf" '(swiper :which-key "search in file")))

(provide 'help)
;;; help.el ends here
