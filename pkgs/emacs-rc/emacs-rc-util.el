;;; emacs-rc-util.el --- Shared utilities
;;;
;;; Commentary:
;;;
;;; Utilities shared between different configuration modules
;;;
;;; Code:

(defface ivy-thing
  '((t :inherit escape-glyph))
  "Face used by ivy for the thing being matched."
  :group 'ivy-faces)

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


(provide 'emacs-rc-util)
;;; emacs-rc-util.el ends here
