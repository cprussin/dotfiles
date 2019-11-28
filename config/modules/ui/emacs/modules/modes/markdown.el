;;; markdown.el --- Configurations for markdown files
;;;
;;; Commentary:
;;;
;;; Configurations here apply to markdown files.
;;;
;;; Code:

(use-package markdown-mode
  :init (setq markdown-hide-markup t
              markdown-asymmetric-header t
              markdown-fontify-code-blocks-natively t)
  :general
  ('(normal motion emacs) markdown-mode-map
   "{" #'markdown-backward-block
   "}" #'markdown-forward-block
   "<<" #'markdown-promote
   ">>" #'markdown-demote
   "C-k" #'markdown-move-up
   "C-j" #'markdown-move-down)
  ('(visual) markdown-mode-map
   ">" #'markdown-indent-region
   "<" #'markdown-outdent-region)
  ('(normal motion emacs) markdown-mode-map
   :prefix "SPC m"
   "" '(:ignore t :which-key "Major Mode (Markdown)")
   "'" '(markdown-edit-code-block :which-key "edit code")
   "]" '(markdown-complete :which-key "complete")
   "C-]" '(markdown-complete-buffer :which-key "complete buffer")

   "i" '(:ignore t :which-key "Insert")
   "i`" '(markdown-electric-bacquote :which-key "backquote")
   "i-" '(markdown-insert-hr :which-key "hr")
   "i[" '(markdown-insert-gfm-checkbox :which-key "checkbox")
   "ib" '(markdown-insert-bold :which-key "bold")
   "iC" '(markdown-insert-gfm-code-block
          :which-key "code block")
   "ic" '(markdown-insert-code :which-key "code")
   "if" '(markdown-insert-footnote :which-key "footnote")
   "ih" '(:ignore t :which-key "Header")
   "ih1" '(markdown-insert-header-atx-1 :which-key "1")
   "ih2" '(markdown-insert-header-atx-2 :which-key "2")
   "ih3" '(markdown-insert-header-atx-3 :which-key "3")
   "ih4" '(markdown-insert-header-atx-4 :which-key "4")
   "ih5" '(markdown-insert-header-atx-5 :which-key "5")
   "ih6" '(markdown-insert-header-atx-6 :which-key "6")
   "ihU" '(markdown-insert-header-setext-1
           :which-key "underlined (L1)")
   "ihu" '(markdown-insert-header-setext-2
           :which-key "underlined (L2)")
   "iI" '(markdown-insert-image :which-key "image")
   "ii" '(markdown-insert-italic :which-key "italic")
   "ik" '(markdown-insert-kbd :which-key "kbd")
   "iL" '(markdown-insert-list-item :which-key "list item")
   "il" '(markdown-insert-link :which-key "link")
   "iP" '(markdown-pre-region :which-key "pre region")
   "ip" '(markdown-insert-pre :which-key "pre")
   "iQ" '(markdown-blockquote-region
          :which-key "blockquote region")
   "iq" '(markdown-insert-blockquote :which-key "blockquote")
   "is" '(markdown-insert-strike-through
          :which-key "strikethrough")
   "iw" '(markdown-insert-wiki-link :which-key "wiki link")

   "l" '(:ignore t :which-key "Link")
   "ln" '(markdown-next-link :which-key "next")
   "lp" '(markdown-previous-link :which-key "previous")

   "m" '(:ignore t :which-key "Mark")
   "mb" '(markdown-mark-block :which-key "block")
   "mt" '(markdown-mark-subtree :which-key "subtree")

   "n" '(markdown-cleanup-list-numbers
         :which-key "cleanup list numbers")

   "r" '(:ignore t :which-key "Refs")
   "rc" '(markdown-check-refs :which-key "check")
   "ru" '(markdown-unused-refs :which-key "unused")

   "s" '(:ignore t :which-key "Switches")
   "s RET" '(markdown-toggle-markup-hiding
             :which-key "markup hiding")
   "sm" '(markdown-toggle-math :which-key "math")
   "sc" '(markdown-toggle-fontify-code-blocks-natively
          :which-key "code highlighting")
   "si" '(markdown-toggle-inline-images :which-key "images")
   "su" '(markdown-toggle-url-hiding :which-key "urls")

   "t" '(:ignore t :which-key "Tables")
   "tc" '(:ignore t :which-key "Column")
   "tcd" '(markdown-table-delete-column :which-key "delete")
   "tci" '(markdown-table-insert-column :which-key "insert")
   "tR" '(markdown-table-convert-region :which-key "convert region")
   "tr" '(:ignore t :which-key "Row")
   "trd" '(markdown-table-delete-row :which-key "delete")
   "tri" '(markdown-table-insert-row :which-key "insert")
   "ts" '(markdown-table-sort-lines :which-key "sort")
   "tt" '(markdown-table-transpose :which-key "transpose")))

(provide 'markdown)
;;; markdown.el ends here
