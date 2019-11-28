;;; completion.el --- Completion configuration
;;;
;;; Commentary:
;;;
;;; This module sets up a completion system.
;;;
;;; Code:

;; Turn on in-buffer completion
(use-package company
  :demand
  :delight
  :general ("C-SPC" #'company-complete)
  :config
  (global-company-mode)

  (defun toggle-fci-mode (command)
    (when (string= "show" command)
      (turn-off-fci-mode))
    (when (string= "hide" command)
      (turn-on-fci-mode))
    )

  (defun unset-company-maps (&rest unused)
    "Set default mappings (outside of company)."
    (general-def '(insert) 'override
      "C-/" nil
      "C-n" nil
      "C-p" nil
      "C-j" nil
      "C-k" nil
      "C-l" nil
      "RET" nil))

  (defun set-company-maps (&rest unused)
    "Set maps for when you're inside company completion."
    (general-def '(insert) 'override
      "C-/" #'company-search-mode
      "C-n" #'company-select-next
      "C-j" #'company-select-next
      "C-p" #'company-select-previous
      "C-k" #'company-select-previous
      "C-l" #'company-complete-selection
      "RET" #'company-complete-selection))

  ;; (advice-add 'company-call-frontends :before #'toggle-fci-mode)
  (add-hook 'company-completion-started-hook #'set-company-maps)
  (add-hook 'company-completion-finished-hook #'unset-company-maps)
  (add-hook 'company-completion-cancelled-hook #'unset-company-maps))

;; Turn on a help screen for the highlighted completion element
(use-package company-quickhelp
  :delight
  :after company
  :config (company-quickhelp-mode))

;; Turn on emoji completion
(use-package company-emoji
  :after company
  :config (add-to-list 'company-backends 'company-emoji))

;; Use Ivy for minibuffer completion
(use-package ivy
  :delight
  :init (setq ivy-use-virtual-buffers t
              ivy-format-function 'ivy-format-function-line
              ivy-initial-inputs-alist nil
              ivy-re-builders-alist '((t . ivy--regex-plus)))
  :config (ivy-mode))

(provide 'completion)
;;; completion.el ends here
