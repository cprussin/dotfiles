;;; emacs-rc-email.el --- Configuration for email
;;;
;;; Commentary:
;;;
;;; Configuration for mu4e and other email-related packages.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-custom)

(use-package mu4e
  :demand
  :commands mu4e-action-view-in-browser mu4e~start
  :preface
  (defun load-mailbox (mbname)
    "Return a lambda to load the inbox for MBNAME."
    `(lambda ()
       (interactive)
       (mu4e~headers-jump-to-maildir (concat "/" ,mbname "/Inbox"))))

  (defun make-mail-context (ctx email &optional folder)
    (let ((maildir (concat "/" (or folder ctx))))
      (make-mu4e-context
       :name ctx
       :match-func `(lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches msg '(:to :from :cc :bcc) ,email)))
       :vars `((user-mail-address . ,email)
               (mu4e-sent-folder . ,(concat maildir "/Archive"))
               (mu4e-drafts-folder . ,(concat maildir "/Drafts"))
               (mu4e-trash-folder . ,(concat maildir "/Archive"))
               (mu4e-refile-folder . ,(concat maildir "/Archive"))))))
  :config
  (defun view-message-in-external-browser ()
    (interactive)
    (mu4e-action-view-in-browser (mu4e-message-at-point)))

  (setq mail-user-agent 'mu4e-user-agent
        user-full-name "Connor Prussin"
        message-sendmail-f-is-evil 't
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program emacs-rc-msmtp-path
        mu4e-completing-read-function 'completing-read
        mu4e-compose-format-flowed t
        visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
        mu4e-view-show-images t
        mu4e-use-fancy-chars t
        mu4e-view-prefer-html t
        mu4e-view-show-addresses t
        mu4e-hide-index-messages t
        mu4e-change-filenames-when-moving t
        mu4e-headers-include-related nil
        mu4e-mu-binary emacs-rc-mu-path
        mu4e-contexts `(,(make-mail-context "PrussinNet" "connor@prussin.net")
                        ,(make-mail-context
                          "BCI Incorporated"
                          "cprussin@bci-incorporated.com"
                          "PrussinNet")
                        ,(make-mail-context "GMail" "cprussin@gmail.com"))
        mu4e-context-policy 'pick-first)
  (mu4e~start)
  :general
  ('(normal) mu4e-headers-mode-map
   "?" nil
   "O" '(view-message-in-external-browser :which-key "View in browser")
   "d" #'mu4e-headers-mark-for-refile)
  ('(normal) mu4e-view-mode-map
   "?" nil
   "C-c C-o" #'mu4e~view-open-attach-from-binding
   "O" '(view-message-in-external-browser :which-key "View in browser")
   "d" #'mu4e-view-mark-for-refile
   "v" nil)
  ('(normal motion emacs)
   :prefix "SPC am"
   "" '(:ignore t :which-key "email")
   "g" `(,(load-mailbox "GMail") :which-key "GMail")
   "p" `(,(load-mailbox "PrussinNet") :which-key "PrussinNet"))
  (mu4e-headers-mode-map
   :prefix "SPC m"
   "" '(:ignore t :which-key "Major Mode (Email)")
   "s" '(mu4e-mark-execute-all :which-key "Save changes")))

(use-package shr
  :demand
  :commands shr-colorize-region
  :config (advice-add #'shr-colorize-region
                      :filter-args
                      (lambda (args)
                        (if (> (length args) 3) (butlast args) args)))
  :general (shr-map
            "I" nil
            "O" nil
            "a" nil
            "i" nil
            "u" nil
            "v" nil
            "w" nil
            "z" nil
            "C-c C-o" #'shr-browse-url))

(use-package org-mu4e)

(provide 'emacs-rc-email)
;;; emacs-rc-email.el ends here
