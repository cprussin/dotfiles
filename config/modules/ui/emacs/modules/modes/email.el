;;; email.el --- Configuration for emails
;;;
;;; Commentary:
;;;
;;; Configurations here set up apps for working with email
;;;
;;; Code:

(defun load-mailbox (mbname)
  "Return a lambda to load the inbox for MBNAME."
  `(lambda ()
     (interactive)
     (mu4e~headers-jump-to-maildir (concat "/" ,mbname "/Inbox"))))

(use-package mu4e
  :demand
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands mu4e~headers-jump-to-maildir
  :config
  (defun make-mail-context (ctx email &optional folder)
    (let ((maildir (concat "/" (or folder ctx))))
      (make-mu4e-context
       :name ctx
       :match-func `(lambda (msg)
                      (when msg
                        (string-match-p
                         (concat "^" ,maildir)
                         (mu4e-message-field msg :maildir))))
       :vars `((user-mail-address . ,email)
               (mu4e-sent-folder . ,(concat maildir "/Archive"))
               (mu4e-drafts-folder . ,(concat maildir "/Drafts"))
               (mu4e-trash-folder . ,(concat maildir "/Archive"))
               (mu4e-refile-folder . ,(concat maildir "/Archive"))))))

  (defun view-message-in-external-browser ()
    (interactive)
    (mu4e-action-view-in-browser (mu4e-message-at-point)))

  (setq mail-user-agent 'mu4e-user-agent
        user-full-name "Connor Prussin"
        message-sendmail-f-is-evil 't
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program (gethash "msmtp" (gethash "paths" nix-config))
        mu4e-completing-read-function 'completing-read
        mu4e-get-mail-command "checkmail"
        mu4e-view-show-images t
        mu4e-use-fancy-chars t
        mu4e-view-prefer-html t
        mu4e-view-show-addresses t
        mu4e-hide-index-messages t
        mu4e-index-cleanup nil
        mu4e-change-filenames-when-moving t
        mu4e-headers-include-related nil
        mu4e-maildir (expand-file-name "~/Mail")
        mu4e-user-mail-address-list '("connor@prussin.net"
                                      "cprussin@bci-incorporated.com"
                                      "cprussin@netflix.com"
                                      "cprussin@gmail.com")
        mu4e-contexts `(,(make-mail-context "PrussinNet" "connor@prussin.net")
                        ,(make-mail-context
                          "BCI Incorporated"
                          "cprussin@bci-incorporated.com"
                          "PrussinNet")
                        ,(make-mail-context "Neflix" "cprussin@netflix.com")
                        ,(make-mail-context "GMail" "cprussin@gmail.com")))
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
   "n" `(,(load-mailbox "Netflix") :which-key "Netflix")
   "p" `(,(load-mailbox "PrussinNet") :which-key "PrussinNet"))
  (mu4e-headers-mode-map
   :prefix "SPC m"
   "" '(:ignore t :which-key "Major Mode (Email)")
   "s" '(mu4e-mark-execute-all :which-key "Save changes")))

(use-package shr
  :demand
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

(provide 'email)
;;; email.el ends here
