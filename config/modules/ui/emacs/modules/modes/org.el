;;; directories.el --- Configurations for org-mode
;;;
;;; Commentary:
;;;
;;; Configurations here apply to org files.
;;;
;;; Code:

;; Set up org-mode
(use-package org
  :demand
  :init (setq org-tags-column 0
              org-log-done 'time
              org-log-repeat nil
              org-agenda-files (list "~/Notes/Personal.org")
              org-agenda-window-setup 'only-window
              org-agenda-custom-commands '(("p" . "Personal searches")

                                           ("pc" . "2019 Subaru Ascent")
                                           ("pca" tags-todo  "+2019_Subaru_Ascent")
                                           ("pcr" tags-todo  "+2019_Subaru_Ascent+SCHEDULED={.+\\+.+}")
                                           ("pcs" tags-todo  "+2019_Subaru_Ascent+SCHEDULED={^[^\\+]+$}")
                                           ("pcu" tags-todo  "+2019_Subaru_Ascent-SCHEDULED={.+}")

                                           ("ph" . "720 Natoma Drive")
                                           ("pha" tags-todo  "+720_Natoma_Drive")
                                           ("phr" tags-todo  "+720_Natoma_Drive+SCHEDULED={.+\\+.+}")
                                           ("phs" tags-todo  "+720_Natoma_Drive+SCHEDULED={^[^\\+]+$}")
                                           ("phu" tags-todo  "+720_Natoma_Drive-SCHEDULED={.+}")

                                           ("ps" . "Shakti")
                                           ("psa" tags-todo  "+shakti")
                                           ("psr" tags-todo  "+shakti+SCHEDULED={.+\\+.+}")
                                           ("pss" tags-todo  "+shakti+SCHEDULED={^[^\\+]+$}")
                                           ("psu" tags-todo  "+shakti-SCHEDULED={.+}")

                                           ("pp" . "Other")
                                           ("ppa" tags-todo "-shakti-720_Natoma_Drive-2019_Subaru_Ascent")
                                           ("ppr" tags-todo  "-shakti-720_Natoma_Drive-2019_Subaru_Ascent+SCHEDULED={.+\\+.+}")
                                           ("pps" tags-todo  "-shakti-720_Natoma_Drive-2019_Subaru_Ascent+SCHEDULED={^[^\\+]+$}")
                                           ("ppu" tags-todo  "-shakti-720_Natoma_Drive-2019_Subaru_Ascent-SCHEDULED={.+}")

                                           ))
  :general
  ('(normal motion emacs)
   "SPC aa" '(org-agenda :which-key "Agenda"))
  ('(normal motion emacs)
   :prefix "SPC eo"
   "" '(:ignore t :which-key "Org links")
   "y" '(org-store-link :which-key "yank")
   "p" '(org-insert-link :which-key "insert")))
(use-package evil-org
  :delight
  :after org
  :functions evil-org-agenda-set-keys
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Use pretty bullets in org-mode
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(provide 'org)
;;; org.el ends here
