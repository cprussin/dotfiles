;;; emacs-rc-org.el --- Configuration for org-mode
;;;
;;; Commentary:
;;;
;;; Configuration org-mode and related packages.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-text) ;; for emojify

;; Set up org-mode
(use-package org
  :demand
  :after emojify
  :hook (org-mode . prettify-org)
  :config
  (defun prettify-org ()
    (push '("[ ]" . "☐") prettify-symbols-alist)
    (push '("[X]" . "☑") prettify-symbols-alist)
    (push '("[-]" . "❍") prettify-symbols-alist)
    (prettify-symbols-mode))
  ;; Make checked checklist entries use the `org-headline-done' face
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
   'append)
  (setq org-tags-column 0
        org-log-done 'time
        org-log-repeat nil
        org-agenda-files (list "~/Notes/Personal.org")
        org-fontify-done-headline t)
  ;; Disable emoji in org-mode since they mess with my prettier checklists and I
  ;; hardly ever use emoji in org docs anyways
  (push 'org-mode emojify-inhibit-major-modes)
  :general
  ('(normal motion emacs)
   "SPC aa" '(org-agenda :which-key "Agenda"))
  ('(normal motion emacs)
   :prefix "SPC eo"
   "" '(:ignore t :which-key "Org links")
   "y" '(org-store-link :which-key "yank")
   "p" '(org-insert-link :which-key "insert")))

(use-package org-agenda
  :config (setq org-agenda-window-setup 'only-window
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
                                             ("ppu" tags-todo  "-shakti-720_Natoma_Drive-2019_Subaru_Ascent-SCHEDULED={.+}"))))

(use-package evil-org
  :delight
  :after org delight
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

(provide 'emacs-rc-org)
;;; emacs-rc-org.el ends here
