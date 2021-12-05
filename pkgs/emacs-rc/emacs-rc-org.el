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
  :after general emojify evil evil-collection
  :hook (org-mode . emacs-rc--prettify-org)
  :config
  (defun emacs-rc--prettify-org ()
    "Make `org-mode' prettier."
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
        org-fontify-done-headline t)
  ;; Disable emoji in org-mode since they mess with my prettier checklists and I
  ;; hardly ever use emoji in org docs anyways
  (push 'org-mode emojify-inhibit-major-modes)
  :general
  ('(normal motion emacs)
   :prefix "SPC eo"
   "" '(:ignore t :which-key "Org links")
   "y" '(org-store-link :which-key "yank")
   "p" '(org-insert-link :which-key "insert")))

(use-package org-agenda
  :after general emojify evil evil-collection
  :config
  ;;; ORG-MODE:  * My Task
  ;;;              SCHEDULED: <%%(diary-last-day-of-month date)>
  ;;; DIARY:  %%(diary-last-day-of-month date) Last Day of the Month
  ;;; See also:  (setq org-agenda-include-diary t)
  ;;; (diary-last-day-of-month '(2 28 2017))
  (defun diary-last-day-of-month (date)
    "Return `t` if DATE is the last day of the month."
    (let* ((day (calendar-extract-day date))
           (month (calendar-extract-month date))
           (year (calendar-extract-year date))
           (last-day-of-month
            (calendar-last-day-of-month month year)))
      (= day last-day-of-month)))

  (setq org-agenda-window-setup 'only-window
        org-agenda-files (list "~/Notes/Personal.org")
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

                                     ("pp" . "Other")
                                     ("ppa" tags-todo "-720_Natoma_Drive-2019_Subaru_Ascent")
                                     ("ppr" tags-todo  "-720_Natoma_Drive-2019_Subaru_Ascent+SCHEDULED={.+\\+.+}")
                                     ("pps" tags-todo  "-720_Natoma_Drive-2019_Subaru_Ascent+SCHEDULED={^[^\\+]+$}")
                                     ("ppu" tags-todo  "-720_Natoma_Drive-2019_Subaru_Ascent-SCHEDULED={.+}")))
  :general
  ('(normal motion emacs)
   "SPC aa" '(org-agenda :which-key "Agenda")))

(use-package evil-org
  :delight
  :after delight general org
  :functions evil-org-agenda-set-keys
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme))
  :init
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'motion org-agenda-mode-map (kbd "SPC") nil))

;; Use pretty bullets in org-mode
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(provide 'emacs-rc-org)
;;; emacs-rc-org.el ends here
