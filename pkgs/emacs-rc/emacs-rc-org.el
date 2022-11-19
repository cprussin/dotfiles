;;; emacs-rc-org.el --- Configuration for org-mode
;;;
;;; Commentary:
;;;
;;; Configuration org-mode and related packages.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package emacs-rc-keybindings
  :demand
  :commands general-define-key)
(use-package emacs-rc-text) ;; for emojify

;; Set up org-mode
(use-package org
  :demand
  :after general emojify evil evil-collection
  :commands org-get-category
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
  (edit-menu-def
   "o" '(:ignore t :which-key "Org links")
   "oy" '(org-store-link :which-key "yank")
   "op" '(org-insert-link :which-key "insert")))

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
        org-agenda-files (list "~/Notes")
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
  :general (apps-menu-def "a" '(org-agenda :which-key "Agenda")))

(use-package emacs-rc-org-roam
  :demand
  :commands
  org-roam-db-autosync-mode emacs-rc--org-roam-tasks-update-tag
  emacs-rc--org-roam-agenda-files-update
  :custom (org-roam-directory (file-truename "~/Notes"))
  :hook ((find-file before-save) . emacs-rc--org-roam-tasks-update-tag)
  :init
  (setq org-agenda-prefix-format
        '((agenda . " %i %(emacs-rc--org-roam-agenda-category 16)   %?-12t% s")
          (todo . " %i %(emacs-rc--org-roam-agenda-category 16)   ")
          (tags . " %i %(emacs-rc--org-roam-agenda-category 16)   ")
          (search . " %i %(emacs-rc--org-roam-agenda-category 16)   ")))
  :config
  (advice-add 'org-agenda :before #'emacs-rc--org-roam-agenda-files-update)
  (advice-add 'org-todo-list :before #'emacs-rc--org-roam-agenda-files-update)
  (advice-add 'cfw:open-org-calendar :before #'emacs-rc--org-roam-agenda-files-update)
  :general
  (apps-menu-def
    "r" '(:ignore t :which-key "Org Roam")
    "rc" '(org-roam-capture :which-key "capture")
    "re" '(org-roam-extract-subtree :which-key "extract")
    "rf" '(org-roam-node-find :which-key "find")
    "ri" '(org-roam-node-insert :which-key "insert")
    "rg" '(org-roam-graph :which-key "graph")
    "rb" '(:ignore t :which-key "Roam Buffer")
    "rbp" '(org-roam-buffer-toggle :which-key "follow point")
    "rbd" '(org-roam-buffer-display-dedicated :which-key "dedicated")))

(use-package evil-org
  :delight
  :after delight general org
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme)))

(use-package evil-org-agenda
  :after evil-org
  :commands evil-org-agenda-set-keys
  :hook (org-agenda-mode . evil-org-agenda-set-keys))

;; Use pretty bullets in org-mode
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package calfw
  :init
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓))

(use-package calfw-org
  :after calfw org
  :general (apps-menu-def "m" '(cfw:open-org-calendar :which-key "Calendar")))

(provide 'emacs-rc-org)
;;; emacs-rc-org.el ends here
