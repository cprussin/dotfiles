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
  :after general emojify evil evil-collection evil-org
  :commands org-get-category org-todo
  :hook (org-mode . emacs-rc--prettify-org)
  :config
  (defun emacs-rc--prettify-org ()
    "Make `org-mode' prettier."
    (push '("[ ]" . "☐") prettify-symbols-alist)
    (push '("[X]" . "☑") prettify-symbols-alist)
    (push '("[-]" . "❍") prettify-symbols-alist)
    (prettify-symbols-mode))
  (defun emacs-rc--org-nextset ()
    (interactive)
    (org-todo 'nextset))
  ;; Make checked checklist entries use the `org-headline-done' face
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
   'append)
  (setq org-tags-column 0
        org-log-done 'time
        org-log-repeat nil
        org-fontify-done-headline t
        org-todo-keywords '((sequence "TODO" "|" "DONE")
                            (sequence "BLOCKED" "|")))
  ;; Disable emoji in org-mode since they mess with my prettier checklists and I
  ;; hardly ever use emoji in org docs anyways
  (push 'org-mode emojify-inhibit-major-modes)
  :general
  (edit-menu-def
    "o" '(:ignore t :which-key "Org links")
    "oy" '(org-store-link :which-key "yank")
    "op" '(org-insert-link :which-key "insert"))
  (general-def
    :keymaps 'org-mode-map
    "C-c C-b" '(emacs-rc--org-nextset :which-key "Toggle blocked")))

(use-package org-agenda
  :after general emojify evil evil-collection evil-org-agenda
  :config
  (setq org-agenda-window-setup 'only-window
        org-agenda-files (list "~/Notes")
        org-agenda-hide-tags-regexp "\\|tasks"
        org-agenda-custom-commands '(("A" "All Tasks" tags-todo "-BLOCKED")
                                     ("a" "Agenda" ((agenda "" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("BLOCKED")))
                                                                (org-agenda-overriding-header "Agenda")))
                                                    (todo "BLOCKED" ((org-agenda-overriding-header "Blocked Tasks")))
                                                    (tags-todo "PRIORITY=\"A\"-SCHEDULED={.+}-DEADLINE={.+}/!-BLOCKED" ((org-agenda-overriding-header "On Deck")))
                                                    (tags-todo "-PRIORITY=\"A\"-SCHEDULED={.+}-DEADLINE={.+}/!-BLOCKED" ((org-agenda-overriding-header "Backlog")))))
                                     ("b" "Blocked Tasks" todo "BLOCKED")
                                     ("r" "Recurring Tasks" tags-todo "SCHEDULED={.+\\+.+}/!-BLOCKED|DEADLINE={.+\\+.+}/!-BLOCKED")
                                     ("s" "Scheduled Tasks" tags-todo "SCHEDULED={^[^\\+]+$}/!-BLOCKED|DEADLINE={^[^\\+]+$}/!-BLOCKED")
                                     ("u" "Unscheduled Tasks" tags-todo "-SCHEDULED={.+}-DEADLINE={.+}/!-BLOCKED")))
  :general
  (apps-menu-def "a" '(org-agenda :which-key "Agenda"))
  (general-def
    :keymaps 'org-agenda-mode-map
    "C-c C-b" '(org-agenda-todo-nextset :which-key "Toggle blocked")))

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
  (org-roam-db-autosync-mode)
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
  :demand
  :delight
  :after delight general
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme)))

(use-package evil-org-agenda
  :demand
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
