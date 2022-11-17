;;; emacs-rc-org.el --- Configuration for org-mode
;;;
;;; Commentary:
;;;
;;; Configuration org-mode and related packages.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(require 'emacs-rc-keybindings)
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
   "p" '(org-insert-link :which-key "insert"))
  ('(normal motion emacs)
   :prefix "SPC o"
   "" '(:ignore t :which-key "Org")))

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
  :general
  ('(normal motion emacs)
   "SPC aa" '(org-agenda :which-key "Agenda")))

(use-package org-roam
  :demand
  :custom (org-roam-directory (file-truename "~/Notes"))
  :config
  (org-roam-db-autosync-mode)

  ;;;
  ;;; Shamelessly stolen from vulpea!
  ;;;
  ;;; TODO should just use vulpea (but I think a lot of this is just on
  ;;; https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html)
  ;;; and isn't even in the library
  ;;;
  (defun org-roam-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                               (point-max) t)
        (buffer-substring-no-properties
         (match-beginning 1)
         (match-end 1)))))

  (defun org-roam-agenda-category (&optional len)
    "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(org-roam-agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
    (let* ((file-name (when buffer-file-name
                        (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name))))
           (title (org-roam-buffer-prop-get "title"))
           (category (org-get-category))
           (result
            (or (if (and
                     title
                     (string-equal category file-name))
                    title
                  category)
                "")))
      (if (numberp len)
          (s-truncate len (s-pad-right len " " result))
        result)))

  (setq org-agenda-prefix-format
        '((agenda . " %i %(org-roam-agenda-category 16)   %?-12t% s")
          (todo . " %i %(org-roam-agenda-category 16)   ")
          (tags . " %i %(org-roam-agenda-category 16)   ")
          (search . " %i %(org-roam-agenda-category 16)   ")))

  (defun org-roam-buffer-prop-set (name value)
    "Set a file property called NAME to VALUE in buffer file.
If the property is already set, replace its value."
    (setq name (downcase name))
    (org-with-point-at 1
      (let ((case-fold-search t))
        (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                               (point-max) t)
            (replace-match (concat "#+" name ": " value) 'fixedcase)
          (while (and (not (eobp))
                      (looking-at "^[#:]"))
            (if (save-excursion (end-of-line) (eobp))
                (progn
                  (end-of-line)
                  (insert "\n"))
              (forward-line)
              (beginning-of-line)))
          (insert "#+" name ": " value "\n")))))

  (defun org-roam-buffer-tags-set (&rest tags)
    "Set TAGS in current buffer.
If filetags value is already set, replace it."
    (org-roam-buffer-prop-set "filetags" (string-join tags " ")))


  (defun org-roam-buffer-tags-get ()
    "Return filetags value in current buffer."
    (org-roam-buffer-prop-get-list "filetags" " "))

  (defun org-roam-buffer-prop-get-list (name &optional separators)
    "Get a buffer property NAME as a list using SEPARATORS.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
    (let ((value (org-roam-buffer-prop-get name)))
      (when (and value (not (string-empty-p value)))
        (split-string-and-unquote value separators))))

  (defun org-roam-tasks-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find                                 ; (3)
     (lambda (type)
       (eq type 'todo))
     (org-element-map                         ; (2)
         (org-element-parse-buffer 'headline) ; (1)
         'headline
       (lambda (h)
         (org-element-property :todo-type h)))))

  (defun org-roam-tasks-update-tag ()
    "Update TASKS tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (org-roam-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (org-roam-buffer-tags-get))
               (original-tags tags))
          (if (org-roam-tasks-p)
              (setq tags (cons "tasks" tags))
            (setq tags (remove "tasks" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'org-roam-buffer-tags-set tags))))))

  (defun org-roam-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun org-roam-task-files ()
    "Return a list of note files containing 'tasks' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag (quote "%\"tasks\"%"))]))))

  (defun org-roam-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (org-roam-task-files)))

  (add-hook 'find-file-hook #'org-roam-tasks-update-tag)
  (add-hook 'before-save-hook #'org-roam-tasks-update-tag)

  (advice-add 'org-agenda :before #'org-roam-agenda-files-update)
  (advice-add 'org-todo-list :before #'org-roam-agenda-files-update)
  (advice-add 'cfw:open-org-calendar :before #'org-roam-agenda-files-update)
  ;;;
  ;;; End shamelessly stolen from vulpea!
  ;;;

  :general
  ('(normal motion emacs)
   :prefix "SPC or"
   "" '(:ignore t :which-key "Roam")
   "c" '(org-roam-capture :which-key "capture")
   "e" '(org-roam-extract-subtree :which-key "extract")
   "f" '(org-roam-node-find :which-key "find")
   "i" '(org-roam-node-insert :which-key "insert")
   "g" '(org-roam-graph :which-key "graph")
   "b" '(:ignore t :which-key "Roam Buffer")
   "bp" '(org-roam-buffer-toggle :which-key "follow point")
   "bd" '(org-roam-buffer-display-dedicated :which-key "dedicated")))

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
  :general
  ('(normal motion emacs)
   :prefix "SPC a"
   "m" '(cfw:open-org-calendar :which-key "Calendar")))

(provide 'emacs-rc-org)
;;; emacs-rc-org.el ends here
