;;; emacs-rc-org.el --- Helper functions for org-mode
;;;
;;; Commentary:
;;;
;;; This file just wraps org-roam with some utils shamelessly stolen from vulpea
;;; (see
;;; https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html).
;;;
;;; TODO I should probably just use vulpea directly, but I think many of these
;;; helpers aren't even in the library and are just on that blog post.
;;;
;;; Code:

(require 'org-roam)
(require 's)

(defun emacs-rc--org-roam-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun emacs-rc--org-roam-agenda-category (len)
  "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (emacs-rc--org-roam-buffer-prop-get "title"))
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

(defun emacs-rc--org-roam-buffer-prop-set (name value)
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

(defun emacs-rc--org-roam-buffer-tags-set (&rest tags)
  "Set TAGS in current buffer.
If filetags value is already set, replace it."
  (emacs-rc--org-roam-buffer-prop-set "filetags" (string-join tags " ")))


(defun emacs-rc--org-roam-buffer-tags-get ()
  "Return filetags value in current buffer."
  (emacs-rc--org-roam-buffer-prop-get-list "filetags" " "))

(defun emacs-rc--org-roam-buffer-prop-get-list (name &optional separators)
  "Get a buffer property NAME as a list using SEPARATORS.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
  (let ((value (emacs-rc--org-roam-buffer-prop-get name)))
    (when (and value (not (string-empty-p value)))
      (split-string-and-unquote value separators))))

(defun emacs-rc--org-roam-tasks-p ()
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

(defun emacs-rc--org-roam-tasks-update-tag ()
  "Update TASKS tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (org-roam-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (emacs-rc--org-roam-buffer-tags-get))
             (original-tags tags))
        (if (emacs-rc--org-roam-tasks-p)
            (setq tags (cons "tasks" tags))
          (setq tags (remove "tasks" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'emacs-rc--org-roam-buffer-tags-set tags))))))

(defun emacs-rc--org-roam-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun emacs-rc--org-roam-task-files ()
  "Return a list of note files containing `tasks' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
              :from tags
              :left-join nodes
              :on (= tags:node-id nodes:id)
              :where (like tag (quote "%\"tasks\"%"))]))))

(defun emacs-rc--org-roam-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (emacs-rc--org-roam-task-files)))

(provide 'emacs-rc-org-roam)
;;; emacs-rc-org-roam.el ends here
