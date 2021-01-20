;;; init.el --- Emacs configuration
;;;
;;; Commentary:
;;;
;;; Emacs configuration entrypoint.
;;;
;;; Code:

(defvar git-path "/usr/bin/git"
  "Path to git executable.")

(defvar rg-path "/usr/bin/rg"
  "Path to the ripgrep executable.")

(defvar browse-path "/usr/bin/chromium"
  "Path to the web browser executable.")

(defvar msmtp-path "/usr/bin/msmtp"
  "Path to the msmtp executable.")

(defvar shell-path (getenv "SHELL")
  "Path to the shell executable.")

(defvar ispell-path "/usr/bin/ispell"
  "Path to the Ispell executable.")

;;(defvar editorconfig-path "/usr/bin/editorconfig"
;;  "Path to the editorconfig executable.")

(defvar mu-path "/usr/bin/mu"
  "Path to the mu executable.")

(defvar emoji-sets-path "~/.emacs.d/emoji"
  "Path to the emoji sets.")

(defvar font-face "DejaVu Sans Mono"
  "Primary font face.")

(defvar font-size 12
  "Primary font size.")

(require 'package)
(setq package-archives nil
      package-enable-at-startup nil)
(package-initialize)

(eval-when-compile (require 'use-package))

(use-package evil
  :init (setq evil-want-C-u-scroll t
              evil-want-integration t
              evil-want-keybinding nil)
  :config (evil-mode))

(use-package evil-collection
  :after evil
  :init (setq evil-collection-key-blacklist '("SPC"))
  :config (evil-collection-init))

(use-package evil-goggles
  :after evil
  :delight
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; Now, set up all custom keybindings
(use-package general
  :after evil
  :config

  ;; Make esc abort
  ;;(general-def
  ;;  "<escape>" #'keyboard-escape-quit)

  ;; Set up SPC-leader keys
  (general-def '(normal motion emacs)
    :prefix "SPC"
    "" nil
    "a" '(:ignore t :which-key "Apps")
    "b" '(:ignore t :which-key "Buffers")
    "bm" '(buffer-menu :which-key "menu")
    "bn" '(next-buffer :which-key "next")
    "bp" '(previous-buffer :which-key "previous")
    "bs" '(ivy-switch-buffer :which-key "switch")
    "bx" '(kill-current-buffer t :which-key "close")
    "e" '(:ignore t :which-key "Editing")
    "g" '(:ignore t :which-key "Git")
    "h" '(:ignore t :which-key "Help")
    "p" '(:ignore t :which-key "Project")
    "s" '(:ignore t :which-key "Search")
    "w" '(evil-window-map :which-key "Window")))

;; Stop making lockfiles
(setq create-lockfiles nil)

;; Disable backup and autosave files
(use-package files
  :config (setq make-backup-files nil
                auto-save-default nil))

(setq
 safe-local-variable-values
 '((psc-ide-output-directory . "out/build/")
   (psc-ide-source-globs "src/**/*.purs"
                         "out/components/purescript-*/src/**/*.purs")))

;; Hide startup screen and messages
(setq inhibit-startup-screen t
      inhibit-startup-message t)

(setq shell-file-name shell-path)

(use-package diff-mode)

;; Enable direnv
(use-package direnv
  :after diff-mode
  :config (direnv-mode))

;; Magit!
(use-package magit
  :init
  (setq magit-git-executable git-path)
  :general
  (magit-mode-map
   "SPC" nil
   "?" nil)
  ('(normal motion emacs)
   :prefix "SPC g"
   "c" '(magit-clone :which-key "git clone")
   "b" '(magit-blame :which-key "git blame")
   "l" '(magit-log :which-key "git log")
   "s" '(magit-status :which-key "git status")))
(use-package evil-magit
  :after (magit evil)
  :config (evil-magit-init))

;; Set up which-key to discover keybindings
(use-package which-key
  :demand
  :delight
  :init (setq which-key-idle-delay 0.25)
  :config (which-key-mode)
  :general ('(normal motion emacs)
            "?" #'which-key-show-top-level
            "SPC ?" '(which-key-show-top-level
                      :which-key "Show all keybindings")))

;; Use this face for highlighting the current match item in custom ivy
;; transformers.
(defface ivy-thing
  '((t :inherit escape-glyph))
  "Face used by ivy for the thing being matched."
  :group 'ivy-faces)

;; Define a few transformers that will be used to make ivy nicer for
;; counsel and helpful commands.
(defun transform-func (cmd)
  "Add keybinding and doc string to CMD."
  (with-temp-buffer
    (let* ((cmdsym (intern cmd))
           (key (key-description (where-is-internal cmdsym nil t)))
           (doc (or (elisp--docstring-first-line
                     (documentation cmdsym))
                    "")))
      (insert (format "%-15s%-40s%s" key cmd doc))
      (put-text-property 1 15 'face 'counsel-key-binding)
      (put-text-property 15 55 'face 'ivy-thing)
      (buffer-string))))
(defun transform-var (variable)
  "Add doc string to VARIABLE."
  (with-temp-buffer
    (let ((doc (or (elisp--docstring-first-line
                    (documentation-property (intern variable)
                                            'variable-documentation))
                   "")))
      (insert (format "%-40s%s" variable doc))
      (put-text-property 1 40 'face 'ivy-thing)
      (buffer-string))))

;; Turn on counsel for better ivy integration for M-x,
;; describe-function, and describe-variable
(use-package counsel
  :delight
  :demand
  :after ivy
  :config
  (setq counsel-rg-base-command (concat rg-path
                                        " -M 240 --with-filename --no-heading --line-number --color never %s"))
  (ivy-configure 'counsel-M-x :display-transformer-fn #'transform-func)
  (counsel-mode)
  :general ('(normal motion emacs)
            :prefix "SPC"
            "SPC" '(counsel-M-x :which-key "Run")
            "ep" '(counsel-yank-pop :which-key "kill ring")))

;; Turn on helpful for more helpful description buffers
(use-package helpful
  :config
  (ivy-configure 'helpful-callable :display-transformer-fn #'transform-func)
  (ivy-configure 'helpful-variable :display-transformer-fn #'transform-var)
  :general ('(normal motion emacs)
            :prefix "SPC h"
            "p" '(helpful-at-point
                  :which-key "describe thing at point")
            "f" '(helpful-callable :which-key "describe function")
            "v" '(helpful-variable :which-key "describe variable")))

;; Turn on swiper for a better search
(use-package swiper
  :after ivy
  :general ('(normal motion emacs)
            "/" '(swiper :which-key "search")
            "SPC sf" '(swiper :which-key "search in file")))

(defun git-cmd (args)
  "Return a string representing the git command ARGS."
  (concat git-path " " args))

;; Enable projectile
(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name) " "))
  :init
  (setq projectile-completion-system 'ivy
        projectile-git-command (git-cmd "ls-files -zco --exclude-standard")
        projectile-git-submodule-command (git-cmd "submodule --quiet foreach 'echo $path' | tr '\\n' '\\0'")
        projectile-git-ignored-command (git-cmd "ls-files -zcoi --exclude-standard"))
  :config (projectile-mode))

;; And enable counsel-projectile, for better ivy integration
(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode)
  :general
  ('(normal motion emacs)
   "SPC sp" '(counsel-projectile-rg :which-key "search in project"))
  ('(normal motion emac)
   :prefix "SPC p"
   "a" '(counsel-projectile-org-agenda :which-key "agenda")
   "d" '(counsel-projectile-find-dir :which-key "find directory")
   "f" '(counsel-projectile-find-file :which-key "find file")
   "g" '(counsel-projectile-switch-project :which-key "go to project")
   "o" '(counsel-projectile-org-capture :which-key "capture note")
   "s" '(counsel-projectile-rg :which-key "search")))

;; Turn on undo-tree
(use-package undo-tree
  :delight
  :init (setq undo-tree-visualizer-timestamps t)
  :config (global-undo-tree-mode)
  :general ('(normal motion emacs)
            "SPC et" '(undo-tree-visualize
                       :which-key "undo/redo tree")))

;; Turn on rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight matching parens
(use-package paren
  :hook (prog-mode . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t))

;; Highlight hardcoded numbers and the likes
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; Set up editorconfig
(use-package editorconfig
  :delight
  :config (editorconfig-mode))

;; Aggressively re-indent
(use-package aggressive-indent
  :delight
  :config
  (global-aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'purescript-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode))

;; Show indentation guide
(use-package indent-guide
  :delight
  :config (indent-guide-global-mode))

;; Enable flycheck for source code checks
(use-package flycheck
  :config
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (defun use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (global-eslint (executable-find "eslint"))
           (local-eslint (expand-file-name "node_modules/.bin/eslint" root))
           (eslint (if (file-executable-p local-eslint)
                       local-eslint
                     global-eslint)))
      (setq-local flycheck-javascript-eslint-executable eslint)))
  (add-hook 'web-mode-hook 'use-eslint-from-node-modules)
  (add-hook 'js-jsx-mode-hook 'use-eslint-from-node-modules)
  )
(use-package flycheck-pos-tip
  :ensure t
  :init
  (setq flycheck-pos-tip-timeout 0)
  (flycheck-pos-tip-mode))

;; Enable smarter surrounding pairs
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))
(use-package evil-smartparens
  :after smartparens
  :delight
  :hook (smartparens-enabled . evil-smartparens-mode))

;; Highlight TODO comments
(use-package hl-todo
  :config (global-hl-todo-mode))

;; Use ranger instead of dired
(use-package ranger
  :demand
  :init (setq ranger-cleanup-on-disable nil
              ranger-cleanup-eagerly nil)
  :config (ranger-override-dired-mode)
  :general
  ('(normal motion emacs)
   "SPC ar" '(ranger :which-key "ranger"))
  (ranger-mode-map
   "C-w" nil
   "?" nil
   "M" #'dired-do-chmod
   "C" #'dired-do-copy))

(defun load-mailbox (mbname)
  "Return a lambda to load the inbox for MBNAME."
  `(lambda ()
     (interactive)
     (mu4e~headers-jump-to-maildir (concat "/" ,mbname "/Inbox"))))

(use-package mu4e
  :demand
  :config
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

  (defun view-message-in-external-browser ()
    (interactive)
    (mu4e-action-view-in-browser (mu4e-message-at-point)))

  (setq mail-user-agent 'mu4e-user-agent
        user-full-name "Connor Prussin"
        message-sendmail-f-is-evil 't
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program msmtp-path
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
        mu4e-mu-binary mu-path
        mu4e-maildir (expand-file-name "~/Mail")
        mu4e-user-mail-address-list '("connor@prussin.net"
                                      "cprussin@bci-incorporated.com"
                                      "cprussin@gmail.com")
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

(use-package web-mode
  :config
  (defun prettify-js ()
    (push '("function" . ?λ) prettify-symbols-alist)
    (push '("require" . ?℞) prettify-symbols-alist)
    (push '("return" . ?←) prettify-symbols-alist)
    (push '("null" . ?∅) prettify-symbols-alist)
    (push '("undefined" . ?�) prettify-symbols-alist)
    (push '("=>" . ?→) prettify-symbols-alist)
    (prettify-symbols-mode))

  ;; Make js2 syntax highlighting better
  (setq-default js2-highlight-level 3
                js2-include-browser-externs t
                js2-include-node-externs t)

  ;;;; Prettify js and jsx files
  (add-hook 'web-mode-hook 'prettify-js)
  (add-hook 'web-mode-hook 'turn-off-fci-mode)

  ;; Use rjsx-mode on .js files since it handles flow syntax better and to
  ;; keep consistency
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  )

(use-package typescript-mode
  :config (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))

;;(defun use-flow-from-node-modules ()
;;  (let* ((root (locate-dominating-file
;;                (or (buffer-file-name) default-directory)
;;                "node_modules"))
;;         (global-flow (executable-find "flow"))
;;         (local-flow (expand-file-name "node_modules/.bin/flow" root))
;;         (flow (if (file-executable-p local-flow)
;;                   local-flow
;;                 global-flow)))
;;    (setq-local flycheck-javascript-flow-executable flow)))
;;
;;(defun use-stylelint-from-node-modules ()
;;  (let* ((root (locate-dominating-file
;;                (or (buffer-file-name) default-directory)
;;                "node_modules"))
;;         (global-stylelint (executable-find "stylelint"))
;;         (local-stylelint (expand-file-name "node_modules/.bin/stylelint"
;;                                            root))
;;         (stylelint (if (file-executable-p local-stylelint)
;;                        local-stylelint
;;                      global-stylelint)))
;;    (setq-local flycheck-css-stylelint-executable stylelint)))
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defun setup-flow-file()
;;  (flycheck-select-checker 'javascript-flow))
;;
;;(defun check-for-buffer-text(buffer-text pattern)
;;  (string-match pattern buffer-text))
;;
;;(defun check-for-buffer-text-with-callback(buffer-text pattern callback)
;;  (if
;;      (check-for-buffer-text pattern buffer-text)
;;      (funcall callback)))
;;
;;(defun setup-flow-files(callback)
;;  (check-for-buffer-text
;;   (buffer-substring-no-properties (point-min) (point-max)) "@flow" callback))
;;
;;(defun javascript-flow-predicate()
;;  (and
;;   buffer-file-name
;;   (file-exists-p buffer-file-name)
;;   (check-for-buffer-text
;;    (buffer-substring-no-properties (point-min) (point-max))
;;    "@flow")
;;   (locate-dominating-file buffer-file-name ".flowconfig")))
;;
;;
;;
;;                                        ; Use eslint, stylelint, and flow from node_modules if it exists
;;(add-hook 'js2-mode-hook #'spacemacs//react-use-eslint-from-node-modules)
;;(add-hook 'js2-mode-hook #'use-flow-from-node-modules)
;;(add-hook 'react-mode-hook #'use-flow-from-node-modules)
;;
;;                                        ; Make flow work properly
;;(flycheck-def-args-var flycheck-javascript-flow-args javascript-flow)
;;(customize-set-variable 'flycheck-javascript-flow-args '())
;;(flycheck-define-checker javascript-flow
;;  "A JavaScript syntax and style checker using Flow. See URL `http://flowtype.org/'."
;;  :command (
;;            "flow"
;;            "check-contents"
;;            (eval flycheck-javascript-flow-args)
;;            "--from" "emacs"
;;            "--color=never"
;;            source-original)
;;  :standard-input t
;;  :predicate javascript-flow-predicate
;;  :next-checkers (javascript-eslint)
;;  :error-patterns
;;  ((error line-start
;;          (file-name)
;;          ":"
;;          line
;;          "\n"
;;          (message (minimal-match (and (one-or-more anything) "\n")))
;;          line-end))
;;  :modes (js-mode js2-mode js3-mode react-mode))
;;(add-to-list 'flycheck-checkers 'javascript-flow)

(use-package markdown-mode
  :init (setq markdown-hide-markup t
              markdown-asymmetric-header t
              markdown-fontify-code-blocks-natively t)
  :general
  ('(normal motion emacs) markdown-mode-map
   "{" #'markdown-backward-block
   "}" #'markdown-forward-block
   "<<" #'markdown-promote
   ">>" #'markdown-demote
   "C-k" #'markdown-move-up
   "C-j" #'markdown-move-down)
  ('(visual) markdown-mode-map
   ">" #'markdown-indent-region
   "<" #'markdown-outdent-region)
  ('(normal motion emacs) markdown-mode-map
   :prefix "SPC m"
   "" '(:ignore t :which-key "Major Mode (Markdown)")
   "'" '(markdown-edit-code-block :which-key "edit code")
   "]" '(markdown-complete :which-key "complete")
   "C-]" '(markdown-complete-buffer :which-key "complete buffer")

   "i" '(:ignore t :which-key "Insert")
   "i`" '(markdown-electric-bacquote :which-key "backquote")
   "i-" '(markdown-insert-hr :which-key "hr")
   "i[" '(markdown-insert-gfm-checkbox :which-key "checkbox")
   "ib" '(markdown-insert-bold :which-key "bold")
   "iC" '(markdown-insert-gfm-code-block
          :which-key "code block")
   "ic" '(markdown-insert-code :which-key "code")
   "if" '(markdown-insert-footnote :which-key "footnote")
   "ih" '(:ignore t :which-key "Header")
   "ih1" '(markdown-insert-header-atx-1 :which-key "1")
   "ih2" '(markdown-insert-header-atx-2 :which-key "2")
   "ih3" '(markdown-insert-header-atx-3 :which-key "3")
   "ih4" '(markdown-insert-header-atx-4 :which-key "4")
   "ih5" '(markdown-insert-header-atx-5 :which-key "5")
   "ih6" '(markdown-insert-header-atx-6 :which-key "6")
   "ihU" '(markdown-insert-header-setext-1
           :which-key "underlined (L1)")
   "ihu" '(markdown-insert-header-setext-2
           :which-key "underlined (L2)")
   "iI" '(markdown-insert-image :which-key "image")
   "ii" '(markdown-insert-italic :which-key "italic")
   "ik" '(markdown-insert-kbd :which-key "kbd")
   "iL" '(markdown-insert-list-item :which-key "list item")
   "il" '(markdown-insert-link :which-key "link")
   "iP" '(markdown-pre-region :which-key "pre region")
   "ip" '(markdown-insert-pre :which-key "pre")
   "iQ" '(markdown-blockquote-region
          :which-key "blockquote region")
   "iq" '(markdown-insert-blockquote :which-key "blockquote")
   "is" '(markdown-insert-strike-through
          :which-key "strikethrough")
   "iw" '(markdown-insert-wiki-link :which-key "wiki link")

   "l" '(:ignore t :which-key "Link")
   "ln" '(markdown-next-link :which-key "next")
   "lp" '(markdown-previous-link :which-key "previous")

   "m" '(:ignore t :which-key "Mark")
   "mb" '(markdown-mark-block :which-key "block")
   "mt" '(markdown-mark-subtree :which-key "subtree")

   "n" '(markdown-cleanup-list-numbers
         :which-key "cleanup list numbers")

   "r" '(:ignore t :which-key "Refs")
   "rc" '(markdown-check-refs :which-key "check")
   "ru" '(markdown-unused-refs :which-key "unused")

   "s" '(:ignore t :which-key "Switches")
   "s RET" '(markdown-toggle-markup-hiding
             :which-key "markup hiding")
   "sm" '(markdown-toggle-math :which-key "math")
   "sc" '(markdown-toggle-fontify-code-blocks-natively
          :which-key "code highlighting")
   "si" '(markdown-toggle-inline-images :which-key "images")
   "su" '(markdown-toggle-url-hiding :which-key "urls")

   "t" '(:ignore t :which-key "Tables")
   "tc" '(:ignore t :which-key "Column")
   "tcd" '(markdown-table-delete-column :which-key "delete")
   "tci" '(markdown-table-insert-column :which-key "insert")
   "tR" '(markdown-table-convert-region :which-key "convert region")
   "tr" '(:ignore t :which-key "Row")
   "trd" '(markdown-table-delete-row :which-key "delete")
   "tri" '(markdown-table-insert-row :which-key "insert")
   "ts" '(markdown-table-sort-lines :which-key "sort")
   "tt" '(markdown-table-transpose :which-key "transpose")))

;; Enable mmm for files that have embedded code in other modes
(use-package mmm-mode
  :init (setq mmm-parse-when-idle t
              mmm-global-mode 'buffers-with-submode-classes)
  :config (require 'mmm-auto))

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

;; Set up org-mode
(use-package pdf-tools
  :demand
  :init (setq-default pdf-view-display-size 'fit-page)
  :config (pdf-tools-install t)
  :general
  (pdf-view-mode-map
   "SPC" nil)
  (pdf-view-mode-map
   :prefix "SPC m"
   "" '(:ignore t :which-key "Major Mode (PDF)")
   "f" '(:ignore t :which-key "Fit")
   "fh" '(pdf-view-fit-height-to-window :which-key "height")
   "fp" '(pdf-view-fit-page-to-window :which-key "page")
   "fw" '(pdf-view-fit-width-to-window :which-key "width")
   "l" '(pdf-view-goto-label :which-key "goto label")
   "r" '(:ignore t :which-key "Rendering")
   "rd" '(pdf-view-dark-minor-mode :which-key "dark")
   "rm" '(pdf-view-midnight-minor-mode :which-key "midnight")
   "rp" '(pdf-view-printer-minor-mode :which-key "printer"))
  (pdf-annot-minor-mode-map
   :prefix "SPC ma"
   "" '(:ignore t :which-key "Annotate")
   "a" '(pdf-annot-attachment-dired :which-key "attach")
   "d" '(pdf-annot-delete :which-key "delete")
   "l" '(pdf-annot-list-annotations :which-key "list")
   "m" '(:ignore t :which-key "Markup")
   "mh" '(pdf-annot-add-highlight-markup-annotation
          :which-key "add highlight markup")
   "mm" '(pdf-annot-add-markup-annotation :which-key "add markup")
   "ms" '(pdf-annot-add-squiggly-markup-annotation
          :which-key "add squiggly markup")
   "mt" '(pdf-annot-add-strikeout-markup-annotation
          :which-key "add strikeout markup")
   "mu" '(pdf-annot-add-underline-markup-annotation
          :which-key "add underline markup")
   "t" '(pdf-annot-add-text-annotation :which-key "add text"))
  (pdf-misc-minor-mode-map
   "SPC mm" '(pdf-misc-display-metadata :which-key "show metadata")))

(use-package purescript-mode
  :hook (purescript-mode . turn-on-purescript-indentation)
  :general ('(normal motion emacs) purescript-mode-map
            :prefix "SPC m"
            "" '(:ignore t :which-key "Major Mode (Purescript)")
            "." '(purescript-mode-format-imports
                  :which-key "format imports")))

(use-package psc-ide
  :hook (purescript-mode . psc-ide-mode)
  :general
  ('(normal motion emacs) psc-ide-mode-map
   :jump t
   "C-]" #'psc-ide-goto-definition)
  ('(normal motion emacs) psc-ide-mode-map
   :prefix "SPC m"
   "a" '(psc-ide-add-clause :which-key "add clause")
   "b" '(psc-ide-rebuild :which-key "rebuild")
   "c" '(psc-ide-case-split :which-key "split cases")
   "i" '(psc-ide-add-import :which-key "add import")
   "l" '(psc-ide-load-all :which-key "load modules")
   "q" '(psc-ide-server-quit :which-key "quit server")
   "s" '(psc-ide-server-start :which-key "start server")
   "t" '(psc-ide-show-type :which-key "show type")))

;; Enable colors!
(use-package shell
  :hook (shell-mode . ansi-color-for-comint-mode-on)
  :general
  ('(normal motion emacs)
   "SPC as" '(shell :which-key "shell"))
  (shell-mode-map
   "C-c" #'comint-interrupt-subjob
   "C-n" #'comint-next-input
   "C-p" #'comint-previous-input)
  ('(normal motion emacs) shell-mode-map
   :prefix "SPC m"
   "" '(:ignore t :which-key "Major Mode (Shell)")
   "q" '(comint-kill-subjob :which-key "quit")
   "w" '(comint-write-output :which-key "save")))

(setq-default

 ;; Stop mixing tabs and spaces
 indent-tabs-mode nil

 ;; Fill to 80 characters instead of the default 70
 fill-column 80)

;; Turn on spell checking
(use-package flyspell
  :init (setq flyspell-issue-message-flag nil
              ispell-program-name ispell-path)
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))

;; Browse with my browse script
(use-package browse-url
  :init (setq browse-url-generic-program browse-path
              browse-url-browser-function 'browse-url-generic))

;; Turn on URL discovery
(use-package goto-address
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)))

;; Intelligently clean up whitespace
(use-package ws-butler
  :delight
  :config (ws-butler-global-mode))

;; Display a tilde after buffer end like in vim
(define-fringe-bitmap 'tilde [#b00000000
                              #b00000000
                              #b00000000
                              #b01110001
                              #b11011011
                              #b10001110
                              #b00000000
                              #b00000000])
(setq-default indicate-empty-lines t
              fringe-indicator-alist '((empty-line . tilde)))

;; Scroll one line at a time
(setq scroll-step 1)

;; Turn on line numbers
(use-package display-line-numbers
  :config (global-display-line-numbers-mode))

;; Highlight current line
(use-package hl-line
  :config (global-hl-line-mode))

;; Show git status in the gutter
(use-package git-gutter
  :delight
  :config (global-git-gutter-mode))

;; Show emojis!
(use-package emojify
  :init (setq emojify-emojis-dir emoji-sets-path
              emojify-emoji-set "emojione")
  :config
  (global-emojify-mode)
  (global-emojify-mode-line-mode))

(use-package company
  :demand
  :delight
  :general ("C-SPC" #'company-complete)
  :hook ((company-completion-started . set-company-maps)
         (company-completion-finished . unset-company-maps)
         (company-completion-cancelled . unset-company-maps))
  :config
  (global-company-mode)

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
      "RET" #'company-complete-selection)))

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

(use-package unicode-fonts
  :config (unicode-fonts-setup))

(add-to-list 'default-frame-alist
             `(font . ,(concat font-face "-" (number-to-string font-size))))

;; Turn off GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(setq x-gtk-use-system-tooltips nil)

(use-package image
  :config (imagemagick-register-types))

(use-package imenu-list
  :init (setq imenu-list-focus-after-activation t
              imenu-list-auto-resize t)
  :general ('(normal motion emacs)
            "SPC ei" '(imenu-list-smart-toggle :which-key "Imenu")))

(use-package link-hint
  :ensure t
  :general ('(normal motion emacs)
            :prefix "SPC el"
            "" '(:ignore t :which-key "Links")
            "o" '(link-hint-open-link :which-key "open")
            "y" '(link-hint-copy-link :which-key "copy")))

;; Turn on powerline
(use-package powerline
  :init (setq powerline-height 25))

;; Turn on powerline-evil-center-color theme
(use-package powerline-evil
  :after powerline
  :init (setq powerline-evil-tag-style 'verbose)
  :config (powerline-evil-center-color-theme))

;; Use icons for most modes
(use-package mode-icons
  :after undo-tree
  :config (mode-icons-mode))

;; Use solarized
(use-package solarized-theme
  :init (setq x-underline-at-descent-line t)
  :config
  (deftheme solarized-dark-with-fixes)
  (eval-when-compile (require 'solarized-palettes))
  (solarized-with-color-variables
    'dark
    'solarized-dark-with-fixes
    solarized-dark-color-palette-alist
    '(

      (custom-theme-set-faces
       theme-name

       ;; Make popups more visible
       `(popup-tip-face ((t (:background ,base3 :foreground ,base01))))
       `(tooltip ((t (:background ,base3 :foreground ,base01))))
       `(company-tooltip ((t (:background ,base3 :foreground ,base01))))
       `(company-tooltip-selection ((t (:background ,base01 :foreground ,base3))))
       `(company-scrollbar-bg ((,class (:background ,base2))))
       `(company-scrollbar-fg ((,class (:foreground ,base3 :background ,base00))))
       `(company-tooltip-common ((,class (:foreground ,magenta))))

       ;; Make org-mode tags look like tags
       `(org-tag ((t (:foreground ,violet :box t :height 0.8))))

       ;; Fix colors on modeline
       `(powerline-evil-normal-face ((t (:weight bold :inherit 'menu))))
       `(powerline-evil-insert-face ((t (:weight bold :inherit 'region))))
       `(powerline-evil-visual-face ((t (:weight bold :inherit 'lazy-highlight))))
       `(powerline-evil-operator-face ((t (:weight bold :inherit 'menu))))
       `(powerline-evil-replace-face ((t (:weight bold :inherit 'isearch))))
       `(powerline-evil-motion-face ((t (:weight bold :inherit 'menu))))
       `(powerline-evil-emacs-face ((t (:weight bold :inherit 'menu))))

       ;; Don't override shell colors
       `(comint-highlight-prompt (())))

      (custom-theme-set-variables
       theme-name
       ;; Fix pos-tip popup colors
       `(pos-tip-foreground-color ,base01)
       `(pos-tip-background-color ,base3))))

  (enable-theme 'solarized-dark-with-fixes))

(use-package zoom-frm
  :general ("C-+" #'zoom-frm-in
            "C--" #'zoom-frm-out
            "C-*" #'zoom-frm-unzoom))

(provide 'emacs-rc)
;;; emacs-rc.el ends here
