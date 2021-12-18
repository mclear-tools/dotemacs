;; All packages related to narrowing and completion

;;; Narrowing Completion

;;;; Vertico
;; Enable vertico for vertical completion
;; This and selectrum are great packages, but vertico is preferable if I can get feature parity with what I was using in selectrum
(use-package vertico
  :straight (:host github :repo "minad/vertico"
             :includes (vertico-repeat vertico-directory vertico-buffer)
             :files (:defaults "extensions/vertico-directory.el" "extensions/vertico-buffer.el" "extensions/vertico-repeat.el"))
  :general
  (:keymaps 'vertico-map
   "<escape>" #'minibuffer-keyboard-quit
   "C-n"      #'vertico-next-group
   "C-p"      #'vertico-previous-group
   "C-j"      #'vertico-next
   "C-k"      #'vertico-previous
   "M-RET"    #'vertico-exit)
  :hook (after-init . vertico-mode)
  :config
  ;; Cycle through candidates
  (setq vertico-cycle t)

  ;; Don't resize buffer
  (setq vertico-resize nil)

  ;; try the `completion-category-sort-function' first
  (advice-add #'vertico--sort-function :before-until #'completion-category-sort-function)

  (defun completion-category-sort-function ()
    (alist-get (vertico--metadata-get 'category)
               completion-category-sort-function-overrides))

  (defvar completion-category-sort-function-overrides
    '((file . directories-before-files))
    "Completion category-specific sorting function overrides.")

  (defun directories-before-files (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-history-length-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files))))

;;;; Vertico Packages
;; Use vertico in buffer
(use-package vertico-buffer
  :after vertico
  :config
  (vertico-buffer-mode 1))

;; No headerline in vertico-buffer
(with-eval-after-load 'el-patch
(el-patch-feature vertico-buffer)
(with-eval-after-load 'vertico-buffer
  (el-patch-defun vertico-buffer--setup()
    "Setup minibuffer overlay, which pushes the minibuffer content down."
    (add-hook 'window-selection-change-functions 'vertico-buffer--select nil 'local)
    (add-hook 'minibuffer-exit-hook 'vertico-buffer--destroy nil 'local)
    (setq-local cursor-type '(bar . 0))
    (setq vertico-buffer--overlay (make-overlay (point-max) (point-max) nil t t))
    (overlay-put vertico-buffer--overlay 'window (selected-window))
    (overlay-put vertico-buffer--overlay 'priority 1000)
    (overlay-put vertico-buffer--overlay 'before-string "\n\n")
    (setq vertico-buffer--buffer (get-buffer-create
                                  (if (= 1 (recursion-depth))
                                      " *Vertico*"
                                    (format " *Vertico-%s*" (1- (recursion-depth))))))
    (with-current-buffer vertico-buffer--buffer
      (add-hook 'window-selection-change-functions 'vertico-buffer--select nil 'local)
      (setq-local display-line-numbers nil
                  truncate-lines t
                  show-trailing-whitespace nil
                  buffer-read-only t
                  (el-patch-add
                    header-line-format nil)
                  cursor-in-non-selected-windows 'box)))))

;; Vertico repeat last command
(use-package vertico-repeat
  :hook (minibuffer-setup . vertico-repeat-save)
  :commands (vertico-repeat))

;; Configure directory extension
(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; A few more useful configurations...
(use-package emacs
  :straight (:type built-in)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Grow and shrink minibuffer
  (setq resize-mini-windows t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setf enable-recursive-minibuffers t))

;; Persist history over Emacs restarts with savehist mode. Vertico sorts by history position.
;; See setup-settings.el

;;;; Ordering
;; Setup for vertico
;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;;; Embark
;; Actions on narrowed candidates
(use-package embark
  :straight (embark :type git :host github :repo "oantolin/embark")
  :commands (embark-act embark-keymap-help)
  :custom
  ;; Don't display extra embark buffer
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  ;; Use completing-read
  (embark-prompter 'embark-completing-read-prompter)
  :general
  ("C-S-o"   'embark-act
   "C-h B"  'embark-bindings)
  (:keymaps 'minibuffer-local-completion-map
   "C-;" 'embark-act-noexit
   "C-S-o" 'embark-act
   "C-J" 'embark-switch-to-live-occur
   "M-q" 'embark-occur-toggle-view)
  (:keymaps 'completion-list-mode-map
   ";"  'embark-act)
  (:keymaps 'embark-file-map
   "x"  'consult-file-externally)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'embark-allow-edit-commands 'consult-imenu)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Useful Functions
  (define-key embark-file-map (kbd "D") 'cpm/dired-here)
  (defun cpm/dired-here (file)
    "Open dired in this directory"
    (dired (file-name-directory file)))

  (define-key embark-file-map (kbd "g") 'cpm/consult-rg-here)
  (defun cpm/consult-rg-here (file)
    "consult-ripgrep in this directory."
    (let ((default-directory (file-name-directory file)))
      (consult-ripgrep))))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

;;;; Marginalia
;; Enable richer annotations using the Marginalia package
;; Info about candidates pulled from metadata
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  :custom-face
  (marginalia-documentation ((t (:inherit bespoke-faded))))
  :general
  (:keymaps 'minibuffer-local-map
   "C-M-a"  'marginalia-cycle)
  ;; When using the Embark package, you can bind `marginalia-cycle' as an Embark action
  (:keymaps 'embark-general-map
   "A"  'marginalia-cycle)
  :init
  (marginalia-mode)
  ;; ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;;;; Consult
;; Example configuration for Consult
;; Useful functions; a drop-in replacement for ivy/swiper

(use-package consult
  :straight (consult :type git :host github :repo "minad/consult" :includes consult-org)
  :commands (consult-line
             consult-line-multi
             consult-buffer
             consult-find
             consult-apropos
             consult-yank-pop
             consult-goto-line
             consult-outline
             consult-org-agenda
             consult-org-heading)
  :custom-face
  (consult-file ((t (:inherit bespoke-popout))))
  (consult-line-number ((t (:inherit bespoke-faded))))
  :init
  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)
  :config
  ;; Preview is manual and immediate
  ;; https://github.com/minad/consult#live-previews
  (setq consult-preview-key (kbd "C-f"))

  ;; search settings
  (setq consult-grep-args
        "grep --null --line-buffered --color=never --ignore-case\
   --exclude-dir=.git --line-number -I -R -S .")

  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --line-number .")

  ;; Make consult locate work with macos spotlight
  (setq consult-locate-args "mdfind -name")

  (setq consult-async-min-input 0))

;; Use consult-completing-read for enhanced interface.
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

;;;; Consult Search At Point
;; Search at point with consult
(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;;;; Consult Flycheck
(use-package consult-flycheck
  :straight (:host github :repo "minad/consult-flycheck")
  :after flycheck
  :commands (consult-flycheck consult-flymake))

;;;; Consult Dir
;; Consult-dir allows you to easily select and switch between “active” directories.
(use-package consult-dir
  :straight (:host github :repo "karthink/consult-dir")
  :commands (consult-dir
             consult-dir-jump-file)
  :general
  (:keymaps 'vertico-map
   "C-d" #'consult-dir))

;;;; Completing-Read Info
;; Info search commands using completing-read
;; https://github.com/raxod502/selectrum/wiki/Useful-Commands#info
(defvar Info-directory-list)
(defvar Info-additional-directory-list)
(defvar Info-default-directory-list)
(declare-function info-initialize "info")
(declare-function cl-mapcar "cl-lib")

(defvar completing-read-info-history nil
  "Completion history for `completing-read-info' and derived commands.")

(defun completing-read--info-section-candidates (top-node)
  "Return an alist of sections and candidates in the Info buffer TOP-NODE.

Candidates are returned in the order that their links are listed
in the Info buffer, which might be different from how the
sections are actually ordered."
  (let ((sub-topic-format
         ;; Node links look like "* Some Thing:: Description" or
         ;; "* Some Thing: actual link. Description", where descriptions
         ;; are optional and might continue on the next line.
         ;;
         ;; The `info' library states:
         ;; Note that nowadays we expect Info files to be made using makeinfo.
         ;; In particular we make these assumptions:
         ;;  - a menu item MAY contain colons but not colon-space ": "
         ;;  - a menu item ending with ": " (but not ":: ") is an index entry
         ;;  - a node name MAY NOT contain a colon
         ;; This distinction is to support indexing of computer programming
         ;; language terms that may contain ":" but not ": ".
         (rx "* " (group (+? (not ?:))) ":"
             (or ":" (seq " "  (group (+? (not "."))) "."))
             ;; Include the description, if one exists.
             ;; If it doesn't, the line ends immediately.
             (or "\n"
                 (seq (0+ blank)
                      (group (+? anychar))
                      ;; Sometimes a heading follows on the next line,
                      ;; and sometimes there's any empty blank line
                      ;; (such as before a section title).  For now,
                      ;; assume continuation lines use indentation and
                      ;; other lines don't.
                      "\n" (not blank))))))
    (save-match-data
      (save-selected-window
        (with-temp-buffer
          ;; Some nodes created from multiple files, so we need to create a
          ;; buffer to make sure that we see everything.
          (info top-node (current-buffer))
          (goto-char (point-min))
          (let ((candidates-alist))
            (while (re-search-forward sub-topic-format nil t)
              (forward-line 0)         ; Go back to start of line.
              (let* ((node-display-name (match-string 1))
                     (node-actual-name (or (match-string 2) node-display-name)))
                (push (cons (concat node-display-name
                                    (if-let ((node-description (match-string 3)))
                                        (propertize
                                         (thread-last node-description
                                           (replace-regexp-in-string "\n" "")
                                           (replace-regexp-in-string " +" " ")
                                           (concat " - "))
                                         'face 'completions-annotations)))
                            node-actual-name)
                      candidates-alist)))
            (nreverse candidates-alist)))))))

(defun completing-read--info-top-dir-menu-items ()
  (let ((sub-topic-format
         ;; The `info' library states:
         ;; Note that nowadays we expect Info files to be made using makeinfo.
         ;; In particular we make these assumptions:
         ;;  - a menu item MAY contain colons but not colon-space ": "
         ;;  - a menu item ending with ": " (but not ":: ") is an index entry
         ;;  - a node name MAY NOT contain a colon
         ;; This distinction is to support indexing of computer programming
         ;; language terms that may contain ":" but not ": ".
         (rx (seq "* " (group (+? anything))
                  ": "
                  (group "(" (+? anything) ")" (*? (not ".")))
                  "."
                  (zero-or-one (seq (any "\n" " " "\t")
                                    (group (+? anychar))))
                  "\n" (or "\n" "*")))))
    (let ((candidates-alist))
      ;; Go through nodes in Info buffer "(dir)Top".
      (save-match-data
        (save-selected-window
          (with-temp-buffer
            ;; Some nodes created from multiple files, so we need to create a
            ;; buffer to make sure that we see everything.
            (info "(dir)Top" (current-buffer))
            (goto-char (point-min))
            (search-forward "Menu:\n")
            (while (re-search-forward sub-topic-format nil t)
              (forward-line 0)          ; Go back to start of line.
              (let* ((node-display-name (match-string-no-properties 1))
                     (node-actual-name (or (match-string-no-properties 2) node-display-name)))
                (push (cons (concat node-display-name
                                    (if-let ((node-description (match-string-no-properties 3)))
                                        (propertize
                                         (thread-last node-description
                                           (replace-regexp-in-string "\n" "")
                                           (replace-regexp-in-string " +" " ")
                                           (concat " - "))
                                         'face 'completions-annotations)))
                            node-actual-name)
                      candidates-alist))))))
      ;; In case something isn't listed (Emacs might just insert itself?), also
      ;; add in files from the Info directories as nodes themselves.
      (dolist (file (save-match-data
                      (thread-last (append (or Info-directory-list
                                               Info-default-directory-list)
                                           Info-additional-directory-list)
                        (mapcan (lambda (directory)
                                  (when (file-directory-p directory)
                                    (directory-files directory nil "\\.info" t))))
                        (mapcar (lambda (file)
                                  (string-match "\\(.+?\\)\\." file)
                                  (match-string 1 file)))
                        seq-uniq)))
        ;; TODO: Node should actually come from opening the file.
        (let ((node (concat "(" file ")")))
          (unless (rassoc node candidates-alist)
            (push (cons file node) candidates-alist))))
      (nreverse candidates-alist))))

;;;###autoload
(defun completing-read-info (&optional top-node)
  "Use `completing-read' to jump to an Info topic.

Select from the available Info top-level nodes, then one of the sub-nodes.
If TOP-NODE is provided, then just select from its sub-nodes."
  (interactive)
  (unless top-node
    (setq top-node
          (let* ((items (completing-read--info-top-dir-menu-items))
                 (key (completing-read "Info node: "
                                       (lambda (input predicate action)
                                         (if (eq action 'metadata)
                                             `(metadata
                                               ;; (display-sort-function . identity)
                                               (category              . info))
                                           (complete-with-action action
                                                                 items
                                                                 input
                                                                 predicate)))
                                       nil
                                       t)))
            (cdr (assoc key items)))))
  ;; If looking at a base node (e.g., "(emacs)"), then select from list of
  ;; optional sub-nodes.  If looking at a normal node (e.g., "(emacs)Intro"),
  ;; then just go there instead of asking for more sub-nodes.
  (if (string-match-p "(.*?)\\'" top-node)
      (let* ((section-candidates-alist (completing-read--info-section-candidates top-node))
             (section (completing-read "Info section: "
                                       (lambda (input predicate action)
                                         (if (eq action 'metadata)
                                             `(metadata
                                               (display-sort-function . identity)
                                               (category              . info))
                                           (complete-with-action action
                                                                 section-candidates-alist
                                                                 input
                                                                 predicate)))
                                       nil
                                       t nil 'completing-read-info-history)))
        (info (concat
               top-node
               (cdr (assoc section section-candidates-alist)))))
    (info top-node)))

;;;###autoload
(defun completing-read-info-elisp-manual ()
  "Like ‘completing-read-info’, but choose nodes from the Elisp reference manual. "
  (interactive)
  (completing-read-info "(elisp)"))

;;;###autoload
(defun completing-read-info-emacs-manual ()
  "Like ‘completing-read-info’, but directly choose nodes from the Emacs manual."
  (interactive)
  (completing-read-info "(emacs)"))

;;;###autoload
(defun completing-read-info-org-manual ()
  "Like ‘completing-read-info’, but directly choose nodes from the Org manual."
  (interactive)
  (completing-read-info "(org)"))

;; Bind keys for completing-read-info
(general-define-key "C-h i" #'completing-read-info)

;;; In-Buffer Completion
;;;; Company
;; complete anything http://company-mode.github.io
;; completion in region; use child-frame for better visual performance
;; see setup-childframe.el
;; might be better off with cofu https://github.com/minad/corfu
(use-package company
  :hook ((markdown-mode org-mode prog-mode) . global-company-mode)
  :general
  (:keymaps 'company-active-map
   ;; "C-/"   #'company-search-candidates
   ;; "C-M-/" #'company-filter-candidates
   "C-d"   #'company-show-doc-buffer
   "C-j"   #'company-select-next
   "C-k"   #'company-select-previous
   "C-l"   #'company-complete-selection)
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-selection-wrap-around t
        company-search-regexp-function 'company-search-words-regexp
        company-show-numbers t
        company-echo-truncate-lines nil)
  :config
  ;; Default backends
  (setq company-backends '(company-elisp
                           company-keywords
                           company-files
                           company-semantic)))

;;;; Company Prescient

(use-package company-prescient
  :disabled
  :after company
  :demand t
  :config
  (company-prescient-mode t))

;;;; Company Org Block
;; Org block completion
;; https://github.com/xenodium/company-org-block
(use-package company-org-block
  :straight (:host github :repo "xenodium/company-org-block")
  :after org
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :config
  (require 'org-element)
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

;;;; Yasnippet
(use-package yasnippet
  :defer 1
  :config
  ;; NOTE: need to specify dirs; does not look in non-snippet subdirs
  (setq yas-snippet-dirs '("~/.emacs.d/.local/all-snippets/cpm-snippets"
                           "~/.emacs.d/.local/all-snippets/yasnippet-snippets"))
  ;; see https://emacs.stackexchange.com/a/30150/11934
  (defun cpm/yas-org-mode-hook ()
    (setq-local yas-buffer-local-condition
                '(not (org-in-src-block-p t))))
  (add-hook 'org-mode-hook #'cpm/yas-org-mode-hook)
  ;; Adding yasnippet support to company
  (with-eval-after-load 'company-mode
    (add-to-list 'company-backends '(company-yasnippet)))
  (yas-global-mode 1)
  (yas-reload-all))

;; the official snippet collection https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :after (yasnippet)
  :custom
  (yasnippet-snippets-dir (concat cpm-local-dir "all-snippets/yasnippet-snippets")))


;;; Icons
(use-package all-the-icons-completion
  :straight (:host github :repo "iyefrat/all-the-icons-completion")
  :if (display-graphic-p)
  :hook (after-init . all-the-icons-completion-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; End Completion
(provide 'setup-completion)
