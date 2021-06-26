;; All packages related to narrowing and completion

;;; Narrowing Completion

;;;; Vertico
;; ;; Enable vertico
;; (use-package vertico
;;   :straight (:host github :repo "minad/vertico")
;;   :general
;;   (:keymaps 'vertico-map
;;    "C-j"    'vertico-next
;;    "C-k"    'vertico-previous)
;;   :hook (after-init . vertico-mode)
;;   :config
;;   (setq vertico-cycle t))

;; ;; Use the `orderless' completion style.
;; ;; Enable `partial-completion' for files to allow path expansion.
;; ;; You may prefer to use `initials' instead of `partial-completion'.
;; (use-package orderless
;;   :init
;;   (setq completion-styles '(orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))

;; ;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   (defun crm-indicator (args)
;;     (cons (concat "[CRM] " (car args)) (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Grow and shrink minibuffer
;;   (setq resize-mini-windows nil)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t))

;;;; Selectrum
;; Good completion package -- much more sane and organized than ivy. Still more than I really need but works well
(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :hook (after-init . selectrum-mode)
  :general
  (:keymaps 'selectrum-minibuffer-map
   ;; "RET"    'icomplete-force-complete-and-exit
   "C-M-i"  'minibuffer-complete
   "M-RET"  'exit-minibuffer
   "<down>" 'selectrum-next-candidate
   "C-j"    'selectrum-next-candidate
   "<up>"   'selectrum-previous-candidate
   "C-k"    'selectrum-previous-candidate)
  :config
  (setq selectrum-num-candidates-displayed 10)
  (setq selectrum-fix-vertical-window-height t)
  (setq selectrum-extend-current-candidate-highlight t)
  (setq selectrum-count-style 'current/matches)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (setq selectrum-refine-candidates-function #'orderless-filter))

;; history
(use-package selectrum-prescient
  :straight t
  :after selectrum
  :config
  (setq selectrum-prescient-enable-filtering nil)
  (setq prescient-save-file (concat cpm-cache-dir "prescient-save.el"))
  (prescient-persist-mode)
  (selectrum-prescient-mode +1))

;; Selectrum info search commands
;; https://github.com/raxod502/selectrum/wiki/Useful-Commands#info
(defvar Info-directory-list)
(defvar Info-additional-directory-list)
(defvar Info-default-directory-list)
(declare-function info-initialize "info")
(declare-function cl-mapcar "cl-lib")

(defvar selectrum-info-history nil
  "Completion history for `selectrum-info' and derived commands.")

(defun selectrum--info-section-candidates (top-node)
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

(defun selectrum--info-top-dir-menu-items ()
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
(defun selectrum-info (&optional top-node)
  "Use `completing-read' to jump to an Info topic.

Select from the available Info top-level nodes, then one of the sub-nodes.
If TOP-NODE is provided, then just select from its sub-nodes."
  (interactive)
  (unless top-node
    (setq top-node
          (let* ((items (selectrum--info-top-dir-menu-items))
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
      (let* ((section-candidates-alist (selectrum--info-section-candidates top-node))
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
                                       t nil 'selectrum-info-history)))
        (info (concat
               top-node
               (cdr (assoc section section-candidates-alist)))))
    (info top-node)))

;;;###autoload
(defun selectrum-info-elisp-manual ()
  "Like ‘selectrum-info’, but choose nodes from the Elisp reference manual. "
  (interactive)
  (selectrum-info "(elisp)"))

;;;###autoload
(defun selectrum-info-emacs-manual ()
  "Like ‘selectrum-info’, but directly choose nodes from the Emacs manual."
  (interactive)
  (selectrum-info "(emacs)"))

;;;###autoload
(defun selectrum-info-org-manual ()
  "Like ‘selectrum-info’, but directly choose nodes from the Org manual."
  (interactive)
  (selectrum-info "(org)"))

;; Bind keys for selectrum-info
(general-define-key "C-h i" #'selectrum-info)

;;;; Ordering
(use-package orderless
  :straight t
  :after selectrum
  :config
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil)
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)))

;;;; Affe (Fuzzy Search)
(use-package affe
  :straight (affe :type git :host github :repo "minad/affe" )
  :after orderless
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches)
  (setq affe-grep-command "rg -L --null --color=never --max-columns=1000 --no-heading --line-number -v ^$ .")
  ;; Manual preview key for `affe-grep'
  (setf (alist-get #'affe-grep consult-config) `(:preview-key ,(kbd "M-."))))

;;;; Embark
;; Actions on narrowed candidates
(use-package embark
  :straight (embark :type git :host github :repo "oantolin/embark")
  ;; :after (icomplete-vertical)
  :commands (embark-act embark-keymap-help)
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
  (setq embark-prompter 'embark-completing-read-prompter))


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
  :general
  (:keymaps 'minibuffer-local-map
   "C-M-a"  'marginalia-cycle)
  ;; When using the Embark package, you can bind `marginalia-cycle' as an Embark action
  (:keymaps 'embark-general-map
   "A"  'marginalia-cycle)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
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
  :straight (consult :type git :host github :repo "minad/consult")
  :commands (consult-buffer consult-find consult-apropos consult-yank-pop)
  :init
  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)
  :config
  ;; disable preview for certain commands
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))
  (consult-customize consult-theme :preview-key '(:debounce 0.5 any))
  ;; Make consult locate work with macos
  (setq consult-locate-command "mdfind -name ARG OPTS")
  ;; Optionally configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-async-min-input 0))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab
                                :repo "OlMon/consult-projectile" :branch "master")
  :commands (consult-projectile))

;; integrate consult and persp-mode
(defun cpm/persp-consult-buffer ()
  (interactive)
  (with-persp-buffer-list () (consult-buffer)))

;; Configure initial narrowing per command
(defvar consult-initial-narrow-config
  '((consult-projectile . ?p)))

;; Add initial narrowing hook
(defun consult-initial-narrow ()
  (when-let (key (alist-get this-command consult-initial-narrow-config))
    (setq unread-command-events (append unread-command-events (list key 32)))))
;; (add-hook 'minibuffer-setup-hook #'consult-initial-narrow)

;; Search at point with consult
(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(use-package consult-flycheck
  :straight (:host github :repo "minad/consult-flycheck")
  :after flycheck
  :commands (consult-flycheck consult-flymake))

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
   "C-/"   #'company-search-candidates
   "C-M-/" #'company-filter-candidates
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
  (add-to-list 'company-backends 'company-keywords)
  (add-to-list 'company-backends 'company-files))

(use-package company-bibtex
  :after company
  :demand t
  :general
  (:states 'insert
   "<C-tab>" #'company-bibtex)
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography "~/Dropbox/Work/bibfile.bib")
  (setq company-bibtex-org-citation-regex "-?cite:"))

;;;; Company Prescient

(use-package company-prescient
  :after company
  :demand t
  :config
  (company-prescient-mode t))




;;;; Company Org Block
;; Org block completion
;; https://github.com/xenodium/company-org-block
(use-package company-org-block
  :after org
  :straight (:host github :repo "xenodium/company-org-block")
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :config
  (require 'org-element)
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))



;;;; Yasnippet
;; the official snippet collection https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets  :after yasnippet :demand t)
(use-package yasnippet
  ;; :hook (after-init . yas-global-mode)
  :defer 2
  :config
  ;; snippet directory
  (setq-default yas-snippet-dirs '("~/.emacs.d/.local/snippets/cpm-snippets"
                                   yasnippet-snippets-dir))
  ;; see https://emacs.stackexchange.com/a/30150/11934
  (defun cpm/yas-org-mode-hook ()
    (setq-local yas-buffer-local-condition
                '(not (org-in-src-block-p t))))
  (add-hook 'org-mode-hook #'cpm/yas-org-mode-hook)
  (yas-global-mode)
  ;; Adding yasnippet support to company
  (with-eval-after-load 'company-mode
    (add-to-list 'company-backends '(company-yasnippet)))
  (yas-reload-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Icons
(use-package icon-affixation
  :straight (:host github :repo "iyefrat/icon-affixation")
  :hook (after-init . icon-affixation-mode))

;;; End Completion
(provide 'setup-completion)
