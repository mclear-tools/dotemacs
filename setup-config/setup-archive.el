;;;; Icomplete
(use-package icomplete-vertical
  :disabled
  :straight (:type built-in)
  :hook (after-init . icomplete-vertical-mode)
  :general
  (:keymaps 'icomplete-minibuffer-map
   "C-v"    'icomplete-vertical-toggle
   "RET"    'icomplete-force-complete-and-exit
   "TAB"    'icomplete-force-complete-and-exit
   "C-M-i"  'minibuffer-complete
   "M-RET"  'exit-minibuffer
   "<down>" 'icomplete-forward-completions
   "C-j"    'icomplete-forward-completions
   "<up>"   'icomplete-backward-completions
   "C-k"    'icomplete-backward-completions)
  :custom
  (icomplete-show-matches-on-no-input t)
  (icomplete-hide-common-prefix nil)
  (icomplete-compute-delay 0.0)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (icomplete-scroll t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode))

;;;; Vertico
;; (use-package vertico
;;   :general
;;   (:keymaps 'vertico-map
;;    "C-j"    'vertico-next
;;    "C-k"    'vertico-previous)
;;   :init
;;   (vertico-mode)
;;   :config
;;   ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
;;   (setq vertico-cycle t))

;; ;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   (defun crm-indicator (args)
;;     (cons (concat "[CRM] " (car args)) (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Grow and shrink minibuffer
;;   ;;(setq resize-mini-windows t)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t))

;; ;; Persist history over Emacs restarts using savehist (see setup-settings). Vertico sorts by history position.

;; (use-package orderless
;;   :init
;;   (setq completion-styles '(orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))

;; (selectrum-mode -1)

;;;; Sticky Buffer
;; Stick/Lock buffer to window, courtesy of ShingoFukuyama.
;; https://gist.github.com/ShingoFukuyama/8797743

;; (defvar sticky-buffer-previous-header-line-format)
;; (define-minor-mode sticky-buffer-mode
;;   "Make the current window always display this buffer."
;;   nil " sticky" nil
;;   (if sticky-buffer-mode
;;       (progn
;;         (set (make-local-variable 'sticky-buffer-previous-header-line-format)
;;              header-line-format)
;;         (set-window-dedicated-p (selected-window) sticky-buffer-mode))
;;     (set-window-dedicated-p (selected-window) sticky-buffer-mode)
;;     (setq header-line-format sticky-buffer-previous-header-line-format)))

;;;; Dired Plus
(use-package dired+
  :disabled t
  :after dired
  :hook ((dired-mode . diredp--set-up-font-locking))
  ;;(dired-mode . dired-omit-mode))
  :init
  (setq font-lock-maximum-decoration t)
  (setq diredp-omit-files-regexp "\\.?#\\|^\\.$\\|^\\.\\.")
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-toggle-find-file-reuse-dir 1))
;; :custom-face
;; ;; TODO: change colors to work with bespoke theme
;; (diredp-compressed-file-name ((t (:foreground "#00629D"))))
;; (diredp-compressed-file-suffix ((t (:foreground "#839496"))))
;; (diredp-date-time ((t (:foreground "#9EA0E5"))))
;; (diredp-deletion ((t (:background "Red" :foreground "Yellow"))))
;; (diredp-dir-heading ((t (:background "#69B7F0" :foreground "#002b36"))))
;; (diredp-dir-name ((t (:foreground "#69B7F0"))))
;; (diredp-dir-priv ((t (:foreground "#268bd2"))))
;; (diredp-exec-priv ((t (:foreground "#990A1b"))))
;; (diredp-file-name ((t (:foreground "#2aa198"))))
;; (diredp-file-suffix ((t (:foreground "#839496"))))
;; (diredp-flag-mark-line ((t (:foreground "#dc322f"))))
;; (diredp-no-priv ((t (:foreground "#b58900"))))
;; (diredp-number ((t (:foreground "#DEB542"))))
;; (diredp-rare-priv ((t (:background "#cb4b16" :foreground "#B4C342"))))
;; (diredp-read-priv ((t (:foreground "#F2804F"))))
;; (diredp-tagged-autofile-name ((t (:foreground "#328C04113"))))
;; (diredp-write-priv ((t (:foreground "#8b2C02")))))

;;; Helm Bibtex
(use-package helm-bibtex
  :disabled
  :straight t
  :commands helm-bibtex
  :after helm
  :config
  ;; Set insert citekey with markdown citekeys for org-mode
  (setq bibtex-completion-format-citation-functions
        '((org-mode    . bibtex-completion-format-citation-pandoc-citeproc)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))
  (setq bibtex-completion-display-formats
        '((t . "${author:16} ${title:36} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))
  ;; Set default action for ivy-bibtex to edit notes file
  (setq helm-bibtex-default-action 'helm-bibtex-edit-notes)
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  (setq bibtex-completion-notes-template-one-file "* ${author} (${date}): ${title} \n :PROPERTIES:\n :INTERLEAVE_PDF: ${file}\n :Custom_ID: ${=key=}\n :END:\n [[pdfview:${file}][file link]]")
  (setq bibtex-completion-notes-template-multiple-files "#+TITLE: ${author-or-editor} (${year}): ${title}\n#+ROAM_KEY: cite:${=key=}\n#+SETUPFILE: ./hugo_setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n#+BEGIN_SRC bibtex\n (insert (org-ref-get-bibtex-entry \"${=key=}\"))\n#+END_SRC")
  (setq bibtex-completion-bibliography "~/Dropbox/Work/bibfile.bib"
        bibtex-completion-library-path "~/Dropbox/Work/be-library/"
        bibtex-completion-pdf-field nil
        bibtex-completion-notes-path "~/Dropbox/Work/projects/notebook/content-org"
        bibtex-completion-notes-extension ".org"
        helm-bibtex-full-frame nil))
