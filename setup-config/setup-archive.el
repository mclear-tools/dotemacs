;;; Old Notes Search
(defvar cpm-notes-dir "~/Dropbox/Work/projects/notebook/content-org/")
(defun cpm/search-all-notes ()
  (interactive)
  (cd cpm-notes-dir)
  (call-interactively #'deadgrep))

;;; Company Org Roam
(use-package company-org-roam
  :disabled t
  :after (org company)
  :demand t
  :config
  (push 'company-org-roam company-backends))


;;; Deft
(use-package deft
  :commands (deft deft-open-file-other-window cpm/notebook deft-new-file-named)
  :general
  (:keymaps 'deft-mode-map :states '(normal motion)
   "o" 'cpm/deft-open
   "p" 'cpm/deft-open-preview
   "q" 'kill-this-buffer)
  (:keymaps 'deft-mode-map :states '(insert)
   "C-j" 'evil-next-line
   "C-k" 'evil-previous-line
   "C-o" 'cpm/deft-open
   "C-p" 'cpm/deft-open-preview)
  :config
  ;; https://github.com/jrblevin/deft/issues/100
  (defun deft-parse-summary (contents title)
    "Parse the file CONTENTS, given the TITLE, and extract a summary.
The summary is a string extracted from the contents following the
title."
    (let* ((summary (let ((case-fold-search nil))
                      (replace-regexp-in-string deft-strip-summary-regexp " " contents)))
           (summary-processed (deft-chomp
                                (if (and title
                                         (not deft-use-filename-as-title)
                                         (string-match (regexp-quote
                                                        (if deft-org-mode-title-prefix
                                                            (concat "^#+TITLE: " title)
                                                          title))
                                                       summary))
                                    (substring summary (match-end 0) nil)
                                  summary))))
      (substring summary-processed 0 (min 512 (string-width summary-processed)))))

  (with-eval-after-load 'evil
    (add-to-list 'evil-insert-state-modes 'deft-mode))
  ;; basic settings for use with zettel
  (setq deft-directory (concat (getenv "HOME") "/Dropbox/work/projects/notebook/content-org")
        deft-recursive t
        deft-use-filename-as-title t
        deft-separator " "
        deft-extensions '("org" "txt" "md")
        deft-default-extension "org")
  ;; file renaming rules
  (setq deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))

  (setq deft-strip-summary-regexp "\\`\\(.+\n\\)")


  ;; (setq deft-strip-summary-regexp
  ;;       (concat "\\("
  ;;               "[\n\t]" ;; blank
  ;;               "\\|^#\\+[a-zA-Z_]+:.*$" ;;org-mode metadata
  ;;               ":PROPERTIES:\n\\(.+\n\\)+:END:\n" ;; strip org property drawer
  ;;               ;;yaml metadata
  ;;               "\\|^\\-\\{3\\}$"
  ;;               "\\|^[a-zA-Z_]+:.*$"
  ;;               "\\|@[a-zA-Z_].*$"
  ;;               ;; line beginning with markdown links
  ;;               "\\|^\\[.*$"
  ;;               "\\|^# .*$" ;; md titles
  ;;               "\\)"))

  ;;function to run deft in specified directory
  (defun any-deft (dir)
    "Run deft in directory DIR"
    (setq deft-directory dir)
    (switch-to-buffer "*Deft*")
    (kill-this-buffer)
    (deft))
  (defun cpm/notebook ()
    "Goto main notes with deft"
    (interactive)
    (any-deft "~/Dropbox/Work/projects/notebook/content-org")
    (kill-this-buffer)
    (any-deft "~/Dropbox/Work/projects/notebook/content-org"))
  (defun cpm/deft-open ()
    (interactive)
    (deft-open-file-other-window t))
  (defun cpm/deft-open-preview ()
    (interactive)
    (deft-open-file-other-window)))

;;; Selectrum
;; Good completion package -- much more sane and organized than ivy. I'd prefer to use vertico but that doesn't work well with mini-frame-mode
(use-package selectrum
  :disabled
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
  :disabled
  :straight t
  :after selectrum
  :config
  (setq selectrum-prescient-enable-filtering nil)
  (setq prescient-save-file (concat cpm-cache-dir "prescient-save.el"))
  (prescient-persist-mode)
  (selectrum-prescient-mode +1))

;; (use-package orderless
;;   :straight t
;;   :after selectrum
;;   :config
;;   (setq completion-styles '(orderless))
;;   (setq completion-category-defaults nil)
;;   (setq orderless-skip-highlighting (lambda () selectrum-is-active)))


;;; Miniframe
;; Provides a great ui for completion, similar to posframe
(use-package mini-frame
  :disabled
  :straight (:type git :host github :repo "muffinmad/emacs-mini-frame")
  :hook (after-init . mini-frame-mode)
  :commands (mini-frame-mode)
  :custom
  (mini-frame-show-parameters
   `((top    . 0.023)
     (width  . 0.98)
     (left   . 0.5)
     (height . 11)
     (child-frame-border-width . 15)
     (internal-border-width . 0)
     (left-fringe . 20)
     (right-fringe . 20)
     ;; set colors for bespoke theme
     (foreground-color . ,bespoke-strong)
     (background-color . ,bespoke-subtle)
     ))
  ;; (mini-frame-color-shift-step 7)
  (mini-frame-advice-functions '(read-from-minibuffer
                                 read-string
                                 completing-read))
  (mini-frame-resize nil)
  :config
  (setq mini-frame-ignore-commands
        '("edebug-eval-expression" debugger-eval-expression "vertico" vertico-mode))
  (setq mini-frame-resize 'not-set))


;;; Icomplete
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

;;; Sticky Buffer
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

;;; Dired Plus
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
