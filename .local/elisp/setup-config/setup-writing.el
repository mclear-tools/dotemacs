;; Writing

;;; Spelling
(use-package ispell
  :commands (ispell-word ispell-region ispell-buffer)
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))
  ;; (when (executable-find "hunspell")
  ;;   (setq-default ispell-program-name "hunspell")
  ;;   (setq ispell-extra-args   '("-d en_US"))
  ;;   (setq ispell-really-hunspell t)))

  ;; Save a new word to personal dictionary without asking
  ;; (setq ispell-silently-savep nil)

  ;; (setq-default ispell-program-name "/usr/local/bin/aspell")
  ;; (setq ispell-extra-args
  ;;     (list "--sug-mode=fast" ;; ultra|fast|normal|bad-spellers
  ;;           "--lang=en_US"
  ;;           "--ignore=3")))

(use-package flyspell
 :config
 (setq flyspell-abbrev-p t
       flyspell-use-global-abbrev-table-p t
       flyspell-issue-message-flag nil
       flyspell-issue-welcome-flag nil)
 :hook ((markdown-mode . flyspell-mode)
        (org-mode . flyspell-mode)
        (prog-mode . flyspell-prog-mode)))

(use-package flyspell-correct-ivy
  :general
  (:states '(normal insert emacs) :keymaps 'flyspell-mode-map
   "C-;" 'flyspell-auto-correct-previous-word
   "C-:" 'flyspell-correct-word-generic))

(with-eval-after-load 'hydra
  (defhydra hydra-spelling (:color blue)
  "
  ^
  ^Spelling^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^───────
  _q_ quit            _<_ previous        _c_ correction
  ^^                  _>_ next            _d_ dictionary
  ^^                  _f_ check           _m_ mode
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("<" flyspell-correct-previous :color pink)
  (">" flyspell-correct-next :color pink)
  ("c" ispell)
  ("d" ispell-change-dictionary)
  ("f" flyspell-buffer :color pink)
  ("m" flyspell-mode)))

;;; Abbrev
(use-package abbrev
  :ensure nil
  :defer 2
  :config
  ;; (add-hook 'text-mode-hook #'abbrev-mode)
  (setq abbrev-file-name (concat cpm-local-dir "abbrev/.abbrev_defs")
        save-abbrevs 'nil)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;;; Helm Bibtex
(use-package helm-bibtex
  :commands helm-bibtex
  :config
  ;; Set insert citekey with markdown citekeys for org-mode
  (setq bibtex-completion-format-citation-functions
        '((org-mode    . bibtex-completion-format-citation-pandoc-citeproc)
        (latex-mode    . bibtex-completion-format-citation-cite)
        (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
        (default       . bibtex-completion-format-citation-default)))
  (setq bibtex-completion-display-formats
        '((t . "${author:36} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))
  ;; Set default action for helm-bibtex as inserting citation
  (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
  (helm-add-action-to-source "Insert citation" 'helm-bibtex-insert-citation helm-source-bibtex 0)
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  (setq bibtex-completion-notes-template-one-file "* ${author} (${date}): ${title} \n :PROPERTIES:\n :INTERLEAVE_PDF: ${file}\n :Custom_ID: ${=key=}\n :END:\n [[pdfview:${file}][file link]]")
  (setq bibtex-completion-notes-template-multiple-files
        "---\ntitle: '${author} (${year}): ${title}'\nnocite: |\n   @${=key=}\n---\n\n[PDF Link](${file})\n\n")
  (setq bibtex-completion-bibliography "~/Dropbox/Work/bibfile.bib"
        bibtex-completion-library-path "~/Dropbox/Work/be-library/"
        bibtex-completion-pdf-field nil
        bibtex-completion-notes-path "~/Dropbox/Notes/zettel/reading-notes"
        ;; bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-notes-extension ".md"
        helm-bibtex-full-frame nil))

;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  ;; markdown hooks
  (add-hook 'markdown-mode-hook
        '(lambda ()
        (turn-on-flyspell) (auto-fill-mode) (centered-cursor-mode) (git-gutter-mode 1)   (hl-todo-mode)))
  (setq markdown-command
    (concat
     "/usr/local/bin/pandoc"
     " --from=markdown --to=html"
     " --standalone --mathjax --highlight-style=pygments"
     " --css=/Users/roambot/.pandoc/pandoc.css"
     " --quiet"
     " --number-sections"
     " --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua"
     " --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua"
     " --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml"
     " --metadata=reference-section-title:'References & Further Reading'"
     " --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib"
     ))

  (setq markdown-enable-math nil
        markdown-enable-wiki-links t
        markdown-nested-imenu-heading-index t
        markdown-open-command "~/bin/mark.sh"
        markdown-footnote-location 'immediately
        markdown-unordered-list-item-prefix "-   "
        markdown-use-pandoc-style-yaml-metadata t)
   :config
   ;; remove strikout comment face
   (set-face-attribute 'markdown-comment-face nil :weight 'bold :strike-through nil))

;; macro: delete backslashes in paragraph to cleanup markdown conversion
(fset 'cpm/md-delete-backslash
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\361\361f\\x" 0 "%d")) arg)))

;;; Markdown TOC
(use-package markdown-toc
  :ensure t
  :after markdown
  :hook (markdown-mode . markdown-toc))

;;; Pandoc
(use-package pandoc-mode
  :commands (cpm/pandoc-convert-to-pdf run-pandoc pandoc-convert-to-pdf)
  :config
  (setq pandoc-use-async t)
  ;; stop pandoc from just hanging forever and not completing conversion
  ;; see https://github.com/joostkremers/pandoc-mode/issues/44
  (setq pandoc-process-connection-type nil)
  (progn
    (defun run-pandoc ()
      "Start pandoc for the buffer and open the menu"
      (interactive)
      (pandoc-mode)
      (pandoc-main-hydra/body))
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

  (defun cpm/pandoc-convert-to-pdf ()
   (interactive)
   (cond
   ((eq major-mode 'org-mode)
    (call-interactively 'org-pandoc-export-to-latex-pdf-and-open))
   (t
    (call-interactively 'pandoc-convert-to-pdf) (cpm/pandoc-pdf-open) (evil-window-prev 1))))

  (defun cpm/pandoc-command-line-convert-to-pdf ()
   "convert to pdf"
   (interactive)
   (evil-ex "!pandoc -s -N -V mainfont=Optima --pdf-engine=xelatex --bibliography=~/Dropbox/Work/bibfile.bib --template=~/.pandoc/pandoc-templates/default.latex -o '%.pdf' '%'"))

  (defun cpm/pandoc-pdf-open ()
   "Open created PDF file"
   (interactive)
   (find-file-other-window (concat (file-name-sans-extension buffer-file-name) ".pdf"))))
  :init
  (progn
    (setq pandoc-data-dir (concat cpm-local-dir "pandoc-mode/"))
    ;; help pandoc find xelatex
    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))))

;;; Writeroom
(use-package writeroom-mode
  :commands (writeroom-mode)
  :config
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-width 90)
  (setq writeroom-mode-line t)
  (setq writeroom-bottom-divider-width 0))

;; Set up a distraction free space
(defun distraction-free ()
  "distraction free writing"
  (interactive)
  (git-gutter-mode 0)
  (linum-mode 0)
  (centered-cursor-mode)
  (writeroom-mode)
  )

;;; Interleave (Notes)
(use-package interleave
  :commands interleave)
  (use-package lorem-ipsum
    :commands (Lorem-ipsum-insert-sentences Lorem-ipsum-insert-list Lorem-ipsum-insert-paragraphs)
    :config
    (lorem-ipsum-use-default-bindings)
    )

;;; Palimpsest (make archive)
  (use-package palimpsest
    :defer t
    :diminish palimpsest-mode
    :hook ((markdown-mode-hook . palimpsest-mode)
           (org-mode-hook . palimpsest-mode))
    :config
    (setq palimpsest-trash-file-suffix ".archive"))

;;; Latex Packages
  ;; Basic settings
(use-package auctex
  :mode (("\\.tex\\'" . latex-mode)
         ("\\.latex\\'" . latex-mode))
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (setq-default TeX-engine 'xetex)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-PDF-mode t)
    (setq-default TeX-master nil)))

(use-package preview
  :ensure nil
  :after auctex
  :commands LaTeX-preview-setup
  :init
  (progn
    (setq-default preview-scale 1.4
      preview-scale-function '(lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))))

(use-package reftex
  :commands turn-on-reftex
  :init
  (progn
    (setq reftex-plug-into-AUCTeX t)))

(use-package bibtex
  :defer t
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))


;; Auto-fill for LaTeX
(defun schnouki/latex-auto-fill ()
  "Turn on auto-fill for LaTeX mode."
  (turn-on-auto-fill)
  (set-fill-column 80)
  (setq default-justification 'left))
(add-hook 'LaTeX-mode-hook #'schnouki/latex-auto-fill)

;; Compilation command
(add-hook 'LaTeX-mode-hook (lambda () (setq compile-command "latexmk -pdflatex=xelatex -f -pdf %f")))

;; Prevent ispell from verifying some LaTeX commands
;; http://stat.genopole.cnrs.fr/dw/~jchiquet/fr/latex/emacslatex
(defvar schnouki/ispell-tex-skip-alists
      '("cite" "nocite"
  "includegraphics"
  "author" "affil"
  "ref" "eqref" "pageref"
  "label"))
(setq ispell-tex-skip-alists
      (list
       (append (car ispell-tex-skip-alists)
         (mapcar #'(lambda (cmd) (list (concat "\\\\" cmd) 'ispell-tex-arg-end)) schnouki/ispell-tex-skip-alists))
       (cadr ispell-tex-skip-alists)))

;; Indentation with align-current in LaTeX environments
(defvar schnouki/LaTeX-align-environments '("tabular" "tabular*"))
(add-hook 'LaTeX-mode-hook
    (lambda ()
      (require 'align)
      (setq LaTeX-indent-environment-list
      ;; For each item in the list...
      (mapcar (lambda (item)
          ;; The car is an environment
          (let ((env (car item)))
            ;; If this environment is in our list...
            (if (member env schnouki/LaTeX-align-environments)
          ;; ...then replace this item with a correct one
          (list env 'align-current)
        ;; else leave it alone
        item)))
        LaTeX-indent-environment-list))))

;; Use dvipdfmx to convert DVI files to PDF in AUCTeX
(eval-after-load 'tex
  '(add-to-list 'TeX-command-list
                '("DVI to PDF" "dvipdfmx %d" TeX-run-command t t) t))

;; SyncTeX (http://www.emacswiki.org/emacs/AUCTeX#toc19)
(defun synctex/un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

;;; Grammar
(use-package typo
  :ensure t
  :defer 2
  :config
  (typo-global-mode))

;;; FIXME Notes / Deft / Zettel
(use-package deft
  :ensure t
  :commands (deft deft-open-file-other-window big-notes zd-new-file zd-deft-new-search deft-new-file-named)
  :general
  (:keymaps 'deft-mode-map :states '(normal motion)
            "o" 'cpm/deft-open
            "p" 'cpm/deft-open-preview)
  (:keymaps 'deft-mode-map :states '(insert)
            "C-j" 'evil-next-line
            "C-k" 'evil-previous-line
            "C-o" 'cpm/deft-open
            "C-p" 'cpm/deft-open-preview)
  :config
  (add-to-list 'evil-insert-state-modes 'deft-mode)
  ;; basic settings for use with zettel
  (setq deft-directory (concat (getenv "HOME") "/Dropbox/notes/zettel/")
        deft-recursive t
        deft-use-filename-as-title t
        deft-separator " "
        deft-extensions '("md" "txt" "org")
        deft-default-extension "md")
  ;; file renaming rules
  (setq deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[a-zA-Z_]+:.*$" ;;org-mode metadata
                ;;yaml metadata
                "\\|^\\-\\{3\\}$"
                "\\|^[a-zA-Z_]+:.*$"
                "\\|@[a-zA-Z_].*$"
                ;; line beginning with markdown links
                "\\|^\\[.*$"
                "\\|^# .*$" ;; md titles
                "\\)"))

  ;;function to run deft in specified directory
  (defun any-deft (dir)
    "Run deft in directory DIR"
    (setq deft-directory dir)
    (switch-to-buffer "*Deft*")
    (kill-this-buffer)
    (require 'org)
    (deft))
  (defun big-notes ()
    "Goto main notes with deft"
    (interactive)
    (any-deft "~/Dropbox/Notes")
    (kill-this-buffer)
    (any-deft "~/Dropbox/Notes"))
  (defun cpm/deft-open ()
    (interactive)
    (deft-open-file-other-window t))
  (defun cpm/deft-open-preview ()
    (interactive)
    (deft-open-file-other-window)))

(defgroup zetteldeft nil
  "A zettelkasten on top of deft.")
(defun zd-get-thing-at-point ()
  "Return the thing at point, which can be a link, tag or word."
  (require 'thingatpt)
  (let* ((link-re "\\[\\[\\([^]]+\\)\\]\\]")
         (htag-re "\\([§#@][[:alnum:]_-]+\\)"))
    (cond
     ((thing-at-point-looking-at link-re)
      (match-string-no-properties 1))
     ((thing-at-point-looking-at htag-re)
      (match-string-no-properties 1))
     (t (thing-at-point 'word t)))))
(defun zd-search-at-point ()
  "Search deft with thing-at-point as filter.
Thing can be a double-bracketed link, a hashtag, or a word."
  (interactive)
  (let ((string (zd-get-thing-at-point)))
    (if string
        (zd-search-global string t)
      (user-error "No search term at point"))))
(defun zd-search-global (str &optional dntOpn)
  "Search deft with STR as filter.
If there is only one result, open that file (unless DNTOPN is true)."
  ;; Sanitize the filter string
  (setq str (replace-regexp-in-string "[[:space:]\n]+" " " str))
  ;; Call deft search on the filter string
  (let ((deft-incremental-search t))
    (deft)
    (deft-filter str t))
  ;; If there is a single match, open the file
  (unless dntOpn
    (when (eq (length deft-current-files) 1)
      (deft-open-file (car deft-current-files)))))
(defun zd-search-filename (thisStr &optional otherWindow)
  "Search for deft files with string THISSTR in filename.
Open if there is only one result (in another window if otherWindow is non-nill)."
  ;; Sanitize the filter string
  (setq thisStr (replace-regexp-in-string "[[:space:]\n]+" " " thisStr))
  ;; Call deft search on the filter string
  (let ((deft-filter-only-filenames t))
    (deft-filter thisStr t))
  ;; If there is a single match, open the file
  (when (eq (length deft-current-files) 1)
    (deft-open-file (car deft-current-files) otherWindow)))
(defun zd-search-current-id ()
  "Search deft with the id of the current file as filter.
Open if there is only one result."
  (interactive)
  (zd--check)
  (zd-search-global (zd-lift-id (file-name-base (buffer-file-name))) t))
(defun zd-get-file-list (srch)
  "Returns a list of files with the search item SRCH."
  (let ((deft-current-sort-method 'title))
    (deft-filter srch t)
    deft-current-files))
(defcustom zd-id-format "%Y-%m%d-%H%M"
  "Format used when generating zetteldeft IDs.
Be warned: the regexp to find these IDs is set separately."
  :type 'string
  :group 'zetteldeft)
(setq deft-new-file-format zd-id-format)
(defun zd-generate-id ()
  "Generates an id in `zd-id-format'."
  (format-time-string zd-id-format))
(defcustom zd-id-regex "[0-9]\\{4\\}\\(-[0-9]\\{2,\\}\\)\\{2\\}"
  "The regex used to search for zetteldeft IDs."
  :type 'string
  :group 'zetteldeft)
(defun zd-lift-id (str)
  "Extract the zetteldeft ID from STR with the regular expression stored in `zd-id-regex'."
  (with-temp-buffer
    (insert str)
    (when (re-search-forward zd-id-regex nil t -1)
      (match-string 0))))
(defun zd-find-file (file)
  "Open deft file FILE."
  (interactive
   (list (completing-read "Deft find file: "
                          (deft-find-all-files-no-prefix))))
  (deft-find-file file))
(defun zd-find-file-id-insert (file)
  "Find deft file FILE and insert its link id"
  (interactive (list
                (completing-read "File to insert id from: "
                                 (deft-find-all-files-no-prefix))))
  (insert (concat "" (zd-lift-id file))))
(defun zd-find-file-full-title-insert (file)
  "Find deft file FILE and insert its link id with title"
  (interactive (list
                (completing-read "File to insert full title from: "
                                 (deft-find-all-files-no-prefix))))
  (insert (concat "" (file-name-base file))))
(defun zd-new-file (str &optional empty)
  "Create a new deft file. Filename is `zd-id-format' appended by STR. No extension needed.

After creating, the title is inserted in org-mode format (unless EMPTY is true) and the full file name is added to the kill ring."
  (interactive (list (read-string "name: ")))
  (let* ((zdId (zd-generate-id))
         (zdName (concat zdId " " str)))
    (setq deft-directory "/Users/roambot/Dropbox/notes/zettel/")
    (deft-new-file-named zdName)
    (kill-new zdName)
    (unless empty (zd-insert-md-title))
    (when (featurep 'evil) (evil-insert-state))))
(defun zd-new-file-and-link (str)
  "Inserts generated id with `zd-id-format' appended with STR.
Creates new deft file with id and STR as name."
  (interactive (list (read-string "name: ")))
  (setq deft-directory "/Users/roambot/Dropbox/notes/zettel/")
  (insert "" (zd-generate-id) "-" str)
  (zd-new-file str))
(defun zd-avy-tag-search ()
  "Call on avy to jump and search tags indicated with #."
  (interactive)
  (save-excursion
    (avy-jump zd-tag-regex)
    (zd-search-at-point)))
(defun zd-avy-link-search ()
  "Call on avy to jump and search link ids indicated with [[.
Opens immediately if there is only one result."
  (interactive)
  (save-excursion
    (avy-goto-char-2 ?\[?\[)
    (zd-search-global (zd-lift-id (zd-get-thing-at-point)))))
(defun zd-avy-file-search (&optional otherWindow)
  "Call on avy to jump to link ids indicated with [[ and use it to search for filenames.
Open that file (when it is the only search result, and in another window if OTHERWINDOW)."
  (interactive)
  (save-excursion
    (avy-goto-char-2 ?\[?\[)
    (forward-char)
    (zd-search-filename (zd-lift-id (zd-get-thing-at-point)) otherWindow)))
(defun zd-avy-file-search-ace-window ()
  "Call on avy to jump to link ids indicated with [[ and use it to search for filenames.
When there is only one search result, as there should be, open that file in a window selected through `ace-window'."
  (interactive)
  (require 'ace-window)
  (save-excursion
    (avy-goto-char ?\[?\[)
    (let ((ID (zd-lift-id (zd-get-thing-at-point))))
      (select-window (aw-select "Select window..."))
      (zd-search-filename ID))))
(defun zd-deft-new-search ()
  "Launch deft, clear filter and enter insert state."
  (interactive)
  (setq deft-directory "/Users/roambot/Dropbox/notes/zettel/")
  (deft)
  (deft-filter-clear)
  (when (featurep 'evil) (evil-insert-state)))
(defun zd--check ()
  "Checks if the currently visited file is in `zetteldeft' territory: whether it has `deft-directory' somewhere in its path."
  (unless (string-match-p
           (regexp-quote deft-directory)
           (file-name-directory (buffer-file-name)))
    (user-error "Not in zetteldeft territory.")))
(defun zd-file-rename ()
  "Rename the current file via the deft function. Use this on files in the deft-directory."
  (interactive)
  (zd--check)
  (let ((old-filename (buffer-file-name))
        (zdId (zd-generate-id))
        (deft-dir (file-name-as-directory deft-directory))
        new-filename old-name new-name)
    (when old-filename
      (setq old-name (deft-base-filename old-filename))
      (setq new-name (read-string
                      (concat "Rename " old-name " to (without extension): ")
                      old-name))
      (setq new-filename
            (concat deft-dir zdId "-" new-name "." deft-default-extension))
      (rename-file old-filename new-filename)
      (deft-update-visiting-buffers old-filename new-filename)
      (zd-update-title-in-file)
      (deft-refresh))))
(defun zd-update-title-in-file ()
  "Update the TITLE in the current file, if present."
  (save-excursion
    (let ((zd-string-after-title ""))
      (goto-char (point-min))
      (when (search-forward "title:" nil t)
        (delete-region (line-beginning-position) (line-end-position))
        (zd-insert-md-title)))))
(defun zd-lift-file-title (zdFile)
  "Returns the title of a zetteldeft note.
ZDFILE should be a full path to a note."
  (let ((baseName (file-name-base zdFile)))
    (replace-regexp-in-string
     "[0-9]\\{2,\\}-[0-9-]+[[:space:]]"
     "" baseName)))
(defun zd-insert-md-title ()
  "Insert filename of current file as md syntax."
  (interactive)
  (zd--check)
  (insert
   "title: "
   (zd-lift-file-title (file-name-base (buffer-file-name)))
   zd-string-after-title))
(defcustom zd-string-after-title ""
  "String inserted below title when `zd-insert-md-title' is called.
Empty by default.
Don't forget to add `\\n' at the beginning to start a new line."
  :type 'string
  :group 'zetteldeft)
(defun zd-count-words ()
  "Prints total number of words and notes in the minibuffer."
  (interactive)
  (let ((numWords 0))
    (dolist (deftFile deft-all-files)
      (with-temp-buffer
        (insert-file-contents deftFile)
        (setq numWords (+ numWords (count-words (point-min) (point-max))))))
    (message "Your zettelkasten contains %s notes with %s words in total." (length deft-all-files) numWords)))
(defun zd-copy-id-current-file ()
  "Add the id from the filename the buffer is currently visiting to the kill ring."
  (interactive)
  (zd--check)
  (let ((ID (concat "" (zd-lift-id (file-name-base (buffer-file-name))))))
    (kill-new ID)
    (message "%s" ID)))
(defun zd-id-to-full-title (zdID)
  "Return full title from given zetteldeft ID.
Throws an error when either none or multiple files with said ID are found."
  (let ((deft-filter-only-filenames t))
    (deft-filter zdID t))
  (unless (eq (length deft-current-files) 1)
    (user-error "ID Error. Either no or multiple zetteldeft files found with ID %s." zdID))
  (file-name-base (car deft-current-files)))
(defun cpm/zettel-dired ()
  (interactive)
  (find-file "~/Dropbox/Notes/zettel")
  (peep-dired))
(defun zd-all-tags ()
  "Return a list of all the tags found in zetteldeft files."
  (setq zd-tag-list (list))
  (dolist (deftFile deft-all-files)
    (zd-extract-tags deftFile))
  zd-tag-list)
(setq zd-tag-buffer-name "*zd-tag-buffer*")
(defun zd-tag-buffer ()
  "Switch to the *zd-tag-buffer* and list tags."
  (interactive)
  (switch-to-buffer zd-tag-buffer-name)
  (erase-buffer)
  (dolist (zdTag (zd-all-tags))
    (insert (format "%s \n" zdTag)))
  (unless (eq major-mode 'org-mode) (org-mode))
  (sort-lines nil (point-min) (point-max)))
(defcustom zd-tag-format "\\(^\\|\s\\)[#@][a_z_]+"
  "Regular expression used to filter out tags."
  :type 'string
  :group 'zetteldeft)
(defun zd-extract-tags (deftFile)
  "Find all tags in DEFTFILE and add them to zd-tag-list"
  (with-temp-buffer
    (insert-file-contents deftFile)
    (while (re-search-forward zd-tag-format nil t)
      (let ((foundTag (replace-regexp-in-string " " "" (match-string 0))))
        ;; Add found tag to zd-tag-list if it isn't there already
        (unless (member foundTag zd-tag-list)
          (push foundTag zd-tag-list)))
      ;; Remove found tag from buffer
      (delete-region (point) (re-search-backward zd-tag-format)))))
(defun zd-insert-list-links (zdSrch)
  "Inserts at point a list of links to all deft files with a search string ZDSRCH.
When searching for a tag, include # manually in the search."
  (interactive (list (read-string "search string: ")))
  (dolist (zdFile (zd-get-file-list zdSrch))
    (zd-list-entry-file-link zdFile)))
(defun zd-insert-list-links-new (zdSrch)
  "Inserst a list of links to all deft files with a search string ZDSRCH, yet in contrast to `zd-insert-list-links' only includes links that are not yet present in the current file.
Can only be called from a file in the zetteldeft directory."
  (interactive (list (read-string "search string: ")))
  (zd--check)
  (let (zdCurrentIDs zdFoundIDs zdFinalIDs)
    (setq zdCurrentIDs (zd-extract-links (buffer-file-name)))
                                        ; filter IDs from search results
    (dolist (zdFile (zd-get-file-list zdSrch))
      (push (zd-lift-id zdFile) zdFoundIDs))
                                        ; create new list with unique ids
    (dolist (zdID zdFoundIDs)
      (unless (member zdID zdCurrentIDs)
        (push zdID zdFinalIDs)))
                                        ; finally find full title for each ID and insert it
    (dolist (zdID zdFinalIDs)
      (setq zdID (zd-id-to-full-title zdID))
      (insert " - " (concat "" zdID "\n")))))
(defun zd-list-entry-file-link (zdFile)
  "Insert ZDFILE as list entry."
  (insert " - " (concat "" (file-name-base zdFile)) "\n"))
(defun zd-org-search-include (zdSrch)
  "Inserts at point org-mode code to include all files with the selected tag. Include the # manually in the prompt."
  (interactive (list (read-string "tag (include the #): ")))
  (dolist (zdFile (zd-get-file-list zdSrch))
    (zd-org-include-file zdFile)))
(defun zd-org-search-insert (zdSrch)
  "Inserts at point all the content of the files with ZDSRCH. When looking for zetteldeft tags, include the # manually in the search."
  (interactive (list (read-string "Search term: ")))
  (dolist (zdFile (zd-get-file-list zdSrch))
    (zd-org-insert-file zdFile)))
(defun zd-file-contents (zdFile &optional removeLines)
  "Inserts file contents of a zetteldeft note.
ZDFILE should be a full path to a note.

Optional: leave out first REMOVELINES lines."
  (with-temp-buffer
    (insert-file-contents zdFile)
    (when removeLines
      (kill-whole-line removeLines))
    (buffer-string)))
(defun zd-org-include-file (zdFile)
  "Insert code to include org-file zdFile."
  (insert
   ;; Insert org-mode title
   "* " (zd-lift-file-title zdFile) "\n"
   ;; Insert #+INCLUDE: "file.org" :lines 2-
   "#+INCLUDE: \"" zdFile "\" :lines \"2-\"\n\n"))
(defun zd-org-insert-file (zdFile)
  "Insert title and contents of ZDFILE."
  (insert
   ;; Insert org-mode title
   "\n* " (zd-lift-file-title zdFile) "\n\n"
   ;; Insert file contents (without the first 3 lines)
   (zd-file-contents zdFile 3)))
(defun zd-org-graph-search (str)
  "Insert org source block for graph with zd search results. STR should be the search the resulting notes of which should be included in the graph."
  (interactive (list (read-string "search string: ")))
  (setq zd-graph--links (list))
  (let ((zdList (zd-get-file-list str)))
    (insert zd-graph-syntax-begin)
    (insert "\n  // links\n")
    (dolist (oneFile zdList)
      (insert "\n")
      (zd-graph-insert-links oneFile))
    (zd-graph-insert-all-titles))
  (insert zd-graph-syntax-end))
(defun zd-org-graph-note (deftFile)
  "Create a graph starting from note DEFTFILE."
  (interactive)
  (setq zd-graph--links (list))
  (insert zd-graph-syntax-begin)
  (insert "\n  // base note and links \n")
  (zd-graph-insert-links deftFile)
  (zd-graph-insert-additional-links)
  (zd-graph-insert-all-titles)
  (insert zd-graph-syntax-end))
(defcustom zd-graph-syntax-begin
  "#+BEGIN_SRC dot :file ./graph.pdf :cmdline -Kfdp -Tpdf
  \n graph {\n"
  "Syntax to be included at the start of the zetteldeft graph.")
(defcustom zd-graph-syntax-end
  "} \n#+END_SRC\n"
  "Syntax to be included at the end of the zetteldeft graph.")
(defun zd-extract-links (deftFile)
  "Find all links in DEFTFILE and return a list."
  (let ((zdLinks (list)))
    (with-temp-buffer
      (insert-file-contents deftFile)
      (while (re-search-forward zd-id-regex nil t)
        (let ((foundTag (replace-regexp-in-string " " "" (match-string 0))))
          ;; Add found tag to zdLinks if it isn't there already
          (unless (member foundTag zdLinks)
            (push foundTag zdLinks)))
        ;; Remove found tag from buffer
        (delete-region (point) (re-search-backward zd-id-regex))))
    zdLinks))
(defun zd-graph-insert-links (deftFile)
  "Inserts a file's links in a one line dot graph format.
Any inserted ID is also stored in `zd-graph--links'."
  (insert "  \""
          (zd-lift-id deftFile)
          "\" -- {")
  (dolist (oneLink (zd-extract-links deftFile))
    (zd-graph-store-link oneLink t)
    (insert "\"" oneLink "\" "))
  (insert "}\n")
  (zd-graph-store-link deftFile))
(defun zd-graph-insert-title (deftFile)
  "Inserts the DEFTFILE title definition in a one line dot graph format."
  (let ((zdTitle (replace-regexp-in-string "\"" "" (zd-lift-file-title deftFile)))
        (zdId    (zd-lift-id deftFile)))
    (insert "  \"" zdId "\""
            " [label = \"" zdTitle " (" zdId ")\"")
    (insert "]" "\n"))
  (zd-graph-store-link deftFile))
(defun zd-graph-store-link (deftFile &optional idToFile)
  "Push DEFTFILE to zd-graph--links unless it's already there.
When IDTOFILE is non-nil, DEFTFILE is considered an id and the the function first looks for the corresponding file."
  (when idToFile
    (let ((deft-filter-only-filenames t))
      (progn
        (deft-filter deftFile t)
        (setq deftFile (car deft-current-files)))))
  (unless (member deftFile zd-graph--links)
    (push deftFile zd-graph--links)))
(defun zd-graph-insert-additional-links ()
  "Insert rest of `zd-graph--links'."
  (setq zd-graph--links (cdr zd-graph--links))
  (dolist (oneFile zd-graph--links)
    (zd-graph-insert-links oneFile)))
(defun zd-graph-insert-all-titles ()
  "Insert all graphviz title lines for all links stored in `zd-graph--links'."
  (insert "\n  // titles \n")
  (dolist (oneLink zd-graph--links)
    ;; Sometimes, a 'nil' list item is present. Ignore those.
    (when oneLink
      (zd-graph-insert-title oneLink))))
(font-lock-add-keywords 'org-mode '(
                                    ("§[0-9]\\{2,\\}-[0-9-]+" . font-lock-warning-face)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-writing)
