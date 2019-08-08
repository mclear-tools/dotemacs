;;; Settings
;;;; General Settings

;;;;; Custom File Location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;;;; Text settings
;; Make sure your text files end in a newline
(setq require-final-newline t)

;; unique buffers
(setq uniquify-buffer-name-style 'forward)
;; Keep focus while navigating help buffers
(setq help-window-select 't)
;; big files
(setq large-file-warning-threshold 100000000)

;; pretty symbols
(setq prettify-symbols-unprettify-at-point t)
(global-prettify-symbols-mode +1)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

; Iterate through CamelCase words
(global-subword-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default indicate-empty-lines nil)
(setq-default fill-column 85)
(global-visual-line-mode)
(setq line-move-visual t) ;; move via visual lines

;;;;; Search and Replace
(use-package visual-regexp
  :commands (vr/query-replace)
  :config
  (use-package visual-regexp-steroids
  :commands (vr/select-query-replace)))

;;;;; UTF 8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;; Interface settings
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)
;; Silence warnings generated by a function's being redefine by =defadvice=.
(setq ad-redefinition-action 'accept)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq show-paren-delay 0)
(blink-cursor-mode 0)


;;;;; Whitespace
;; Manage whitespace in prog modes
(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

;;;; Backups / Auto-Save
(let ((backup-dir (concat cpm-cache-dir "backup")))
  ;; Move backup file to `~/.emacs.d/.local/cache/backup'
  (setq backup-directory-alist `(("." . ,backup-dir)))
  ;; Makesure backup directory exist
  (when (not (file-exists-p backup-dir))
    (make-directory backup-dir t)))


(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made
      kept-new-versions 6               ; newest versions to keep when a new numbered backup is made
      )
(setq vc-make-backup-files t) ;;  backup versioned files, which Emacs does not do by default

(use-package backup-walker
  :commands backup-walker-start)

(setq auto-save-list-file-prefix
      (concat cpm-cache-dir "auto-save-list/.saves-"))
(let ((auto-save-files-dir (concat cpm-cache-dir "auto-save-files")))
  (setq auto-save-file-name-transforms
      `((".*" ,auto-save-files-dir t)))
  (when (not (file-exists-p auto-save-files-dir))
    (make-directory auto-save-files-dir t)))

(setq auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      auto-save-visited-file-name nil
      delete-auto-save-files t
      create-lockfiles nil)

(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))
(add-hook 'auto-save-hook 'full-auto-save)

;; Save all buffers after idle time or exit from insert state
(run-with-idle-timer 5 t (lambda () (save-some-buffers t)))
(add-hook 'evil-insert-state-exit-hook 'full-auto-save)


;;;; Save History
(use-package    savehist-mode
  :ensure nil
  :defer 1
  :config
  (setq-default savehist-file (concat cpm-cache-dir "savehist"))
  (when (not (file-exists-p savehist-file))
    (make-empty-file savehist-file))
  (setq savehist-save-minibuffer-history t)
  (setq history-length 100)
  (put 'minibuffer-history 'history-length 50)
  (put 'evil-ex-history 'history-length 50)
  (put 'kill-ring 'history-length 25)
  (savehist-mode 1))

;;;; Desktop
(when (file-exists-p custom-file)
  (load custom-file))
  (setq desktop-dirname             (concat cpm-cache-dir "desktops")
        desktop-base-file-name      "emacs.desktop"
        desktop-base-lock-name      "lock"
        desktop-path                (list desktop-dirname)
        desktop-save                'ask-if-new
        desktop-files-not-to-save   (concat "^$" ".*magit$")
        desktop-restore-eager 4
        desktop-load-locked-desktop t)

(when (not (file-exists-p desktop-dirname))
(make-directory desktop-dirname t))
(setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
            "\\)$"))

(desktop-save-mode 0)

(defun cpm/my-desktop ()
  "Load the desktop and enable autosaving"
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (desktop-read)
    (desktop-save-mode 1)))

(defun cpm/save-desktop-save-buffers-kill-emacs ()
  "Save buffers and current desktop every time when quitting emacs."
  (interactive)
  (desktop-save-in-desktop-dir)
  (save-buffers-kill-emacs))


;;;; Date & Time
(defun cpm/insert-date ()
  (interactive)
  (format-date "%A, %B %d %Y"))

(defun cpm/insert-date-and-time ()
  (interactive)
  (format-date "%m-%d-%Y %H:%M:%S"))

(defun cpm/time-stamp ()
  (interactive)
  (insert (concat  "Time-stamp: <"(format-time-string "%04y-%02m%02d-%02H:%02M:%02S")">")))

(setq time-stamp-active t          ; do enable time-stamps
      time-stamp-line-limit -10     ; check last 10 buffer lines for Time-stamp:
      time-stamp-format "Last modified on %04y-%02m%02d-%02H:%02M:%02S") ; date format
(add-hook 'before-save-hook 'time-stamp) ; update when saving
(defun format-date (format)
(let ((system-time-locale "en_US.UTF-8"))
  (insert (format-time-string format))))

;;;; Universal Argument
(general-define-key "M-u" 'universal-argument)

;;;; Pop-up Windows
(use-package shackle
    :after helm
    :demand t
    :config
    ;; make helm pop-ups behave
    (setq helm-display-function #'pop-to-buffer)
    (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.46)))
    (shackle-mode 1))

;;;; Information
(use-package helpful
  :config (evil-set-initial-state 'helpful-mode 'motion)
  :general
  ("C-h f" #'helpful-callable)
  ("C-h k" #'helpful-key)
  ("C-h v" #'helpful-variable)
  ("C-c C-." #'helpful-at-point)
  ("C-h C-l" #'find-library)
  :commands (helpful-function helpful-callable helpful-key helpful-variable helpful-at-point))

(advice-add 'describe-package-1 :after #'cpm/describe-package--add-melpa-link)

;; Add melpa link to describe package info
(defun cpm/describe-package--add-melpa-link (pkg)
  (let* ((desc (if (package-desc-p pkg)
                   pkg
                 (cadr (assq pkg package-archive-contents))))
         (name (if desc (package-desc-name desc) pkg))
         (archive (if desc (package-desc-archive desc)))
         (melpa-link (format "https://melpa.org/#/%s" name)))
    (when (equal archive "melpa")
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "Summary:" nil t)
          (forward-line 1)
          (package--print-help-section "MELPA")
          (help-insert-xref-button melpa-link 'help-url melpa-link)
          (insert "\n"))))))

;;;; Server
;; start server for emacsclient
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;;;; Outshine Outline Navigation
(use-package outshine
  :ensure t
  ;; Easier navigation for source files, especially this one.
  :general
  (:keymaps 'outline-minor-mode-map :states '(normal motion)
   "<tab>" 'outshine-cycle
   "M-<tab>" 'outshine-cycle-buffer)
  (:keymaps 'outline-minor-mode-map :states '(normal motion)
   "gh" 'outline-up-heading
   "gj" 'outline-forward-same-level
   "gk" 'outline-backward-same-level
   "gl" 'outline-next-visible-heading
   "gu" 'outline-previous-visible-heading
   "M-RET" 'outshine-insert-heading
   "M-j"   'outline-move-subtree-down
   "M-k"   'outline-move-subtree-up
   "M-h"   'outline-promote
   "M-l"   'outline-demote)
  :config
  (setq outshine-use-speed-commands t
        outshine-cycle-emulate-tab 'white
        outshine-startup-folded-p nil)
  :hook (prog-mode . outshine-mode))

;;;; Emacs Profiling
(use-package esup
  :ensure t
  :config
  (setq esup-depth 0))

;;;; Miscellaneous
(use-package remember
 :ensure nil
 :commands (remember remember-notes)
 :config
 (setq remember-data-dir (concat cpm-cache-dir "remember")
       remember-data-file (concat cpm-cache-dir "remember/notes"))
 (unless (file-directory-p remember-data-dir)
         (make-directory remember-data-dir t)))

(use-package restart-emacs :commands restart-emacs)
(setq confirm-kill-processes nil) ; don't object when quitting

(use-package autorevert
  :ensure nil
  :defer 1
  :config
  (setq auto-revert-interval .5)
  (global-auto-revert-mode)
  (setq auto-revert-verbose nil ; Shut up, please!
        revert-without-query '(".*") ;; disable revert query
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t))

(use-package request
  :defer t
  :ensure nil
  :config
  (setq request-storage-directory (concat cpm-cache-dir "request")))

(use-package multi-compile
  :ensure t
  :defer 3
  :config
  (setq multi-compile-history-file (concat cpm-cache-dir "multi-compile.cache"))
  (setq multi-compile-completion-system 'helm)
  (setq multi-compile-alist '(
                              ;; commands for pandoc
                              (markdown-mode . (
                                                ("pandoc-normalize" . "pandoc -f markdown -t markdown -s --atx-headers --columns=85 --wrap=auto --reference-location=block -o %file-name %file-name")
                                                ("pandoc-sep-html & Open" . "pandoc -f markdown -t html4 -s --base-header-level=1 --number-sections --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/sep.html4 --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name && open %file-sans.html")
                                                ("pandoc-pdf & Open" . "pandoc -s -V mainfont=Optima --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ("pandoc-beamer-compile-presentation" . "pandoc -i --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && pandoc --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name")
                                                ("pandoc-beamer & Open" . "pandoc -i --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && open %file-sans.pdf")
                                                ("pandoc-beamer-handout & Open" . "pandoc --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name && open %file-sans-handout.pdf")
                                                ("pandoc-handout & Open" . "pandoc -s -V mainfont=Optima --pdf-engine=xelatex  --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --template=/Users/Roambot/.pandoc/pandoc-templates/tufte.tex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ("pandoc-docx & Open" . "pandoc -s --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.docx %file-name && open %file-sans.docx")
                                                ("pandoc-html & Open" . "pandoc -f markdown -t html5 -s --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/default.html5 --css=/Users/roambot/.pandoc/pandoc.css --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name && open %file-sans.html")
                                                ("pandoc-pdf" . "pandoc -s -V mainfont=Optima --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name")
                                                ("pandoc-docx" . "pandoc -s --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.docx %file-name")
                                                ("pandoc-html" . "pandoc -f markdown -t html5 -s --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/default.html5 --css=/Users/roambot/.pandoc/pandoc.css --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name")
                                                ("pandoc-handout" . "pandoc -s -V mainfont=Optima --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --template=/Users/Roambot/.pandoc/pandoc-templates/tufte.tex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name")
                                                ("test pdf" . "pandoc -s -V mainfont=Optima --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ("pandoc-letter-pdf & Open" . "pandoc -s -V mainfont=Optima --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --template=/Users/Roambot/dotfiles/pandoc/pandoc-templates/letter.tex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")))
                              ((string/starts-with buffer-file-name "/Users/roambot/Dropbox/Work/projects/Book-Projects/rationality-book/") . (("compile rationality book" . "cd %make-dir && make -k && open %make-dirbuild/pdf/book.pdf"))))))

;; I think this goes with multi-compile
(defun string/starts-with (string prefix)
    "Return t if STRING starts with prefix."
    (and (stringp string) (string-match (rx-to-string `(: bos ,prefix) t) string)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-settings)
