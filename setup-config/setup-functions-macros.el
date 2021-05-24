;;; Useful Functions

;;;; Delete Frame or Quit
(defun cpm/delete-frame-or-quit ()
  "Delete the selected frame.  If the last one, kill Emacs."
  (interactive)
  (condition-case nil (delete-frame) (error (save-buffers-kill-emacs))))

;;;; Undo-Redo
;; Copied from Emacs 28
(when (version< emacs-version "28.0")
  (defun undo-only (&optional arg)
    "Undo some previous changes.
Repeat this command to undo more changes. A numeric ARG serves as
a repeat count. Contrary to `undo', this will not redo a previous
undo."
    (interactive "*p")
    (let ((undo-no-redo t)) (undo arg)))

  (defun undo--last-change-was-undo-p (undo-list)
    (while (and (consp undo-list) (eq (car undo-list) nil))
      (setq undo-list (cdr undo-list)))
    (gethash undo-list undo-equiv-table))

  (defun undo-redo (&optional arg)
    "Undo the last ARG undos."
    (interactive "*p")
    (cond
     ((not (undo--last-change-was-undo-p buffer-undo-list))
      (user-error "No undo to undo"))
     (t
      (let* ((ul buffer-undo-list)
             (new-ul
              (let ((undo-in-progress t))
                (while (and (consp ul) (eq (car ul) nil))
                  (setq ul (cdr ul)))
                (primitive-undo arg ul)))
             (new-pul (undo--last-change-was-undo-p new-ul)))
        (message "Redo%s" (if undo-in-region " in region" ""))
        (setq this-command 'undo)
        (setq pending-undo-list new-pul)
        (setq buffer-undo-list new-ul))))))

;;;; Make Temp Buffer
(defun cpm/tmp-buffer()
  "Make a temporary buffer and switch to it"
  (interactive)
  (switch-to-buffer (get-buffer-create (concat "tmp-" (format-time-string "%m.%dT%H.%M.%S"))))
  (delete-other-windows))

;;;; Mode line benchmark
;; https://emacs.stackexchange.com/a/59221/11934
(defmacro mode-line-benchmark-elapse-time (&rest forms)
  "Return the time in seconds elapsed for execution of FORMS."
  (declare (indent 0) (debug t))
  (let ((t1 (make-symbol "t1")))
    `
    (let (,t1)
      (setq ,t1 (current-time))
      ,@forms
      (float-time (time-since ,t1)))))

(defun mode-line-benchmark ()
  (interactive)
  (let* ((value nil)
         (repetitions 10000)
         (wall-clock-time
          (mode-line-benchmark-elapse-time
           (dotimes (_ repetitions)
             (setq value (format-mode-line mode-line-format)))))
         (average-time (/ wall-clock-time repetitions)))
    (message
     "Time: %.10f per call, %.10f for %S calls: %S"
     average-time wall-clock-time repetitions value)))
;;;; Revert all buffers
;;
(defun cpm/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))
;;;; Insert Weather
;; From [[https://www.baty.blog/2019/insert-weather-into-emacs-buffer][Jack Baty]] with some slight modifications for formatting. See also [[https://github.com/chubin/wttr.in][wttr.in]]. 
(defun cpm/insert-weather ()
  (interactive)
  (let ((w (shell-command-to-string "curl -s 'wttr.in/?0qT'")))
    (insert (mapconcat (function (lambda (x) (format ": %s" x)))
                       (split-string w "\n")
                       "\n")))
  (newline))


;;;; Org Archive
(defun cpm/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'agenda))

;;;; Blank Buffer New Frame
;; Make a blank buffer when opening a new frame. From
;; https://stackoverflow.com/a/25792276.

(defun cpm/new-buffer-new-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

;;;; Built-in Functions
;; These are useful built-in functions, but you have to enable them
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Not going to use these commands
(put 'ns-print-buffer 'disabled t)
(put 'suspend-frame 'disabled t)

;;;; Call an emacs instance
;; Call an emacs instance for testing

(defun cpm/call-emacs ()
  (interactive)
  (start-process "Emacs" nil
                 ;; (executable-find "/usr/local/bin/emacs")))
                 ;; (executable-find "/Applications/Emacs.app/Contents/MacOS/Emacs")))
                 (executable-find "Emacs")))

;;;; Clipboard to/from Buffer
;; http://stackoverflow.com/a/10216338/4869
(defun cpm/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun cpm/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;;;; Goto Files
(defun cpm/goto-private ()
  (interactive)
  (find-file (concat cpm-elisp-dir "private.el")))
(defun cpm/goto-journal ()
  (interactive)
  (find-file (concat org-directory "journal.org")))
(defun goto-early-init.el ()
  "Open early-init.el file"
  (interactive)
  (find-file "~/.emacs.d/early-init.el"))
(defun goto-init.el ()
  "Open init.el file"
  (interactive)
  (find-file user-init-file))
(defun goto-custom.el ()
  "Open custom.el file"
  (interactive)
  (find-file custom-file))
(defun goto-config.org ()
  "Open config.org file"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(defun load-config ()
  "Load config "
  (interactive)
  ;; (cpm/tangle-emacs-config)
  (load-file user-init-file))
(defun goto-dotfiles.org ()
  "Open dotfiles.org file"
  (interactive)
  (find-file "~/dotfiles/dotfiles.org"))
(defun goto-emacs-dir ()
  "Open dotfiles.org file"
  (interactive)
  (find-file user-emacs-directory))
(defun goto-cpm-elisp-dir ()
  (interactive)
  (find-file cpm-elisp-dir))
(defun goto-org-files ()
  "Open directory with org files"
  (interactive)
  (find-file org-directory))
(defun goto-pandoc-config ()
  "open pandoc metadata file"
  (interactive)
  (find-file "~/.pandoc/metadata.yml"))


;;;; Formatted Copy
(defun formatted-copy ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))

(global-set-key (kbd "H-w") 'formatted-copy)

;;;; CRUX
(use-package crux :defer 1)

;;;; Useful Buffers
(defun cpm/user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
  Typically, if buffer name starts with *, it's not considered a user buffer.
  This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
  You can override this function to get your idea of “user buffer”.
  version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

(defun cpm/next-user-buffer ()
  "Switch to the next user buffer.
  “user buffer” is determined by `cpm/user-buffer-q'.
  URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
  Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (cpm/user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun cpm/previous-user-buffer ()
  "Switch to the previous user buffer.
  “user buffer” is determined by `cpm/user-buffer-q'.
  URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
  Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (cpm/user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

;;;; Delete Current File
;; from magnars

(defun cpm/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;;;; Duplicate file
;; Duplicate a file in dired or deer
(defun cpm/duplicate-file ()
  (interactive)
  (dired-do-copy-regexp "\\(.*\\)\\.\\(.*\\)" "\\1 (copy).\\2"))

;;;; Ediff Hydra
;; From the hydra wiki https://github.com/abo-abo/hydra/wiki/Emacs#ediff

(with-eval-after-load 'ediff
  (defhydra hydra-ediff (:color blue :hint nil)
    "
  ^Buffers           Files           VC                     Ediff regions
  ----------------------------------------------------------------------
  _b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
  _B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
  _c_urrent file
  "
    ("b" ediff-buffers)
    ("B" ediff-buffers3)
    ("=" ediff-files)
    ("f" ediff-files)
    ("F" ediff-files3)
    ("c" ediff-current-file)
    ("r" ediff-revision)
    ("l" ediff-regions-linewise)
    ("w" ediff-regions-wordwise)))
;; esc quits

;;;; Quit All the Things!
;; From a great vim migration guide by Juanjo Álvarez
;; https://juanjoalvarez.net/en/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
;; (original code from davvil) https://github.com/davvil/.emacs.d/blob/master/init.el

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(with-eval-after-load 'evil
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state))

;;;; Eval emacs buffer until error

(defun cpm/eval-buffer-until-error ()
  "Evaluate emacs buffer until error occured."
  (interactive)
  (goto-char (point-min))
  (while t (eval (read (current-buffer)))))

;;;; Fill or Unfill
(defun cpm/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  ;; if in an org buffer use org-fill-paragraph
  (if (derived-mode-p 'org-mode)
      (org-fill-paragraph)
    (let ((fill-column
           (if (eq last-command 'cpm/fill-or-unfill)
               (progn (setq this-command nil)
                      (point-max))
             fill-column)))
      (call-interactively #'fill-paragraph))))

(global-set-key [remap fill-paragraph]
                #'cpm/fill-or-unfill)

;;;; Unfill
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;; Insert seconds
(defun cpm/insert-seconds-epoch ()
  (interactive)
  (insert (format-time-string "%s"))) ; the integer number of seconds since the epoch
(global-set-key (kbd "C-c e") 'cpm/insert-seconds-epoch)

;;;; Jump in Buffer
(defun cpm/jump-in-buffer ()
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (call-interactively 'consult-org-heading))
   (t
    (call-interactively 'consult-outline))))

;;;; Resume
;; resume last jump
(defun cpm/resume-last-jump ()
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (call-interactively 'ivy-resume))
   (t
    (call-interactively 'helm-resume))))

;;;; Jump to sexp

(defun cpm/forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;;;; Kill Current Buffer
;; (kill-this-buffer) is unreliable when not invoked from the menubar. So here's a
;; wrapper on (kill-buffer) to kill the current buffer. This is sometimes better
;; than (evil-delete-buffer) since it keeps the window.

(defun cpm/kill-this-buffer ()
  (interactive)
  (kill-buffer))

;;;; Make Move
(defun cpm/make-move ()
  "move files to project web directory"
  (interactive)
  (evil-ex "!make move"))

;;;; Make Parent Directory
;;  Create a directory – or a hierarchy of them – while finding a file in a
;;  nonexistent directory. From mbork
;;  http://mbork.pl/2016-07-25_Making_directories_on_the_fly

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'make-parent-directory)

;;;; Move File
(defun cpm/move-file ()
  "Write this file to a new location, and delete the old one."
  (interactive)
  (let ((old-location (buffer-file-name)))
    (call-interactively #'write-file)
    (when old-location
      (delete-file old-location))))

;;;; Narrow/Widen
(defun cpm/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, markdown
  subtree, or defun, whichever applies first. Narrowing to
  org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'markdown-mode)
         (markdown-narrow-to-subtree))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;;;; New Git Project
(defun cpm/git-new-project ()
  "Initializes a new git repo and adds it to projectile's known projects."
  (interactive)
  (let ((project-dir (expand-file-name
                      (read-directory-name "New project root:"))))
    (magit-init project-dir)
    (projectile-add-known-project project-dir)
    (setq default-directory project-dir)))
  (defun cpm/goto-projects ()
      "Open projects dir"
      (interactive)
      (find-file "~/Dropbox/Work/projects"))

;;;; Open in iTerm
(defun cpm/open-dir-in-iterm ()
  "Open the current directory of the buffer in iTerm."
  (interactive)
  (let* ((iterm-app-path "/Applications/iTerm.app")
         (iterm-brew-path "/opt/homebrew-cask/Caskroom/iterm2/2.1.4/iTerm.app")
         (iterm-path (if (file-directory-p iterm-app-path)
                         iterm-app-path
                       iterm-brew-path)))
    (shell-command (concat "open -a " iterm-path " ."))))
(global-set-key (kbd "C-x t") 'open-dir-in-iterm)


;;;; Org Tree/Heading to New File
(defun cpm/org-tree-to-new-file ()
  (interactive)
  "Move an org subtree to a new file"
  (org-copy-subtree nil t)
  (find-file-other-window
   (read-file-name "Move subtree to file:" ))
  (org-paste-subtree))

;;;; Org Wrap in Block Template
;; A helpful function I found for wrapping text in a block template.
;; http://pragmaticemacs.com/emacs/wrap-text-in-an-org-mode-block/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-block-wrap ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(
                      ("a" . "ascii")
                      ("c" . "comment")
                      ("C" . "center")
                      ("e" . "example")
                      ("E" . "src emacs-lisp")
                      ("h" . "html")
                      ("l" . "laTeX")
                      ("n" . "notes")
                      ("q" . "quote")
                      ("s" . "src")
                      ("v" . "verse")
                      ))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+end_" choice "\n")
                (goto-char start)
                (insert "#+begin_" choice "\n")))
             (t
              (insert "#+begin_" choice "\n")
              (save-excursion (insert "#+end_" choice))))))))))


;;;; Org Export Body to HTML Buffer
(defun cpm/org-export-to-buffer-html-as-body (&optional async subtreep visible-only body-only ext-plist)
  "Export org buffer body to html"
  (interactive)
  (org-export-to-buffer 'html "*Org HTML Export*"
    async body-only ext-plist (lambda () (html-mode)))
  (cpm/copy-whole-buffer-to-clipboard)
  (delete-windows-on "*Org HTML Export*")
  (message "HTML copied!"))
;; (cpm/previous-user-buffer))


;;;; Clipboard Transforms Using Pandoc
(defun cpm/org-to-markdown ()
  "convert clipboard contents from org to markdown and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t markdown"))
  (yank))

(defun cpm/markdown-to-org ()
  "convert clipboard contents from markdown to org and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f markdown -t org"))
  (yank))

(defun cpm/tex-to-org ()
  "convert clipboard contents from markdown to org and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f latex -t org"))
  (yank))

(defun cpm/org-to-tex ()
  "convert clipboard contents from org to tex and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t latex"))
  (yank))

(defun cpm/tex-to-markdown ()
  "convert clipboard contents from markdown to org and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f latex -t markdown --atx-headers"))
  (yank))

(defun cpm/markdown-to-tex ()
  "convert clipboard contents from markdown to org and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f markdown -t latex"))
  (yank))

(defun cpm/cite-to-org ()
  "convert clipboard contents from markdown to org with citations and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc --bibliography=/Users/Roambot/Dropbox/Work/Master.bib -s -t markdown-native_divs-raw_html-citations | pandoc -f markdown -t org"))
  (yank))

(defun cpm/cite-to-markdown ()
  "convert clipboard contents to markdown with citations and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -s -t markdown-native_divs-raw_html-citations --atx-headers"))
  (yank))

(defun cpm/bibtex-to-yaml-reference ()
  "convert clipboard bibtex contents to yaml and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc-citeproc -y -f bibtex | pbcopy"))
  (yank))

(defun cpm/md-to-rtf ()
  "convert md to rtf and send to clipboard"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f markdown -t rtf | pbcopy"))
  (yank))


;; NOTE: piping to pbcopy doesn't seem to work but it is ready to paste as is
(defun cpm/org-to-rtf ()
  "convert org to rtf and send to clipboard"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t html | /usr/bin/textutil -stdin -stdout -format html -convert rtf -fontsize 14 -font Helvetica | pbcopy")))

(defun cpm/org-to-mail-rtf ()
  "copy buffer, convert clipboard contents from org to rtf, and send to mail message"
  (interactive)
  (cpm/copy-whole-buffer-to-clipboard)
  ;; (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t rtf"))
  (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t html | /usr/bin/textutil -stdin -stdout -format html -convert rtf -fontsize 14 | pbcopy"))
  (kill-buffer)
  (delete-frame)
  (do-applescript "if application \"Mail\" is running then
  tell application \"Mail\"
  activate
  delay 0.35
  tell application \"System Events\"
  keystroke \"v\" using {command down}
  end tell
  end tell
  end if"))


;;;; Mailmate save mail and kill client
;; Save buffer and exit emacsclient for Mailmate
(defun cpm/email-save-and-kill ()
  (interactive)
  (save-buffer)
  (server-edit))
;;;; Helm Projectile
(defun cpm/helm-projectile-find-file-other-window ()
  "Find a file in a project and open in a vertical split"
  (interactive)
  (cpm/split-window-right-and-focus)
  (helm-projectile-find-file))

;;;; List Helm Search Buffers
(defun cpm/helm-list-search-buffers ()
  "get list of helm search buffers"
  (interactive)
  (helm-resume 1))

;;;; Open directory in Finder
(defun cpm/browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

;;;; Rotate Windows
;; from magnars modified by ffevotte for dedicated windows support
(defun cpm/rotate-windows (count)
  "Rotate your windows.
  Dedicated windows are left untouched. Giving a negative prefix
  argument takes the kindows rotate backwards."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))

                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

(defun cpm/rotate-windows-backward (count)
  "Rotate your windows backward."
  (interactive "p")
  (cpm/rotate-windows (* -1 count)))

;;;; Helm Ag Directory Search
(defun cpm/helm-files-do-ag (&optional dir)
  "Search in files with `ag' using a default input."
  (interactive)
  (require 'helm-ag)
  (helm-do-ag dir))

;;;; Make Window  a Frame
;;;; Helm Search Directory
(defun cpm/helm-files-search-current-directory ()
  "search in files with `ag' in current buffer's directory"
  (interactive)
  (helm-do-ag (file-name-directory buffer-file-name)))

;;;; Show Filename of Buffer

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun cpm/show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer and copy to clipboard."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

;;;; Spelling Goto Next Error
(defun cpm/flyspell-ispell-goto-next-error ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

;;;; Smart Yanking
;;Courtesy of Marcin Borkowski http://mbork.pl/2018-07-02_Smart_yanking

(defun has-space-at-boundary-p (string)
  "Check whether STRING has any whitespace on the boundary.
  Return 'left, 'right, 'both or nil."
  (let ((result nil))
    (when (string-match-p "^[[:space:]]+" string)
      (setq result 'left))
    (when (string-match-p "[[:space:]]+$" string)
      (if (eq result 'left)
          (setq result 'both)
        (setq result 'right)))
    result))

(defun is-there-space-around-point-p ()
  "Check whether there is whitespace around point.
  Return 'left, 'right, 'both or nil."
  (let ((result nil))
    (when (< (save-excursion
               (skip-chars-backward "[:space:]"))
             0)
      (setq result 'left))
    (when (> (save-excursion
               (skip-chars-forward "[:space:]"))
             0)
      (if (eq result 'left)
      (setq result 'both)
    (setq result 'right)))
    result))

(defun set-point-before-yanking (string)
  "Put point in the appropriate place before yanking STRING."
  (let ((space-in-yanked-string (has-space-at-boundary-p string))
    (space-at-point (is-there-space-around-point-p)))
    (cond ((and (eq space-in-yanked-string 'left)
        (eq space-at-point 'left))
       (skip-chars-backward "[:space:]"))
      ((and (eq space-in-yanked-string 'right)
        (eq space-at-point 'right))
       (skip-chars-forward "[:space:]")))))

(defun set-point-before-yanking-if-in-text-mode (string)
  "Invoke `set-point-before-yanking' in text modes."
  (when (derived-mode-p 'text-mode)
    (set-point-before-yanking string)))

(advice-add
 'insert-for-yank
 :before
 #'set-point-before-yanking-if-in-text-mode)
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2007-05/msg00975.html

;;;; Exchange Windows
;; Swap buffers in windows and leave the cursor in the original window. Courtesy of
;; Mike Zamansky's video.
;; http://cestlaz.github.io/posts/using-emacs-36-touch-of-elisp/#.WX5Wg0czpcx

(defun cpm/window-exchange-buffer ()
  "Swap buffer in windows and leave focus in original window"
  (interactive)
  (ace-swap-window)
  (aw-flip-window))

;;;; Switch previous buffer

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


;;;; Transpose hydra
;; From the hydra wiki https://github.com/abo-abo/hydra/wiki/Emacs#transpose

(with-eval-after-load 'hydra
  (general-define-key "C-c t"
                      (defhydra hydra-transpose (:color red)
                        "Transpose"
                        ("c" transpose-chars "characters")
                        ("w" transpose-words "words")
                        ("o" org-transpose-words "Org mode words")
                        ("l" transpose-lines "lines")
                        ("s" transpose-sentences "sentences")
                        ("e" org-transpose-element "Org mode elements")
                        ("p" transpose-paragraphs "paragraphs")
                        ("t" org-table-transpose-table-at-point "Org mode table")
                        ("q" nil "cancel" :color blue))))

;;;; Split Windows
(defun cpm/toggle-window-split ()
  "Move from a horizontal to a vertical split and vice versa"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;;; Toggle markup
(defun cpm/toggle-display-markup ()
  "Toggle the display of markup in markdown and org modes"
  (interactive)
  (if (eq major-mode 'org-mode)
      (org-toggle-link-display)
    (if markdown-hide-markup
        (markdown-toggle-markup-hiding 0)
      (markdown-toggle-markup-hiding))))

;;;; Search TODOs
;; Make an equivalent of vim's quickfix buffer using helm-ag
;; git:~/.emacs.d/config.org::master@{2018-02-18}::3795 and highlight-todo
;; git:~/.emacs.d/config.org::master@{2018-02-18}::2947

(defun cpm/search-file-todo-markers ()
  "Search file for any TODO markers as specified in hl-todo-keyword-faces. Note that this uses the word boundary \\b to avoid matching these within other words, but this means that non-word keywords such as ???, which is in the list by default, will not be matched."
  (interactive)
  (require 'projectile)
  (require 'helm-ag)
  (let* ((grouped (funcall #'regexp-opt (--map (car it) hl-todo-keyword-faces)))
         (unescaped (s-replace-all '(("\\(" . "(") ("\\)" . ")") ("\\|" . "|"))
                                   grouped))
         (bounded (concat "\\b" unescaped "\\b"))
         (helm-follow-mode-persistent t))
    (helm-do-ag-this-file bounded)))

(defun cpm/search-todo-markers ()
  "Search directory for any TODO markers as specified in hl-todo-keyword-faces. Note that this uses the word boundary \\b to avoid matching these within other words, but this means that non-word keywords such as ???, which is in the list by default,will not be matched."
  (interactive)
  (require 'projectile)
  (let* ((grouped (funcall #'regexp-opt (--map (car it) hl-todo-keyword-faces)))
         (unescaped (s-replace-all '(("\\(" . "(") ("\\)" . ")") ("\\|" . "|"))
                                   grouped))
         (bounded (concat "\\b" unescaped "\\b"))
         (helm-follow-mode-persistent t))
    (helm-do-ag (projectile-project-root) nil bounded)))

;;;; Wrap in Yaml block
(defun cpm/yaml-wrap ()
  "wrap region in --- for a yaml block"
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "---" "\n")
    (goto-char start)
    (insert "---" "\n")))


;;;; Search given directory
(defun cpm/search-in-input-dir ()
  "Grep for a string in the input directory using completing read function"
  (interactive)
  (let ((current-prefix-arg '(4))) (call-interactively #'consult-grep)))

;; (let ((current-prefix-arg '(4))) (call-interactively #'counsel-rg)))

;;; Doom Functions & Macros
;; A set of fantastic macros written by hlissner
;; https://github.com/hlissner
;; There won't be much documentation around these because the comments for each macro
;; does a great job explaining their function. For more information you can also look
;; at the wiki https://github.com/hlissner/doom-emacs/wiki and the entry on macros in
;; particular. https://github.com/hlissner/doom-emacs/wiki/Modules#macros

;;;; After!
(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
  compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (bound-and-true-p byte-compile-current-file))
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         #'progn
       #'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

;;;; Map!
(eval-and-compile
  (defun cmacs-enlist (exp)
    "Return EXP wrapped in a list, or as-is if already a list."
    (if (listp exp) exp (list exp)))

  (defun doom-unquote (exp)
    "Return EXP unquoted."
    (while (memq (car-safe exp) '(quote function))
      (setq exp (cadr exp)))
    exp)

  (defvar cmacs-evil-state-alist
    '((?n . normal)
      (?v . visual)
      (?i . insert)
      (?e . emacs)
      (?o . operator)
      (?m . motion)
      (?r . replace))
    "A list of cons cells that map a letter to a evil state symbol.")

  ;; Register keywords for proper indentation (see `map!')
  (put ':after        'lisp-indent-function 'defun)
  (put ':desc         'lisp-indent-function 'defun)
  (put ':leader       'lisp-indent-function 'defun)
  (put ':local        'lisp-indent-function 'defun)
  (put ':localleader  'lisp-indent-function 'defun)
  (put ':map          'lisp-indent-function 'defun)
  (put ':map*         'lisp-indent-function 'defun)
  (put ':mode         'lisp-indent-function 'defun)
  (put ':prefix       'lisp-indent-function 'defun)
  (put ':textobj      'lisp-indent-function 'defun)
  (put ':unless       'lisp-indent-function 'defun)
  (put ':when         'lisp-indent-function 'defun)

  ;; specials
  (defvar cmacs--keymaps nil)
  (defvar cmacs--prefix  nil)
  (defvar cmacs--defer   nil)
  (defvar cmacs--local   nil)

  (defun cmacs--keybind-register (key desc &optional modes)
    "Register a description for KEY with `which-key' in MODES.

  KEYS should be a string in kbd format.
  DESC should be a string describing what KEY does.
  MODES should be a list of major mode symbols."
    (if modes
        (dolist (mode modes)
          (which-key-add-major-mode-key-based-replacements mode key desc))
      (which-key-add-key-based-replacements key desc)))

  (defun cmacs--keyword-to-states (keyword)
    "Convert a KEYWORD into a list of evil state symbols.

  For example, :nvi will map to (list 'normal 'visual 'insert). See
  `cmacs-evil-state-alist' to customize this."
    (cl-loop for l across (substring (symbol-name keyword) 1)
             if (cdr (assq l cmacs-evil-state-alist))
             collect it
             else
             do (error "not a valid state: %s" l)))

  (defmacro map! (&rest rest)
    "A nightmare of a key-binding macro that will use `evil-define-key*',
  `define-key', `local-set-key' and `global-set-key' depending on context and
  plist key flags (and whether evil is loaded or not). It was designed to make
  binding multiple keys more concise, like in vim.

  If evil isn't loaded, it will ignore evil-specific bindings.

  States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace

  These can be combined (order doesn't matter), e.g. :nvi will apply to
  normal, visual and insert mode. The state resets after the following
  key=>def pair.

  If states are omitted the keybind will be global.

  This can be customized with `cmacs-evil-state-alist'.

  :textobj is a special state that takes a key and two commands, one for the
  inner binding, another for the outer.

  Flags
  (:mode [MODE(s)] [...])    inner keybinds are applied to major MODE(s)
  (:map [KEYMAP(s)] [...])   inner keybinds are applied to KEYMAP(S)
  (:map* [KEYMAP(s)] [...])  same as :map, but deferred
  (:prefix [PREFIX] [...])   assign prefix to all inner keybindings
  (:after [FEATURE] [...])   apply keybinds when [FEATURE] loads
  (:local [...])             make bindings buffer local; incompatible with keymaps!

  Conditional keybinds
  (:when [CONDITION] [...])
  (:unless [CONDITION] [...])

  Example
  (map! :map magit-mode-map
        :m \"C-r\" 'do-something           ; assign C-r in motion state
        :nv \"q\" 'magit-mode-quit-window  ; assign to 'q' in normal and visual states
        \"C-x C-r\" 'a-global-keybind

        (:when IS-MAC
         :n \"M-s\" 'some-fn
         :i \"M-o\" (lambda (interactive) (message \"Hi\"))))"
    (let ((cmacs--keymaps cmacs--keymaps)
          (cmacs--prefix  cmacs--prefix)
          (cmacs--defer   cmacs--defer)
          (cmacs--local   cmacs--local)
          key def states forms desc modes)
      (while rest
        (setq key (pop rest))
        (cond
         ;; it's a sub expr
         ((listp key)
          (push (macroexpand `(map! ,@key)) forms))

         ;; it's a flag
         ((keywordp key)
          (cond ((eq key :leader)
                 (push 'cmacs-leader-key rest)
                 (setq key :prefix
                       desc "<leader>"))
                ((eq key :localleader)
                 (push 'cmacs-localleader-key rest)
                 (setq key :prefix
                       desc "<localleader>")))
          (pcase key
            (:when    (push `(if ,(pop rest)       ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
            (:unless  (push `(if (not ,(pop rest)) ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
            (:after   (push `(after! ,(pop rest)   ,(macroexpand `(map! ,@rest))) forms) (setq rest '()))
            (:desc    (setq desc (pop rest)))
            (:map*    (setq cmacs--defer t) (push :map rest))
            (:map
             (setq cmacs--keymaps (cmacs-enlist (pop rest))))
            (:mode
             (setq modes (cmacs-enlist (pop rest)))
             (unless cmacs--keymaps
               (setq cmacs--keymaps
                     (cl-loop for m in modes
                              collect (intern (format "%s-map" (symbol-name m)))))))
            (:textobj
             (let* ((key (pop rest))
                    (inner (pop rest))
                    (outer (pop rest)))
               (push (macroexpand `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                         (:map evil-outer-text-objects-map ,key ,outer)))
                     forms)))
            (:prefix
             (let ((def (pop rest)))
               (setq cmacs--prefix `(vconcat ,cmacs--prefix (kbd ,def)))
               (when desc
                 (push `(cmacs--keybind-register ,(key-description (eval cmacs--prefix))
                                                 ,desc ',modes)
                       forms)
                 (setq desc nil))))
            (:local
             (setq cmacs--local t))
            (_ ; might be a state cmacs--prefix
             (setq states (cmacs--keyword-to-states key)))))

         ;; It's a key-def pair
         ((or (stringp key)
              (characterp key)
              (vectorp key)
              (symbolp key))
          (unwind-protect
              (catch 'skip
                (when (symbolp key)
                  (setq key `(kbd ,key)))
                (when (stringp key)
                  (setq key (kbd key)))
                (when cmacs--prefix
                  (setq key (append cmacs--prefix (list key))))
                (unless (> (length rest) 0)
                  (user-error "map! has no definition for %s key" key))
                (setq def (pop rest))
                (when desc
                  (push `(cmacs--keybind-register ,(key-description (eval key))
                                                  ,desc ',modes)
                        forms))
                (cond ((and cmacs--local cmacs--keymaps)
                       (push `(lwarn 'cmacs-map :warning
                                     "Can't local bind '%s' key to a keymap; skipped"
                                     ,key)
                             forms)
                       (throw 'skip 'local))
                      ((and cmacs--keymaps states)
                       (dolist (keymap cmacs--keymaps)
                         (push `(,(if cmacs--defer 'evil-define-key 'evil-define-key*)
                                 ',states ,keymap ,key ,def)
                               forms)))
                      (states
                       (dolist (state states)
                         (push `(define-key
                                  ,(intern (format "evil-%s-state-%smap" state (if cmacs--local "local-" "")))
                                  ,key ,def)
                               forms)))
                      (cmacs--keymaps
                       (dolist (keymap cmacs--keymaps)
                         (push `(define-key ,keymap ,key ,def) forms)))
                      (t
                       (push `(,(if cmacs--local 'local-set-key 'global-set-key) ,key ,def)
                             forms))))
            (setq states '()
                  cmacs--local nil
                  desc nil)))

         (t (user-error "Invalid key %s" key))))
      `(progn ,@(nreverse forms)))))

;;;; Add-hook!
;;  A macro that makes adding hooks easy

(eval-and-compile
  (defun cmacs--resolve-hook-forms (hooks)
    (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
             for hook in (cmacs-enlist (doom-unquote hooks))
             if (eq (car-safe hook) 'quote)
             collect (cadr hook)
             else if quoted-p
             collect hook
             else collect (intern (format "%s-hook" (symbol-name hook)))))

  (defvar cmacs--transient-counter 0)
  (defmacro add-transient-hook! (hook &rest forms)
    "Attaches transient forms to a HOOK.

   HOOK can be a quoted hook or a sharp-quoted function (which will be advised).

   These forms will be evaluated once when that function/hook is first invoked,
   then it detaches itself."
    (declare (indent 1))
    (let ((append (eq (car forms) :after))
          (fn (intern (format "cmacs-transient-hook-%s" (cl-incf cmacs--transient-counter)))))
      `(when ,hook
         (fset ',fn
               (lambda (&rest _)
                 ,@forms
                 (cond ((functionp ,hook) (advice-remove ,hook #',fn))
                       ((symbolp ,hook)   (remove-hook ,hook #',fn)))
                 (unintern ',fn nil)))
         (cond ((functionp ,hook)
                (advice-add ,hook ,(if append :after :before) #',fn))
               ((symbolp ,hook)
                (add-hook ,hook #',fn ,append)))))))

(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:

   1. Optional properties :local and/or :append, which will make the hook
      buffer-local or append to the list of hooks (respectively),
   2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
      a quoted hook variable or a quoted list of hook variables. If unquoted, the
      hooks will be resolved by appending -hook to each symbol.
   3. A function, list of functions, or body forms to be wrapped in a lambda.

 Examples:
     (add-hook! 'some-mode-hook 'enable-something)
     (add-hook! some-mode '(enable-something and-another))
     (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
     (add-hook! (one-mode second-mode) 'enable-something)
     (add-hook! :append (one-mode second-mode) 'enable-something)
     (add-hook! :local (one-mode second-mode) 'enable-something)
     (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
     (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))

 Body forms can access the hook's arguments through the let-bound variable
 `args'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (cmacs--resolve-hook-forms (pop args)))
          (funcs
           (let ((val (car args)))
             (if (memq (car-safe val) '(quote function))
                 (if (cdr-safe (cadr val))
                     (cadr val)
                   (list (cadr val)))
               (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(function ,fn)
                   `(lambda (&rest _) ,@args)))
        (dolist (hook hooks)
          (push (cond ((eq hook-fn 'remove-hook)
                       `(remove-hook ',hook ,fn ,local-p))
                      (t
                       `(add-hook ',hook ,fn ,append-p ,local-p)))
                forms)))
      `(progn ,@(nreverse forms)))))

(defmacro remove-hook! (&rest args)
  "Convenience macro for `remove-hook'. Takes the same arguments as
 `add-hook!'."
  `(add-hook! :remove ,@args))

;;;; Quiet!
;; A simple macro that prevents code from making any noise

(defmacro quiet! (&rest forms)
  "Run FORMS without making any noise."
  `(if nil
       (progn ,@forms)
     (fset 'doom--old-write-region-fn (symbol-function 'write-region))
     (cl-letf ((standard-output (lambda (&rest _)))
               ((symbol-function 'load-file) (lambda (file) (load file nil t)))
               ((symbol-function 'message) (lambda (&rest _)))
               ((symbol-function 'write-region)
                (lambda (start end filename &optional append visit lockname mustbenew)
                  (unless visit (setq visit 'no-message))
                  (doom--old-write-region-fn
                   start end filename append visit lockname mustbenew)))
               (inhibit-message t)
               (save-silently t))
       ,@forms)))

;;;; Def-memoized!
;;  Creates a memoized function

(defvar doom-memoized-table (make-hash-table :test 'equal :size 10)
  "A lookup table containing memoized functions. The keys are argument lists,
 and the value is the function's return value.")

(defun doom-memoize (name)
  "Memoizes an existing function. NAME is a symbol."
  (let ((func (symbol-function name)))
    (put name 'function-documentation
         (concat (documentation func) " (memoized)"))
    (fset name
          `(lambda (&rest args)
             (let ((key (cons ',name args)))
               (or (gethash key doom-memoized-table)
                   (puthash key (apply ',func args)
                            doom-memoized-table)))))))

(defmacro def-memoized! (name arglist &rest body)
  "Create a memoize'd function. NAME, ARGLIST, DOCSTRING and BODY
 have the same meaning as in `defun'."
  (declare (indent defun) (doc-string 3))
  `(,(if (bound-and-true-p byte-compile-current-file)
         'with-no-warnings
       'progn)
    (defun ,name ,arglist ,@body)
    (doom-memoize ',name)))


;;;; Lambda!
(defmacro λ! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

;;;; Other Macros
(defmacro find-file-in! (path &optional project-p)
  "Returns an interactive function for searching files"
  `(lambda () (interactive)
     (let ((default-directory ,path))
       (call-interactively
        ',(if project-p
              (command-remapping 'projectile-find-file)
            (command-remapping 'find-file))))))

;;;; Quit Function
(defun doom-quit-p (&optional prompt)
  "Return t if this session should be killed. Prompts the user for
 confirmation."
  (or (yes-or-no-p (format "››› %s" (or prompt "Quit Emacs?")))
      (ignore (message "Aborted"))))
(setq confirm-kill-emacs nil)
(add-hook 'kill-emacs-query-functions #'doom-quit-p)
(defvar +doom-quit-messages
  '(;; from Doom 1
    "Let's beat it -- This is turning into a bloodbath!"
    "I wouldn't leave if I were you. DOS is much worse."
    "Ya know, next time you come in here I'm gonna toast ya."
    "Go ahead and leave. See if I care."
    "Are you sure you want to quit this great editor?"
    ;; Custom
    "Emacs! Emacs!! Emacs!!!"
    "The King is dead, long live the King!"
    "Like you have somewhere better to be..."
    "Don't worry, I won't tell everyone you're a failure"
    "Aus so krummem Holze, als woraus der Mensch gemacht ist, kann nichts ganz Gerades gezimmert werden"
    "(setq nothing t everything 'permitted)"
    "Emacs will remember that."
    "Emacs, Emacs never changes."
    "Hey! Hey, M-x listen!"
    "Okay, look. We've both said a lot of things you're going to regret..."
    "You are *not* prepared!")
  "A list of quit messages, picked randomly by `+doom-quit'. Taken from
 http://doom.wikia.com/wiki/Quit_messages and elsewhere.")

(defun +doom|quit (&rest _)
  (doom-quit-p
   (format "%s  Quit?"
           (nth (random (length +doom-quit-messages))
                +doom-quit-messages))))

(remove-hook 'kill-emacs-query-functions #'doom-quit-p)
(add-hook 'kill-emacs-query-functions #'+doom|quit)

;;; End Funtions-Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-functions-macros)
