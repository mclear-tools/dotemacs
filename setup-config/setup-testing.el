;;; Testing

;;;; Helpful Demos
(use-package elisp-demos
  :defer 1
  :config
  ;; inject demos into helpful
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;;; Pretty Ligatures
;; https://www.reddit.com/r/emacs/comments/icem4s/emacs_271_freezes_when_using_font_ligatures/g23795b?utm_source=share&utm_medium=web2x&context=3
;; (when (window-system)
;;   (set-frame-font cpm-font5))
;; (let ((alist '(
;;                (33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)") ;; Broken
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)") ;; Broken
;;                (48 . ".\\(?:x[a-zA-Z]\\)") ;; Broken?
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;                )
;;              ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))



;; (when cpm-ligatures
;;   (defun my-correct-symbol-bounds (pretty-alist)
;;     "Prepend a TAB character to each symbol in this alist,
;; this way compose-region called by prettify-symbols-mode
;; will use the correct width of the symbols
;; instead of the width measured by char-width."
;;     (mapcar (lambda (el)
;;               (setcdr el (string ?\t (cdr el)))
;;               el)
;;             pretty-alist))

;;   (defun my-ligature-list (ligatures codepoint-start)
;;     "Create an alist of strings to replace with
;; codepoints starting from codepoint-start."
;;     (require 'dash)
;;     (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
;;       (-zip-pair ligatures codepoints)))

;;                                         ; list can be found at https://github.com/i-tu/Hasklig/blob/master/GlyphOrderAndAliasDB#L1588
;;   (setq my-hasklig-ligatures
;;         (let* ((ligs '("&&" "***" "*>" "\\\\" "||" "|>" "::"
;;                        "==" "===" "==>" "=>" "=<<" "!!" ">>"
;;                        ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
;;                        "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
;;                        "<<" "<<<" "<+>" ".." "..." "++" "+++"
;;                        "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")))
;;           (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

;; nice glyphs for haskell with hasklig
;; (defun my-set-hasklig-ligatures ()
;;   "Add hasklig ligatures for use with prettify-symbols-mode."
;;   (setq prettify-symbols-alist
;;         (append my-hasklig-ligatures prettify-symbols-alist))
;;   (prettify-symbols-mode))

;; (add-hook 'text-mode-hook 'my-set-hasklig-ligatures))

;;;; Bug Hunter
(use-package bug-hunter
  :defer 2)


;;;; Named Servers
;; from https://www.reddit.com/r/emacs/comments/9ih3fo/frameoriented_workflow/e6lkfpa?utm_source=share&utm_medium=web2x
;; set different server names for multiple processes
;; (setq server-name "org")
;; (setq server-name "dashboard")


;;;; Did You Mean
;; https://gitlab.com/kisaragi-hiu/didyoumean.el/
;; (eval-when-compile
;;   (quelpa '(didyoumean :fetcher gitlab :repo "kisaragi-hiu/didyoumean.el")))
(use-package didyoumean
  :defer 2)

;;;; Compile with Nearest Makefile
;; See https://www.emacswiki.org/emacs/CompileCommand
(defun cpm/upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
  manage to find it, return the containing directory. Else if we
  get to the toplevel directory and still can't find it, return
  nil. Start at startdir or . if startdir not given"

  (let ((dirname (expand-file-name
                  (if startdir startdir ".")))
        (found nil) ; found is set as a flag to leave loop if we find it
        (top nil))  ; top is set when we get
                                        ; to / so that we only check it once

                                        ; While we've neither been at the top last time nor have we found
                                        ; the file.
    (while (not (or found top))
                                        ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
          (setq top t))

                                        ; Check for the file
      (if (file-exists-p (expand-file-name filename dirname))
          (setq found t)
                                        ; If not, move up a directory
        (setq dirname (expand-file-name ".." dirname))))
                                        ; return statement
    (if found dirname nil)))

(defun cpm/compile-next-makefile ()
  (interactive)
  (let* ((default-directory (or (cpm/upward-find-file "Makefile") "."))
         (compile-command (concat "cd " default-directory " && "
                                  compile-command)))
    (compile compile-command)))

;;;; Writegood Mode
(use-package writegood-mode
  :hook (markdown-mode . writegood-mode)
  :config
  (setq cpm/weasel-words
        '("actually"
          "basically"
          "easily"
          "easy"
          "specifically"
          "simple"
          "simply"))
  (setq writegood-weasel-words
        (-concat writegood-weasel-words cpm/weasel-words)))

;;;; Pretty Info
(use-package info+
  :straight t
  :defer 1
  :config
  (eval-after-load "info" '(require 'info+)))


;;;; Make Temp Buffer
(defun cpm/tmp-buffer()
  "Make a temporary buffer and switch to it"
  (interactive)
  (switch-to-buffer (get-buffer-create (concat "tmp-" (format-time-string "%m.%dT%H.%M.%S"))))
  (delete-other-windows))

;;;; Org export to doc

;; This setup is tested on Emacs 24.3 & Emacs 24.4 on Linux/OSX
;; org v7 bundled with Emacs 24.3
(setq org-export-odt-preferred-output-format "doc")
;; org v8 bundled with Emacs 24.4
(setq org-odt-preferred-output-format "doc")
;; BTW, you can assign "pdf" in above variables if you prefer PDF format

;; Only OSX need below setup
(defun my-setup-odt-org-convert-process ()
  (interactive)
  (let ((cmd "/Applications/LibreOffice.app/Contents/MacOS/soffice"))
    (when (and (eq system-type 'darwin) (file-exists-p cmd))
      ;; org v7
      (setq org-export-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")))
      ;; org v8
      (setq org-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i"))))
    ))
(my-setup-odt-org-convert-process)


;;;; Clone a Repo
;; http://xenodium.com/emacs-clone-git-repo-from-clipboard/
;; -*- lexical-binding: t -*-

(defun ar/git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name "~/Downloads/"))
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

;;;; Remove Org Links
;; https://emacs.stackexchange.com/a/10714/11934
(defun cpm/org-replace-link-by-link-description ()
  "Replace an org link by its description or, if empty, its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))


;;;; Engrave Faces
(use-package engrave-faces
  :disabled
  :straight (:type git :host github :repo "tecosaur/engrave-faces")
  :after org
  :demand t)


;;; End Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-testing)
