;;; Testing
;;;; Helpful Demos
  (use-package elisp-demos
    :ensure t
    :defer 1
    :config
    ;; inject demos into helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;;; Pretty Ligatures
(when cpm-ligatures
  (defun my-correct-symbol-bounds (pretty-alist)
    "Prepend a TAB character to each symbol in this alist,
this way compose-region called by prettify-symbols-mode
will use the correct width of the symbols
instead of the width measured by char-width."
    (mapcar (lambda (el)
              (setcdr el (string ?\t (cdr el)))
              el)
            pretty-alist))

  (defun my-ligature-list (ligatures codepoint-start)
    "Create an alist of strings to replace with
codepoints starting from codepoint-start."
    (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
      (-zip-pair ligatures codepoints)))

  ; list can be found at https://github.com/i-tu/Hasklig/blob/master/GlyphOrderAndAliasDB#L1588
  (setq my-hasklig-ligatures
    (let* ((ligs '("&&" "***" "*>" "\\\\" "||" "|>" "::"
                   "==" "===" "==>" "=>" "=<<" "!!" ">>"
                   ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
                   "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
                   "<<" "<<<" "<+>" ".." "..." "++" "+++"
                   "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")))
      (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

  ;; nice glyphs for haskell with hasklig
  (defun my-set-hasklig-ligatures ()
    "Add hasklig ligatures for use with prettify-symbols-mode."
    (setq prettify-symbols-alist
          (append my-hasklig-ligatures prettify-symbols-alist))
    (prettify-symbols-mode))

 (add-hook 'text-mode-hook 'my-set-hasklig-ligatures))

;;;; Bug Hunter
(use-package bug-hunter
  :ensure t
  :defer 2)


;;;; Named Servers
;; from https://www.reddit.com/r/emacs/comments/9ih3fo/frameoriented_workflow/e6lkfpa?utm_source=share&utm_medium=web2x
;; set different server names for multiple processes
;; (setq server-name "org")
;; (setq server-name "dashboard")

;;;; Hercules
;; https://gitlab.com/jjzmajic/hercules.el.git
(eval-when-compile
  (quelpa
   '(hercules :fetcher gitlab :repo "jjzmajic/hercules.el")))
(use-package hercules
  :ensure t)

;;;; Did You Mean
;; https://gitlab.com/kisaragi-hiu/didyoumean.el/
(eval-when-compile
  (quelpa '(didyoumean :fetcher gitlab :repo "kisaragi-hiu/didyoumean.el")))
(use-package didyoumean
  :ensure nil
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
  :ensure t
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

;;;; Add Colors to Info Mode
(eval-when-compile
  (quelpa
   '(info-colors :fetcher github :repo "ubolonton/info-colors")))
(use-package info-colors
  :ensure nil
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;;;; Make Temp Buffer
(defun cpm/tmp-buffer()
  "Make a temporary buffer and switch to it"
  (interactive)
  (switch-to-buffer (get-buffer-create (concat "tmp-" (format-time-string "%m.%dT%H.%M.%S"))))
  (delete-other-windows))

;;;; Batch Export Files with Org-Hugo
(defun cpm/ox-hugo-for-batch-export-files ()
  "use this emacs in batch mode to export files from org to md"
  (interactive)
  (require 'org)
  (require 'ox-hugo)
  (org-hugo-export-wim-to-md))

;;; End Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-testing)
