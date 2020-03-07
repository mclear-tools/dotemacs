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

;;;; Org Roam
(eval-when-compile
  (quelpa
   '(org-roam :fetcher github :repo "jethrokuan/org-roam")))

(use-package org-roam
  :ensure nil
  :after org
  :hook
  ((org-mode . org-roam-mode)
   (after-init . org-roam--build-cache-async) ;; optional!
   )
  :custom
  (org-roam-directory "~/Dropbox/Notes")
  :general
  (:states '(normal motion) 
   (cpm/leader-keys 
     "R l"  #'org-roam
     "R t"  #'org-roam-today
     "R f"  #'org-roam-find-file
     "R i"  #'org-roam-insert
     "R g"  #'org-roam-show-graph)))

;;;; Org Expand Region
;; see https://www.reddit.com/r/emacs/comments/f9e6kw/expandregion_for_org_mode_with_org_element_api/
;; Expansion functions for Org mode based on Org element API.
(defun gb/er/mark-org-element (&optional parent)
  "Mark the smallest Org element or object around point.
Uses the Org Element API to identify those elements or objects.
With argument PARENT, mark the parent element instead."
  (interactive)
  (let* ((el-at-point (org-element-context))
         (up-el-at-point (org-element-property :parent el-at-point))
         (el (if parent
                 (cond
                  ((not up-el-at-point)
                   (save-excursion
                     (ignore-errors (org-up-element))
                     ;; Given el-at-point has no parent at this point,
                     ;; `org-up-element' will bring point to a heading
                     ;; (back-to-heading, if not on a heading, and
                     ;; up-heading, if on one), unless it is before the
                     ;; first one.
                     ;; Note: 'org-element-at-point' and
                     ;; 'org-element-context' won't normally get a
                     ;; headline's parent (which will return nil), we'd need
                     ;; 'org-element-parse-buffer' for that.  But we don't
                     ;; want to parse the whole buffer for an expansion
                     ;; either.
                     (when (org-with-limited-levels (org-at-heading-p))
                       (org-element-at-point))))
                  ((and
                    (memq (org-element-type el-at-point)
                          org-element-all-objects)
                    (eq (org-element-type up-el-at-point) 'paragraph)
                    (memq (org-element-type
                           (org-element-property :parent up-el-at-point))
                          '(item quote-block center-block drawer)))
                   ;; Corner case, when an 'object' is also the first thing
                   ;; on a plain list item.  In this case, if we simply get
                   ;; the parent, it will be paragraph, and further
                   ;; expansion will lose the list structure from there.
                   ;; Same thing happens on quote-blocks.  So, if element at
                   ;; point is an object, its parent is a paragraph, and its
                   ;; grandparent is one of those types, we pass the
                   ;; grandparent, to follow the structure properly.
                   ;; Probably, other cases will emerge with use, which can
                   ;; just be added here.  Unfortunately, we cannot simply
                   ;; pass the granparent for all cases: e.g. if the parent
                   ;; is a headline, there is no grandparent.
                   (org-element-property :parent up-el-at-point))
                  (t
                   up-el-at-point))
               el-at-point))
         (type (org-element-type el))
         beg end)
    (when el
      (cond
       ((memq type org-element-all-objects)
        (setq beg (org-element-property :begin el))
        (setq end (- (org-element-property :end el)
                     (org-element-property :post-blank el))))
       ((memq type '(src-block center-block comment-block
                               example-block export-block quote-block
                               special-block verse-block
                               latex-environment
                               drawer property-drawer))
        (setq beg (org-element-property :begin el))
        (setq end (save-excursion
                    (goto-char (org-element-property :end el))
                    (forward-line
                     (- (org-element-property :post-blank el)))
                    (point))))
       (t
        (setq beg (org-element-property :begin el))
        (setq end (org-element-property :end el)))))
    (when (and beg end)
      (goto-char end)
      (set-mark (point))
      (goto-char beg))))

(defun gb/er/mark-org-element-parent ()
  "Mark the parent of the Org element or object around point."
  (interactive)
  (gb/er/mark-org-element t))

(defun gb/er/mark-org-element-inside ()
  "Mark contents of the smallest Org element or object around point."
  (interactive)
  (let* ((el (org-element-context))
         (type (org-element-type el))
         beg end)
    ;; Here we handle just special cases, remaining ones will fall back to
    ;; 'gb/er/mark-org-element'. So, there is no need for a residual
    ;; condition.
    (cond
     ((memq type '(bold italic strike-through underline
                        quote-block special-block verse-block
                        drawer property-drawer
                        footnote-definition footnote-reference))
      (setq beg (org-element-property :contents-begin el))
      (setq end (org-element-property :contents-end el)))
     ((memq type '(code verbatim))
      (setq beg (save-excursion
                  (goto-char (org-element-property :begin el))
                  (unless (bolp) (backward-char 1))
                  (when (looking-at org-verbatim-re)
                    (goto-char (match-beginning 4))
                    (point))))
      (setq end (save-excursion
                  (goto-char (org-element-property :begin el))
                  (unless (bolp) (backward-char 1))
                  (when (looking-at org-verbatim-re)
                    (goto-char (match-end 4))
                    (point)))))
     ((memq type '(src-block center-block comment-block
                             example-block export-block
                             latex-environment))
      (setq beg (save-excursion
                  (goto-char (org-element-property :post-affiliated el))
                  (forward-line)
                  (point)))
      (setq end (save-excursion
                  (goto-char (org-element-property :end el))
                  (forward-line
                   (1- (- (org-element-property :post-blank el))))
                  (point))))
     ;; Unsure whether this is a good handling for headlines.
     ;; ((eq type 'headline)
     ;;  (save-excursion
     ;;    ;; Following the steps of 'org-element-headline-parser' to get the
     ;;    ;; start and end position of the title.
     ;;    (goto-char (org-element-property :begin el))
     ;;    (skip-chars-forward "*")
     ;;    (skip-chars-forward " \t")
     ;;    (and org-todo-regexp
     ;;         (let (case-fold-search) (looking-at (concat org-todo-regexp " ")))
     ;;         (goto-char (match-end 0))
     ;;         (skip-chars-forward " \t"))
     ;;    (when (looking-at "\\[#.\\][ \t]*")
     ;;      (goto-char (match-end 0)))
     ;;    (when (let (case-fold-search) (looking-at org-comment-string))
     ;;      (goto-char (match-end 0)))
     ;;    (setq beg (point))
     ;;    (when (re-search-forward
     ;;           "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"
     ;;           (line-end-position)
     ;;           'move)
     ;;      (goto-char (match-beginning 0)))
     ;;    (setq end (point))))
     )
    (when (and beg end)
      (goto-char end)
      (set-mark (point))
      (goto-char beg))))

;; The default sentence expansion is quite frequently fooled by a regular
;; Org document (plain lists, code blocks, in particular), so we use Org's
;; sentence commands and restrict the mark to a single paragraph.
(defun gb/er/mark-org-sentence ()
  "Marks one sentence."
  (interactive)
  (let ((par-el (org-element-lineage
                 (org-element-context) '(paragraph) t))
        (beg-quote (when (use-region-p)
                     (save-excursion
                       (goto-char (region-beginning))
                       (looking-back "[\"“]" (1- (point))))))
        (end-quote (when (use-region-p)
                     (save-excursion
                       (goto-char (region-end))
                       (looking-at "[\"”]")))))
    (when (and
           ;; Do not mark sentences when not in a paragraph.
           par-el
           ;; Also do not mark a sentence when current region is
           ;; equivalent to an 'inside-quotes' expansion, let
           ;; 'outside-quotes' expand first.
           (not (and beg-quote end-quote)))
      (save-restriction
        ;; Never mark beyond the limits of the current paragraph.
        (narrow-to-region (org-element-property :contents-begin par-el)
                          (org-element-property :contents-end par-el))
        (forward-char 1)
        (org-backward-sentence 1)
        ;; Sentences which start or end with quotes will not be expanded
        ;; into by the heuristics of 'expand-region', as the 'inside-quotes'
        ;; expansion will prevail.  Thus, we expand the sentence up to the
        ;; quote only, to be able to expand one sentence when multiple
        ;; sentences are between quotes.  This rule of thumb will not always
        ;; be ideal: e.g. when the sentence is a sequence of multiple quoted
        ;; strings.
        (when (looking-at "[\"“]")
          (goto-char (match-end 0)))
        (set-mark (point))
        (org-forward-sentence 1)
        ;; Ditto.
        (when (looking-back "[\"”]" (1- (point)))
          (goto-char (match-beginning 0)))
        (exchange-point-and-mark)))))

;; Alternate version: simpler, but no control for quotes.
;; (defun gb/er/mark-org-sentence ()
;;   "Marks one sentence."
;;   (interactive)
;;   (let ((par-el (org-element-lineage
;;                  (org-element-context) '(paragraph) t)))
;;     ;; Do not mark sentences when not in a paragraph.
;;     (when par-el
;;       (save-restriction
;;         ;; Never mark beyond the limits of the current paragraph.
;;         (narrow-to-region (org-element-property :contents-begin par-el)
;;                           (org-element-property :contents-end par-el))
;;         (forward-char 1)
;;         (org-backward-sentence 1)
;;         (set-mark (point))
;;         (org-forward-sentence 1)
;;         (exchange-point-and-mark)))))

;; Mark curved quotes in Org mode.
(defun gb/er/mark-org-inside-curved-quotes (&optional outside)
  "Mark the inside of the current curved quotes string, not
including the quotation marks."
  (interactive)
  (let ((par-el (org-element-lineage
                 (org-element-context) '(paragraph) t)))
    (when par-el
      (save-restriction
        (narrow-to-region (org-element-property :contents-begin par-el)
                          (org-element-property :contents-end par-el))
        (let* ((beg-quote (save-excursion
                            (when (search-backward "“" nil t)
                              (if outside
                                  (match-beginning 0)
                                (match-end 0)))))
               (end-quote (save-excursion
                            (when (search-forward "”" nil t)
                              (if outside
                                  (match-end 0)
                                (match-beginning 0))))))
          (when (and beg-quote end-quote)
            (goto-char end-quote)
            (set-mark (point))
            (goto-char beg-quote)))))))

(defun gb/er/mark-org-outside-curved-quotes ()
  "Mark the current curved-quotes string, including the quotation marks."
  (interactive)
  (gb/er/mark-org-inside-curved-quotes t))

;; Control pair marking in Org: let some between-pairs objects be marked as
;; Org elements.
(defun gb/er/mark-org-inside-pairs ()
  "Mark inside pairs (as defined by the mode), not including the pairs.
Don't mark when at certain Org objects."
  (interactive)
  (unless (memq (org-element-type (org-element-context))
                '(link footnote-definition macro
                       target radio-target timestamp))
    (er/mark-inside-pairs)))

(defun gb/er/mark-org-outside-pairs ()
  "Mark pairs (as defined by the mode), including the pair chars.
Don't mark when at certain Org objects."
  (interactive)
  (unless (memq (org-element-type (org-element-context))
                '(link footnote-definition macro
                       target radio-target timestamp))
    (er/mark-outside-pairs)))

;; The default expansion for symbol includes Org's emphasis markers which
;; are contiguous to symbols (they do indeed belong to the syntax class).
;; Thus, the default expansion to symbol "leaks" beyond
;; 'inside-emphasis-markers'.  To avoid this, we restrict symbol expansion
;; to the contents of Org emphasis objects.
(defun gb/er/mark-org-symbol ()
  "Mark the entire symbol around or in front of point."
  (interactive)
  (let* ((symbol-regexp "\\s_\\|\\sw")
         (el (org-element-context))
         (type (org-element-type el))
         beg-emph end-emph)
    (cond ((memq type '(bold italic underline strike-through))
           (setq beg-emph (org-element-property :contents-begin el))
           (setq end-emph (org-element-property :contents-end el)))
          ((memq type '(code verbatim))
           (setq beg-emph (save-excursion
                            (goto-char (org-element-property :begin el))
                            (unless (bolp) (backward-char 1))
                            (when (looking-at org-verbatim-re)
                              (goto-char (match-beginning 4))
                              (point))))
           (setq end-emph (save-excursion
                            (goto-char (org-element-property :begin el))
                            (unless (bolp) (backward-char 1))
                            (when (looking-at org-verbatim-re)
                              (goto-char (match-end 4))
                              (point))))))
    (save-restriction
      (when (and beg-emph end-emph)
        (narrow-to-region beg-emph end-emph))
      (when (or (looking-at symbol-regexp)
                (er/looking-back-on-line symbol-regexp))
        (skip-syntax-forward "_w")
        (set-mark (point))
        (skip-syntax-backward "_w")))))

;; expand-region configuration for Org mode
(defun gb/er/config-org-mode-expansions ()
  (when (< emacs-major-version 27)
    (require 'seq))
  (setq-local er/try-expand-list
              (append
               ;; Removing some expansions from the list
               (seq-remove
                (lambda (x)
                  (memq x '(;; The expansions based on the Org element API
                            ;; cover most of the default expansions, others
                            ;; don't seem that useful (to me) and may
                            ;; introduce some noise in the expansion
                            ;; sequence.
                            org-mark-subtree
                            er/mark-org-element
                            er/mark-org-element-parent
                            er/mark-org-code-block
                            er/mark-org-parent
                            er/mark-comment
                            er/mark-url
                            er/mark-email
                            mark-page

                            ;; The basic symbol and method-call expansion
                            ;; consider Org emphasis markers as part of the
                            ;; unit,  so I created a dedicated function for
                            ;; symbol, disabled the others.
                            er/mark-symbol
                            er/mark-symbol-with-prefix
                            er/mark-next-accessor
                            er/mark-method-call

                            ;; er/mark-paragraph actually confuses
                            ;; expand-region on plain lists, and paragraphs
                            ;; actually do work with the other expansions on
                            ;; the list (as an org-element).  For the same
                            ;; reason, remove er/mark-text-paragraph.
                            er/mark-paragraph
                            er/mark-text-paragraph

                            ;; Remove er/mark-sentence, better to work with
                            ;; Org sentence commands, which are in
                            ;; gb/er/mark-org-sentence.  For the same reason
                            ;; remove er/mark-text-sentence.
                            er/mark-sentence
                            er/mark-text-sentence

                            ;; Tweak pair expansion for Org.
                            er/mark-inside-pairs
                            er/mark-outside-pairs)))
                er/try-expand-list)
               '(gb/er/mark-org-symbol
                 gb/er/mark-org-element
                 gb/er/mark-org-element-parent
                 gb/er/mark-org-element-inside
                 gb/er/mark-org-sentence
                 gb/er/mark-org-inside-curved-quotes
                 gb/er/mark-org-outside-curved-quotes
                 gb/er/mark-org-inside-pairs
                 gb/er/mark-org-outside-pairs))))




;;; End Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-testing)
