;;; PDF Management
;;;; Doc-View Mode
(use-package doc-view
  :config
  (fset 'doc-prev "\C-xo\C-x[\C-xo")
  (fset 'doc-next "\C-xo\C-x]\C-xo")
  (global-set-key (kbd "M-[") 'doc-prev)
  (global-set-key (kbd "M-]") 'doc-next)
  (evil-set-initial-state 'doc-view-mode 'normal)
  (evil-define-key 'normal doc-view-mode-map
    "/"  'spacemacs/doc-view-search-new-query
    "?"  'spacemacs/doc-view-search-new-query-backward
    "gg" 'doc-view-first-page
    "f"  'doc-view-autofit-mode
    "G"  'doc-view-last-page
    "gt" 'doc-view-goto-page
    "h"  'doc-view-previous-page
    "j"  'doc-view-next-line-or-next-page
    "k"  'doc-view-previous-line-or-previous-page
    "K"  'doc-view-kill-proc-and-buffer
    "l"  'doc-view-next-page
    "n"  'doc-view-search
    "N"  'doc-view-search-backward
    "-"  'doc-view-shrink
    "+"  'doc-view-enlarge
    (kbd "C-d") 'doc-view-scroll-up-or-next-page
    (kbd "C-k") 'doc-view-kill-proc
    (kbd "C-u") 'doc-view-scroll-down-or-previous-page)
  (progn
    (defun spacemacs/doc-view-search-new-query ()
      "Initiate a new query."
      (interactive)
      (doc-view-search 'newquery))

    (defun spacemacs/doc-view-search-new-query-backward ()
      "Initiate a new query."
      (interactive)
      (doc-view-search 'newquery t))

    (defcustom doc-view-autofit-timer-start 1.0
      "Initial value (seconds) for the timer that delays the fitting when
  `doc-view-autofit-fit' is called (Which is when a window
  configuration change occurs and a document needs to be fitted)."
      :type 'number
      :group 'doc-view)

    (defcustom doc-view-autofit-timer-inc 0.02
      "Value to increase (seconds) the timer (see `doc-view-autofit-timer-start')
  by, if there is another window configuration change occuring, before
  it runs out."
      :type 'number
      :group 'doc-view)

    (defcustom doc-view-autofit-default-fit 'width
      "The fitting type initially used when mode is enabled.
  Valid values are: width, height, page."
      :type 'symbol
      :group 'doc-view)

    (defvar doc-view-autofit-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c W") 'doc-view-autofit-width)
        (define-key map (kbd "C-c H") 'doc-view-autofit-height)
        (define-key map (kbd "C-c P") 'doc-view-autofit-page)
        map)
      "Keymap used by `doc-view-autofit-mode'.")

    (defun doc-view-autofit-set (type)
      "Set autofitting to TYPE for current buffer."
      (when doc-view-autofit-mode
        (setq doc-view-autofit-type type)
        (doc-view-autofit-fit)))

    (defun doc-view-autofit-width ()
      "Set autofitting to width for current buffer."
      (interactive) (doc-view-autofit-set 'width))

    (defun doc-view-autofit-height ()
      "Set autofitting to height for current buffer."
      (interactive) (doc-view-autofit-set 'height))

    (defun doc-view-autofit-page ()
      "Set autofitting to page for current buffer."
      (interactive) (doc-view-autofit-set 'page))

    (defun doc-view-autofit-fit ()
      "Fits the document in the selected window's buffer
  delayed with a timer, so multiple calls in succession
  don't cause as much overhead."
      (lexical-let
          ((window (selected-window)))
        (if (equal doc-view-autofit-timer nil)
            (setq doc-view-autofit-timer
                  (run-with-timer
                   doc-view-autofit-timer-start nil
                   (lambda ()
                     (if (window-live-p window)
                         (save-selected-window
                           (select-window window)
                           (cancel-timer doc-view-autofit-timer)
                           (setq doc-view-autofit-timer nil)
                           (cond
                            ((equal 'width doc-view-autofit-type)
                             (doc-view-fit-width-to-window))
                            ((equal 'height doc-view-autofit-type)
                             (doc-view-fit-height-to-window))
                            ((equal 'page doc-view-autofit-type)
                             (doc-view-fit-page-to-window))))))))
          (timer-inc-time doc-view-autofit-timer doc-view-autofit-timer-inc))))

    (define-minor-mode doc-view-autofit-mode
      "Minor mode for automatic (timer based) fitting in DocView."
      :lighter " AFit" :keymap doc-view-autofit-mode-map :group 'doc-view
      (when doc-view-autofit-mode
        (set (make-local-variable 'doc-view-autofit-type)
             doc-view-autofit-default-fit)
        (set (make-local-variable 'doc-view-autofit-timer) nil)
        (add-hook 'window-configuration-change-hook
                  'doc-view-autofit-fit nil t)
        (doc-view-autofit-fit))
      (when (not doc-view-autofit-mode)
        (remove-hook 'window-configuration-change-hook
                     'doc-view-autofit-fit t)
        (when doc-view-autofit-timer
          (cancel-timer doc-view-autofit-timer)
          (setq doc-view-autofit-timer nil))
        (setq doc-view-autofit-type nil)))

    (add-hook 'doc-view-mode-hook 'doc-view-autofit-mode)
    ;; reload when file changes
    (add-hook 'doc-view-mode-hook 'auto-revert-mode)
    ;; continuous scroll mode
    (setq doc-view-continuous t)))

;;;; PDF-Tools
;; good but often problematic pdf reader and annotator
(use-package pdf-tools
  :disabled t
  :mode (("\\.pdf$" . pdf-view-mode))
  :commands (pdf-view-mode)
  :config
  (progn
    (pdf-tools-install)
    (evil-set-initial-state 'pdf-view-mode 'normal)
    (evil-set-initial-state 'pdf-outline-buffer-mode 'normal)
    (general-define-key :states '(normal) :keymaps 'pdf-view-mode-map
                        ;; Navigation
                        "j"  'pdf-view-next-line-or-next-page
                        "k"  'pdf-view-previous-line-or-previous-page
                        "l"  'pdf-view-next-page
                        "h"  'pdf-view-previous-page
                        "J"  'image-forward-hscroll
                        "K"  'image-backward-hscroll
                        "gg"  'pdf-view-first-page
                        "G"  'pdf-view-last-page
                        "gt"  'pdf-view-goto-page
                        "gl"  'pdf-view-goto-label
                        "u" 'pdf-view-scroll-down-or-previous-page
                        "d" 'pdf-view-scroll-up-or-next-page
                        "-"  'pdf-view-shrink
                        "+"  'pdf-view-enlarge
                        "="  'pdf-view-fit-page-to-window
                        (kbd "C-u") 'pdf-view-scroll-down-or-previous-page
                        (kbd "C-d") 'pdf-view-scroll-up-or-next-page
                        (kbd "``")  'pdf-history-backward
                        ;; Search
                        "/" 'isearch-forward
                        "?" 'isearch-backward
                        ;; Actions
                        "r"   'pdf-view-revert-buffer
                        "o"   'pdf-links-action-perform
                        "O"   'pdf-outline
                        "!"   'bms/pdf-no-filter
                        "#"   'bms/pdf-midnight-original
                        )
    (general-define-key :states '(insert) :keymaps 'pdf-view-mode-map
                        "y" 'pdf-view-kill-ring-save )

    ;; midnite mode
    (setq pdf-view-midnight-colors '("#839496" . "#002b36" )) ; original values

    (defun bms/pdf-no-filter ()
      "View pdf without colour filter."
      (interactive)
      (pdf-view-midnight-minor-mode -1)
      )

    ;; change midnite mode colours functions
    (defun bms/pdf-midnite-original ()
      "Set pdf-view-midnight-colors to original colours."
      (interactive)
      (setq pdf-view-midnight-colors '("#839496" . "#002b36" )) ; original values
      (pdf-view-midnight-minor-mode)
      )

    (defun bms/pdf-midnite-amber ()
      "Set pdf-view-midnight-colors to amber on dark slate blue."
      (interactive)
      (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" )) ; amber
      (pdf-view-midnight-minor-mode)
      )

    (defun bms/pdf-midnite-green ()
      "Set pdf-view-midnight-colors to green on black."
      (interactive)
      (setq pdf-view-midnight-colors '("#00B800" . "#000000" )) ; green
      (pdf-view-midnight-minor-mode)
      )

    (defun bms/pdf-midnite-colour-schemes ()
      "Midnight mode colour schemes bound to keys"
      (local-set-key (kbd "!") (quote bms/pdf-no-filter))
      (local-set-key (kbd "@") (quote bms/pdf-midnite-amber))
      (local-set-key (kbd "#") (quote bms/pdf-midnite-green))
      (local-set-key (kbd "$") (quote bms/pdf-midnite-original))
      )

    (defun cpm/pdf-color-theme ()
      (if (eq active-theme 'solarized-light)
          (bms/pdf-no-filter)
        (bms/pdf-midnite-original)))

    ;; midnite mode hook
    (add-hook 'pdf-view-mode-hook (lambda ()
                                        ; automatically turns on midnight-mode for pdfs
                                    (pdf-view-midnight-minor-mode)
                                    (cpm/pdf-color-theme)
                                    (bms/pdf-midnite-colour-schemes)
                                        ; fixes blinking pdf in evil
                                    (blink-cursor-mode -1)
                                    (beacon-mode -1)))

    ))

(use-package org-pdfview
  :commands (org-pdfview-open)
  :after pdf-tools
  :init
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-pdfview-open link))))
  )
(use-package pdf-tools-org
  :ensure nil
  :commands (pdf-tools-org-export-to-org pdf-tools-org-import-from-org))

;; Extracting annotations using pdf-tools
;; modified from https://github.com/politza/pdf-tools/pull/133
;; taken from http://matt.hackinghistory.ca/2015/11/11/note-taking-with-pdf-tools/

(defun mwp/pdf-multi-extract (sources)
  "Helper function to print highlighted text from a list of pdf's, with one org header per pdf,
      and links back to page of highlight."
  (let (
        (output ""))
    (dolist (thispdf sources)
      (setq output (concat output (pdf-annot-markups-as-org-text thispdf nil level ))))
    (princ output))
  )

(defun cpm/pdf-summary-extract (sources)
  "Helper function to print underlined text from a list of pdf's, with one org header per pdf,
      and links back to page of highlight."
  (let (
        (output ""))
    (dolist (thispdf sources)
      (setq output (concat output (pdf-annot-summary-as-org-text thispdf nil level ))))
    (princ output))
  )

;; this is stolen from https://github.com/pinguim06/pdf-tools/commit/22629c746878f4e554d4e530306f3433d594a654
(defun pdf-annot-edges-to-region (edges)
  "Attempt to get 4-entry region \(LEFT TOP RIGHT BOTTOM\) from several edges.
      We need this to import annotations and to get marked-up text, because annotations
      are referenced by its edges, but functions for these tasks need region."

  (let ((left0 (nth 0 (car edges)))
        (top0 (nth 1 (car edges)))
        (bottom0 (nth 3 (car edges)))
        (top1 (nth 1 (car (last edges))))
        (right1 (nth 2 (car (last edges))))
        (bottom1 (nth 3 (car (last edges))))
        (n (safe-length edges)))
    ;; we try to guess the line height to move
    ;; the region away from the boundary and
    ;; avoid double lines
    (list left0
          (+ top0 (/ (- bottom0 top0) 2))
          right1
          (- bottom1 (/ (- bottom1 top1) 2 )))))

(defun pdf-annot-markups-as-org-text (pdfpath &optional title level)
  "Acquire highligh annotations as text, and return as org-heading"

  (interactive "fPath to PDF: ")
  (let* ((outputstring "") ;; the text to be returned
         (title (or title (replace-regexp-in-string "-" " " (file-name-base pdfpath ))))
         (level (or level (1+ (org-current-level)))) ;; I guess if we're not in an org-buffer this will fail
         (levelstring (make-string level ?*)) ;; set headline to proper level
         (annots (sort (pdf-info-getannots nil pdfpath)  ;; get and sort all annots
                       'pdf-annot-compare-annotations)))
    ;; create the header
    (setq outputstring (concat levelstring " Quotes From " title "\n\n")) ;; create heading

    ;; extract text
    (mapc
     (lambda (annot) ;; traverse all annotations
       (if (eq 'highlight (assoc-default 'type annot))
           (let* ((page (assoc-default 'page annot))
                  ;; use pdf-annot-edges-to-region to get correct boundaries of annotation
                  (real-edges (pdf-annot-edges-to-region
                               (pdf-annot-get annot 'markup-edges)))
                  (text (or (assoc-default 'subject annot) (assoc-default 'content annot)
                            (replace-regexp-in-string "\n" " " (pdf-info-gettext page real-edges nil pdfpath))))

                  (height (nth 1 real-edges)) ;; distance down the page
                  ;; use pdfview link directly to page number
                  (linktext (concat "[[pdfview:" pdfpath "::" (number-to-string page)
                                    "++" (number-to-string height) "][" title  "]]" )))
             (setq outputstring (concat outputstring text " ("
                                        linktext ", " (number-to-string page) ")\n\n"))
             ))

       (if (eq 'text (assoc-default 'type annot))
           (let* ((page (assoc-default 'page annot))
                  ;; use pdf-annot-edges-to-region to get correct boundaries of annotation
                  (real-edges (pdf-annot-edges-to-region
                               (pdf-annot-get annot 'markup-edges)))
                  (text (or (assoc-default 'subject annot) (assoc-default 'content annot)
                            (replace-regexp-in-string "\n" " " (pdf-info-gettext page real-edges nil pdfpath))))

                  (height (nth 1 real-edges)) ;; distance down the page
                  ;; use pdfview link directly to page number
                  (linktext (concat "[[pdfview:" pdfpath "::" (number-to-string page)
                                    "++" (number-to-string height) "][" title  "]]" )))
             (setq outputstring (concat outputstring text " ("
                                        linktext ", " (number-to-string page) ")\n\n"))
             ))

       (if (eq 'underline (assoc-default 'type annot))
           (let* ((page (assoc-default 'page annot))
                  ;; use pdf-annot-edges-to-region to get correct boundaries of highlight
                  (real-edges (pdf-annot-edges-to-region
                               (pdf-annot-get annot 'markup-edges)))
                  (text (or (assoc-default 'subject annot) (assoc-default 'content annot)
                            (replace-regexp-in-string "\n" " " (pdf-info-gettext page real-edges nil pdfpath))))

                  (height (nth 1 real-edges)) ;; distance down the page
                  ;; use pdfview link directly to page number
                  (linktext (concat "[[pdfview:" pdfpath "::" (number-to-string page)
                                    "++" (number-to-string height) "][" title  "]]" )))
             (setq outputstring (concat outputstring text " ("
                                        linktext ", " (number-to-string page) ")\n\n"))
             ))
       )
     annots)
    outputstring ;; return the header
    )
  )

(defun pdf-annot-summary-as-org-text (pdfpath &optional title level)
  "Acquire underlined annotations as text, and return as org-heading"

  (interactive "fPath to PDF: ")
  (let* ((outputstring "") ;; the text to be returned
         (title (or title (replace-regexp-in-string "-" " " (file-name-base pdfpath ))))
         (level (or level (1+ (org-current-level)))) ;; I guess if we're not in an org-buffer this will fail
         (levelstring (make-string level ?*)) ;; set headline to proper level
         (annots (sort (pdf-info-getannots nil pdfpath)  ;; get and sort all annots
                       'pdf-annot-compare-annotations)))
    ;; create the header
    (setq outputstring (concat levelstring " Summary from " title "\n\n")) ;; create heading

    ;; extract text
    (mapc
     (lambda (annot) ;; traverse all annotations
       (if (eq 'underline (assoc-default 'type annot))
           (let* ((page (assoc-default 'page annot))
                  ;; use pdf-annot-edges-to-region to get correct boundaries of annotation
                  (real-edges (pdf-annot-edges-to-region
                               (pdf-annot-get annot 'markup-edges)))
                  (text (or (assoc-default 'subject annot) (assoc-default 'content annot)
                            (replace-regexp-in-string "\n" " " (pdf-info-gettext page real-edges nil pdfpath))))

                  (height (nth 1 real-edges)) ;; distance down the page
                  ;; use pdfview link directly to page number
                  (linktext (concat "[[pdfview:" pdfpath "::" (number-to-string page)
                                    "++" (number-to-string height) "][" title  "]]" )))
             (setq outputstring (concat outputstring text " ("
                                        linktext ", " (number-to-string page) ")\n\n"))
             ))
       )
     annots)
    outputstring ;; return the header
    )
  )

(provide 'setup-pdf)
