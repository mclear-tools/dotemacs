;;; PDF Management
    (use-package pdf-tools
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
