;;; interleave-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "interleave" "interleave.el" (0 0 0 0))
;;; Generated autoloads from interleave.el

(define-obsolete-variable-alias 'interleave--pdf-current-page-fn 'interleave-pdf-current-page-fn "1.3.0")

(define-obsolete-variable-alias 'interleave--pdf-next-page-fn 'interleave-pdf-next-page-fn "1.3.0")

(define-obsolete-variable-alias 'interleave--pdf-previous-page-fn 'interleave-pdf-previous-page-fn "1.3.0")

(define-obsolete-variable-alias 'interleave--pdf-goto-page-fn 'interleave-pdf-goto-page-fn "1.3.0")

(define-obsolete-variable-alias 'interleave--pdf-scroll-up-or-next-page-fn 'interleave-pdf-scroll-up-or-next-page-fn "1.3.0")

(define-obsolete-variable-alias 'interleave--pdf-scroll-down-or-previous-page-fn 'interleave-pdf-scroll-down-or-previous-page-fn "1.3.0")

(define-obsolete-function-alias 'interleave--open-notes-file-for-pdf 'interleave-open-notes-file-for-pdf "1.3.0")

(autoload 'interleave-open-notes-file-for-pdf "interleave" "\
Open the notes org file for the current pdf file if it exists.
Else create it.

It is assumed that the notes org file will have the exact same base name
as the pdf file (just that the notes file will have a .org extension instead
of .pdf).

\(fn)" t nil)

(define-obsolete-variable-alias 'interleave 'interleave-mode "1.3.0")

(define-obsolete-variable-alias 'interleave-hook 'interleave-mode-hook "1.3.0")

(define-obsolete-function-alias 'interleave 'interleave-mode "1.3.0")

(autoload 'interleave-mode "interleave" "\
Interleaving your text books since 2015.

In the past, textbooks were sometimes published as 'interleaved' editions.
That meant, each page was followed by a blank page and the ambitious student/
scholar had the ability to take their notes directly in their copy of the
textbook. Newton and Kant were prominent representatives of this technique.

Nowadays textbooks (or lecture material) come in PDF format. Although almost
every PDF Reader has the ability to add some notes to the PDF itself, it is
not as powerful as it could be.

This is what this minor mode tries to accomplish. It presents your PDF side by
side to an [[http://orgmode.org][Org Mode]] buffer with your notes, narrowing
down to just those passages that are relevant to the particular page in the
document viewer.

Usage:

- Create a Org file that will keep your notes. In the Org headers section, add
#+INTERLEAVE_PDF: /the/path/to/your/pdf.pdf
- Start `interleave-mode' with `M-x interleave-mode'.
- To insert a note for a page, type `i'.
- Navigation is the same as in `doc-view-mode'/`pdf-view-mode'.

The split direction is determined by the customizable variable
`interleave-split-direction'. When `interleave-mode' is invoked
with a prefix argument the inverse split direction is used
e.g. if `interleave-split-direction' is 'vertical the buffer is
split horizontally.

Keybindings (`doc-view-mode'/`pdf-view-mode'):

\\{interleave-pdf-mode-map}

Keybindings (org-mode buffer):

\\{interleave-map}

\(fn &optional ARG)" t nil)

(autoload 'interleave-pdf-mode "interleave" "\
Interleave view for the pdf.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "interleave" '("interleave-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; interleave-autoloads.el ends here
