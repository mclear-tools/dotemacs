;;; org-ref-ox-hugo.el -- Org-ref overrides for ox-hugo

;; Copyright(C) 2019 Jethro Kuan

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/org-ref-ox-hugo
;; Version: 0.1
;; Keywords: org-mode, org-ref, ox-hugo
;; Package-Requires: ((org-ref))

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'org-ref)

(defun org-ref-get-bibtex-entry-md (key)
  "Return a md string for the bibliography entry corresponding to KEY."
  ;; We create an anchor to the key that we can jump to, and provide a jump back
  ;; link with the md5 of the key.
  (let ((org-ref-formatted-citation-backend "md"))
    (format "<a id=\"%s\"></a>%s [↩](#%s)"
            key
            (org-ref-format-entry key)
            (md5 key))))

(defun org-ref-format-bibtex-entry (entry)
  "Return a formatted citation for the bibtex entry at point.
Formats are from `org-ref-formatted-citation-formats'. The
variable `org-ref-formatted-citation-backend' determines the set
of format strings used."
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((formats (cdr (assoc org-ref-formatted-citation-backend  org-ref-formatted-citation-formats)))
           (format-string)
           (ref))
      (if (null entry)
          "!!! No entry found !!!"
        (setq format-string (or (cdr (assoc (downcase (bibtex-completion-get-value "=type=" entry)) formats))
                                (cdr (assoc nil formats))))
        (setq ref (s-format format-string 'bibtex-completion-apa-get-value entry))
        (replace-regexp-in-string "\\([.?!]\\)\\." "\\1" ref)))))

(defmacro org-ref-make-format-function (type)
  "Macro to make a format function for a link of TYPE."
  `(defun ,(intern (format "org-ref-format-%s" type)) (keyword desc format)
     ,(format "Formatting function for %s links.\n[[%s:KEYWORD][DESC]]
FORMAT is a symbol for the export backend.
Supported backends: 'html, 'latex, 'ascii, 'org, 'md, 'pandoc" type type)
     (cond
      ((eq format 'org)
       (mapconcat
        (lambda (key)
          (format "[[#%s][%s]]" key key))
        (org-ref-split-and-strip-string keyword) ","))

      ((eq format 'ascii)
       (concat "["
               (mapconcat
                (lambda (key)
                  (format "%s" key))
                (org-ref-split-and-strip-string keyword) ",") "]"))

      ((eq format 'html)
       (mapconcat
        (lambda (key)
          (format org-ref-ref-html key key))
        (org-ref-split-and-strip-string keyword) ","))

      ((eq format 'latex)
       (if (string= (substring ,type -1) "s")
           ;; biblatex format for multicite commands, which all end in s. These
           ;; are formated as \cites{key1}{key2}...
           (concat "\\" ,type
                   (mapconcat (lambda (key) (format "{%s}" key))
                              (org-ref-split-and-strip-string keyword) ""))
         ;; bibtex format
         (concat "\\" ,type
                 (when desc (org-ref-format-citation-description desc)) "{"
                 (mapconcat
                  (lambda (key) key)
                  (org-ref-split-and-strip-string keyword) ",")
                 "}")))
      ;; simple format for odt.
      ((eq format 'odt)
       (format "[%s]" keyword))

      ((eq format 'md)
       (mapconcat (lambda (key)
                    ;; this is an html link that has an anchor to jump back to,
                    ;; and links to the entry in the bibliography. Also contains
                    ;; a tooltip.
                    (format "<sup id=\"%s\"><a href=\"#%s\" title=\"%s\">%s</a></sup>"
                            ;; this makes an anchor to return to
			                      (md5 key)
			                      key
                            ;; awful way to get a simple tooltip... I just need
                            ;; a simple formatted string, but the default has
                            ;; too much html stuff in it, and this needs to be
                            ;; cleaned of quotes and stuff,
                            (let ((org-ref-bibliography-files (org-ref-find-bibliography))
				                          (file) (entry) (bibtex-entry) (entry-type) (format)
				                          (org-ref-bibliography-entry-format
				                           '(("article" . "%a, %t, %j, v(%n), %p (%y).")
				                             ("book" . "%a, %t, %u (%y).")
				                             ("techreport" . "%a, %t, %i, %u (%y).")
				                             ("proceedings" . "%e, %t in %S, %u (%y).")
				                             ("inproceedings" . "%a, %t, %p, in %b, edited by %e, %u (%y)"))))
			                        (setq file (catch 'result
					                                 (cl-loop for file in org-ref-bibliography-files do
						                                        (if (org-ref-key-in-file-p key (file-truename file))
							                                          (throw 'result file)
						                                          (message "%s not found in %s"
							                                                 key (file-truename file))))))

			                        (with-temp-buffer
				                        (insert-file-contents file)
				                        (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
				                        (bibtex-search-entry key nil 0)
				                        (setq bibtex-entry (bibtex-parse-entry))
                                ;; downcase field names so they work in the format-citation code
				                        (dolist (cons-cell bibtex-entry)
				                          (setf (car cons-cell) (downcase (car cons-cell))))
				                        (setq entry-type (downcase (cdr (assoc "=type=" bibtex-entry))))

				                        (setq format (cdr (assoc entry-type org-ref-bibliography-entry-format)))
				                        (if format
				                            (setq entry  (org-ref-reftex-format-citation bibtex-entry format))
				                          ;; if no format, we use the bibtex entry itself as a fallback
				                          (save-restriction
				                            (bibtex-narrow-to-entry)
				                            (setq entry (buffer-string)))))
			                        (replace-regexp-in-string "\"" "" (htmlize-escape-or-link entry)))
                            (let ((org-ref-bibliography-files (org-ref-find-bibliography))
				                          (file) (entry) (bibtex-entry) (entry-type) (format)
				                          (org-ref-bibliography-entry-format
				                           '(("article" . "(%2a, %y)")
				                             ("book" . "(%2a, %y)")
				                             ("techreport" . "(%2a, %y)")
				                             ("proceedings" . "(%2a, %y)")
				                             ("inproceedings" . "(%2a, %y)")
                                     ("misc" . "(%2a, %y)"))))
			                        (setq file (catch 'result
					                                 (cl-loop for file in org-ref-bibliography-files do
						                                        (if (org-ref-key-in-file-p key (file-truename file))
							                                          (throw 'result file)
						                                          (message "%s not found in %s"
							                                                 key (file-truename file))))))

			                        (with-temp-buffer
				                        (insert-file-contents file)
				                        (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
				                        (bibtex-search-entry key nil 0)
				                        (setq bibtex-entry (bibtex-parse-entry))
                                ;; downcase field names so they work in the format-citation code
				                        (dolist (cons-cell bibtex-entry)
				                          (setf (car cons-cell) (downcase (car cons-cell))))
				                        (setq entry-type (downcase (cdr (assoc "=type=" bibtex-entry))))

				                        (setq format (cdr (assoc entry-type org-ref-bibliography-entry-format)))
				                        (if format
				                            (setq entry  (org-ref-reftex-format-citation bibtex-entry format))
				                          ;; if no format, we use the bibtex entry itself as a fallback
				                          (save-restriction
				                            (bibtex-narrow-to-entry)
				                            (setq entry (buffer-string)))))
                              entry)))
                  (s-split "," keyword) "<sup>,</sup>"))
      ;; for  pandoc we generate pandoc citations
      ((eq format 'pandoc)
       (cond
        (desc ;; pre and or post text
         (let* ((text (split-string desc "::"))
                (pre (car text))
                (post (cadr text)))
           (concat
            (format "[@%s," keyword)
            (when pre (format " %s" pre))
            (when post (format ", %s" post))
            "]")))
        (t
         (format "[%s]"
                 (mapconcat
                  (lambda (key) (concat "@" key))
                  (org-ref-split-and-strip-string keyword)
                  "; "))))))))

(defun org-ref-define-citation-link (type &optional key)
  "Add a citation link of TYPE for `org-ref'.
With optional KEY, set the reftex binding.  For example:
\(org-ref-define-citation-link \"citez\" ?z) will create a new
citez link, with reftex key of z, and the completion function."
  (interactive "sCitation Type: \ncKey: ")

  ;; create the formatting function
  (eval `(org-ref-make-format-function ,type))

  (eval
   `(if (fboundp 'org-link-set-parameters)
	      (org-link-set-parameters
	       ,type
	       :follow (lambda (_) (funcall org-ref-cite-onclick-function nil))
	       :export (quote ,(intern (format "org-ref-format-%s" type)))
	       :complete (quote ,(intern (format "org-%s-complete-link" type)))
	       :help-echo (lambda (window object position)
		                  (when org-ref-show-citation-on-enter
			                  (save-excursion
			                    (goto-char position)
			                    ;; Here we wrap the citation string to a reasonable size.
			                    (let ((s (org-ref-format-entry
				                            (org-ref-get-bibtex-key-under-cursor))))
			                      (with-temp-buffer
			                        (insert s)
			                        (fill-paragraph)
			                        (buffer-string))))))
	       :face 'org-ref-cite-link-face-fn
	       :display 'full
	       :keymap org-ref-cite-keymap)
      (org-add-link-type
       ,type
       (lambda (_path) (funcall org-ref-cite-onclick-function nil))
       (quote ,(intern (format "org-ref-format-%s" type))))))

  ;; create the completion function
  (eval `(org-ref-make-completion-function ,type))

  ;; store new type so it works with adding citations, which checks
  ;; for existence in this list
  (add-to-list 'org-ref-cite-types type)

  (unless (assoc 'org reftex-cite-format-builtin)
    (add-to-list 'reftex-cite-format-builtin '(org "org-ref citations" ())))

  ;; and finally if a key is specified, we modify the reftex menu
  (when key
    (setf (nth 2 (assoc 'org reftex-cite-format-builtin))
          (append (nth 2 (assoc 'org reftex-cite-format-builtin))
                  `((,key  . ,(concat type ":%l")))))))

(defun org-ref-generate-cite-links ()
  "Create all the link types and their completion functions."
  (interactive)
  (dolist (type org-ref-cite-types)
    (org-ref-define-citation-link type))
  (when (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "cite" :store #'org-ref-bibtex-store-link)))

;; This is what actually generated the cite links
(org-ref-generate-cite-links)

(provide 'org-ref-ox-hugo)
