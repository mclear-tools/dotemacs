(defvar cpm--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;; Startup timing
(defvar my-start-time (current-time)
  "Time when Emacs was started")

;;; Untangling and loading
;;; Taken, with slight modifications, from http://www.holgerschurig.de/en/emacs-init-tangle/

(defun my-tangle-section-canceled ()
  "Checks if the previous section header was DISABLED"
  (save-excursion
    (if (re-search-backward "^\\*+\\s-+\\(.*?\\)?\\s-*$" nil t)
        (progn
          ;; (message "FOUND '%s'" (match-string 1))
          (string-prefix-p "DISABLED" (match-string 1)))
      nil)))

(defun my-tangle-config-org (orgfile elfile)
  "This function will write all source blocks from =config.org= into
=config.el= that are ...

- not marked as :tangle no
- have a source-code of =emacs-lisp=
- doesn't have the todo-marker DISABLED"
  (let* (;; list where we cobble together body parts
         (body-list ())
         ;; disable special file handlers when loading .org files
         (file-name-handler-alist nil)
         ;; monster-regexp to extract pieces out of an .org file
         (org-babel-src-block-regexp (concat
                                      ;; (1) indentation                 (2) lang
                                      "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
                                      ;; (3) switches
                                      "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
                                      ;; (4) header arguments
                                      "\\([^\n]*\\)\n"
                                      ;; (5) body
                                      "\\([^\000]*?\n\\)??[ \t]*#\\+end_src")))
    (with-temp-buffer
      (insert-file-contents orgfile)
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
        (let ((lang (match-string 2))
              (args (match-string 4))
              (body (match-string 5))
              (canc (my-tangle-section-canceled)))
          (when (and (string= lang "emacs-lisp")
                     (not (string-match-p ":tangle\\s-+no" args))
                     (not canc))
              (add-to-list 'body-list body)))))
    (with-temp-file elfile
      (insert ";; *- lexical-binding: t; -*-\n")
      (insert (format ";; Don't edit this file, edit %s instead ...\n\n" orgfile))
      ;; (insert (apply 'concat (reverse body-list)))
      (apply 'insert (reverse body-list)))
    ))

(defun my-load-file (fname)
  "This loads an elisp configuration file. If an .org file exists,
it will be first untangled. If a byte-compiled file does NOT exist,
it will be created. After this, the normal loading logic happens."
  (let* (;; disable garbage collection while we do heavy string work
		 (gc-cons-threshold most-positive-fixnum)
         ;; fname with various extensions
         (sansfile (expand-file-name (file-name-sans-extension fname) user-emacs-directory))
         (orgfile (concat sansfile ".org"))
         (elfile  (concat sansfile ".el"))
         (elcfile (concat sansfile ".elc")))
    (when (file-exists-p orgfile)
      (when (or (not (file-exists-p elfile))
                (file-newer-than-file-p orgfile elfile))
        (my-tangle-config-org orgfile elfile)))
    ;; (when (or (not (file-exists-p elcfile))
    ;;           (file-newer-than-file-p elfile elcfile))
    ;;   (byte-compile-file elfile))
    (load elfile nil 'nomessage)))

;;; Actually loading my configuration

(my-load-file "config")

;; reset file-name-handler-alist
(add-hook! 'emacs-startup-hook
  (setq file-name-handler-alist cpm--file-name-handler-alist))

