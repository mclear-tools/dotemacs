;;; init.el --- Global settings -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;; This is the bootstrap code for getting the literate configuration file in config.org up and running. The original source for much of this code is from sriramkswamy/dotemacs at https://github.com/sriramkswamy/dotemacs.

;;; Code:
 ;;Normally file-name-handler-alist is set to
  ;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
  ;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
  ;; ("\\`/:" . file-name-non-special))
  ;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.

 ;;setting this should ease startup time
(let ((file-name-handler-alist nil))
  ;; startup time
  (defvar my-start-time (current-time)
    "Time when Emacs was started")
  ;; debug
  (setq debug-on-error t)
  (setq debug-on-quit t)
  ;; Increase the garbage collection threshold to decrease startup time
  (setq gc-cons-threshold 500000000)

  ;; Show elapsed start-up time in mini-buffer
  ;; (let ((emacs-start-time (current-time)))
  ;;   (add-hook 'emacs-startup-hook
  ;;             (lambda ()
  ;;               (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  ;;                 (message "[Emacs initialized in %.3fs]" elapsed)))))

  ;; List package archives and initialize them
  ;; (setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
  (setq load-prefer-newer t)
  (require 'package)
  (setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                           ("gnu"       . "http://elpa.gnu.org/packages/")
                           ("melpa"     . "https://melpa.org/packages/")))
  (package-initialize)

  ;; Make sure Org is installed
  (unless (package-installed-p 'org)
    (package-refresh-contents)
    (package-install 'org))

  ;; Org plus contrib needs to be loaded before any org related functionality is called
  (unless (package-installed-p 'org-plus-contrib)
    (package-refresh-contents)
    (package-install 'org-plus-contrib))

  ;; Check if use-package is installed, and install if not. Then require it.
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  ;; use-package default installs all packages
  (setq use-package-always-ensure t)
  (setq use-package-verbose t)
  (require 'use-package)

  ;; This is GPLv2. If you still don't know the details, read
  ;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html
  ;; From Holger Schurig's config.

  (defun my-tangle-section-canceled ()
    "Checks if the previous section header was DISABLED"
    (save-excursion
      (if (re-search-backward "^\\*+\\s-+\\(.*?\\)?\\s-*$" nil t)
          (progn
            ;; (message "FOUND '%s'" (match-string 1))
            (string-prefix-p "DISABLED" (match-string 1)))
        nil)))

  ;; tangle on save
  (defun my-tangle-config-org-hook-func ()
    (when (string= "config.org" (buffer-name))
      (let ((orgfile (locate-user-emacs-file "config.org"))
            (elfile (locate-user-emacs-file "config.el")))
        (my-tangle-config-org orgfile elfile))))
  (add-hook 'after-save-hook #'my-tangle-config-org-hook-func)

  ;; This uses partially derived code from ob-core.el. So this snippet
  ;; is GPLv3 or later. If you still don't know the details, read
  ;; http://www.gnu.org/licenses/

  (defun my-tangle-config-org (orgfile elfile)
  "This function will write all source blocks from =config.org= into
=config.el= that are ...

- not marked as :tangle no
- have a source-code of =emacs-lisp=
- doesn't have the todo-marker DISABLED"
  (let* ((body-list ())
		 (gc-cons-threshold most-positive-fixnum)
         (org-babel-src-block-regexp   (concat
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
    (message "Wrote %s ..." elfile)
    ))

(let ((orgfile (concat user-emacs-directory "config.org"))
      (elfile (concat user-emacs-directory "config.el")))
  (when (or (not (file-exists-p elfile))
            (file-newer-than-file-p orgfile elfile))
    (my-tangle-config-org orgfile elfile))
  (load-file elfile))

 ;; Garbage collector - decrease threshold back to 5 MB
  (run-with-idle-timer
   5 nil
   (lambda ()
     (setq gc-cons-threshold 1000000)
     (message "gc-cons-threshold restored to %S"
              gc-cons-threshold)))

  ;; turn debug off
  (setq debug-on-error nil)
  (setq debug-on-quit nil)

  (message "Emacs initialized in %.2fs" (float-time (time-subtract (current-time) my-start-time)))
;;; init.el ends here
  )
