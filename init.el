;;; init.el --- Global settings -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;; This is the bootstrap code for getting the literate configuration file in =config.org= up and running. The original source for much of this code is from sriramkswamy/dotemacs at =https://github.com/sriramkswamy/dotemacs=.

;;; Code:
 ;;Normally file-name-handler-alist is set to
  ;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
  ;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
  ;; ("\\`/:" . file-name-non-special))
  ;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
 ;;setting this should ease startup time
(let ((file-name-handler-alist nil))
  ;; debug
  (setq debug-on-error t)
  (setq debug-on-quit t)
  ;; Increase the garbage collection threshold to decrease startup time
  (setq gc-cons-threshold 100000000)

  ;; Show elapsed start-up time in mini-buffer
  (let ((emacs-start-time (current-time)))
    (add-hook 'emacs-startup-hook
              (lambda ()
                (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                  (message "[Emacs initialized in %.3fs]" elapsed)))))

  ;; List package archives and initialize them
  ;; (setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
  (setq load-prefer-newer t)
  (require 'package)
  (setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                           ("gnu"       . "http://elpa.gnu.org/packages/")
                           ("melpa"     . "https://melpa.org/packages/")))
  (package-initialize)
  ;; ("marmalade" . "http://marmalade-repo.org/packages/")
  ;; (when (>= emacs-major-version 24)
  ;;   (require 'package)
  ;;   (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  ;;   (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  ;;   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))
  ;; (when (< emacs-major-version 24)
  ;;   ;; For important compatibility libraries like cl-lib
  ;;   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

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

  (require 'use-package)
  (require 'bind-key) ;; if you use any :bind variant
  (require 'diminish) ;; if you use diminish

  ;; This is GPLv2. If you still don't know the details, read
  ;; http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html
  ;; From Holger Schurig's config.
  (defun my-tangle-section-canceled ()
    "Return t if the current section header was DISABLED, else nil."
    (save-excursion
      (if (re-search-backward "^\\*+\\s-+\\(.*?\\)?\\s-*$" nil t)
          (string-prefix-p "DISABLED" (match-string 1))
        nil)))

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
        (insert (format ";; Don't edit this file, edit %s instead ...\n\n" orgfile))
        (apply 'insert (reverse body-list)))
      (message "Wrote %s ..." elfile)))

  (let ((orgfile (locate-user-emacs-file "config.org"))
      (elfile (locate-user-emacs-file "config.el")))
  (when (or (not (file-exists-p elfile))
            (file-newer-than-file-p orgfile elfile))
    (my-tangle-config-org orgfile elfile))
  (load-file elfile))

  (defun my-tangle-config-org-hook-func ()
    (when (string= "config.org" (buffer-name))
  	(let ((orgfile (locate-user-emacs-file "config.org"))
  		  (elfile (locate-user-emacs-file "config.el")))
  	  (my-tangle-config-org orgfile elfile))))
  (add-hook 'after-save-hook #'my-tangle-config-org-hook-func)

  ;; load config -- Note that config file must exist & have source code for tangling, otherwise errors get thrown!!
  ;; (defvar init-source-org-file (expand-file-name "config.org" user-emacs-directory)
  ;;   "The file that our emacs initialization comes from")

  ;; (defvar init-source-el-file (expand-file-name "config.el" user-emacs-directory)
  ;;   "The file that our emacs initialization is generated into")

  ;; (if (file-exists-p init-source-org-file)
  ;;     (if (and (file-exists-p init-source-el-file)
  ;;              (time-less-p (nth 5 (file-attributes init-source-org-file)) (nth 5 (file-attributes init-source-el-file))))
  ;;         (load-file init-source-el-file)
  ;;       (if (fboundp 'org-babel-load-file)  
  ;;           (org-babel-load-file init-source-org-file)
  ;;         (message "Function not found: org-babel-load-file")
  ;;         (load-file init-source-el-file)))
  ;;   (error "Init org file '%s' missing." init-source-org-file))

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
;;; init.el ends here
  )
