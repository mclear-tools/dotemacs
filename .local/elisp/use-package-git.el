;;; use-package-git.el --- Git extension for use-package -*- lexical-binding: t -*-

;; Author: Matthew Sojourner Newton
;; Maintainer: Matthew Sojourner Newton
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (use-package "2.4"))
;; Requires: ((git "1.7.2.3"))
;; Homepage: https://github.com/mnewt/dotemacs
;; Keywords: dotemacs config package git


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This package adds support for installing packages directly from git
;; repositories.

;; When the :git keyword is specified and :ensure is non-nil, the given package
;; will be cloned, byte compiled, and added to `load-path'.

;; Upgrade functions are also provided:

;; `use-package-git-upgrade-package' - Upgrade a package.

;; `use-package-git-upgrade-all-packages' - Upgrade all packages.

;;; Examples:

;; (use-package package
;;   :ensure t
;;   :git "https://git.example.org/user/package")

;; (use-package package
;;   :ensure t
;;   :git (:name package-name
;;         :uri "https://git.example.org/user/package"
;;         :dir "/path/to/local/copy/of/repo"
;;         :files "*.el"
;;         :ref "tags/1.0"))


;; * TODO Generate package description
;; * TODO Generate autoloads file
;; * TODO :command keyword to support `org' make
;; * TODO Add the directory of :files to `load-path'
;; * TODO Install Info manual
;; * TODO Install regular manual?

;;; Code:

(require 'use-package-ensure)

(defcustom use-package-git-user-dir
  (expand-file-name "git" user-emacs-directory)
  "Directory containing the user's git packages."
  :group 'use-package-ensure
  :type 'string)

(defvar use-package-git--packages nil
  "Alist specifying packages ensured by `use-package-git'.

CAR is the package's local name as a symbol.

CDR is a Plist that contains the information needed to fetch the
package via git.")

(defvar use-package-git--upgrade-package-history nil
  "History for `use-package-git-upgrade-package' command.")

(defvar use-package-git--previous-ensure-function nil
  "The previous value of `use-package-ensure-function'.")

(defun use-package-git--byte-compile-package (config)
  "Byte compile Elisp for CONFIG."
  (let* ((dir (plist-get config :dir))
         (default-directory (if (file-name-absolute-p dir)
                                dir
                              (expand-file-name dir use-package-git-user-dir))))
    (dolist (file (seq-mapcat #'file-expand-wildcards (plist-get config :files)))
      (unless (string-prefix-p "." file)
        (save-window-excursion (byte-compile-file file t))))))

(defun use-package-git-upgrade-package (package)
  "Upgrade PACKAGE.

PACKAGE is a string, symbol, or config Plist."
  (interactive
   (list (completing-read
          "Upgrade git package: "
          (mapcar (lambda (p) (symbol-name (car p))) use-package-git--packages)
          nil t nil use-package-git--upgrade-package-history)))
  (when (stringp package) (setq package (intern package)))
  (when (symbolp package) (setq package (alist-get package use-package-git--packages)))
  (let ((dir (expand-file-name (plist-get package :dir) use-package-git-user-dir)))
    (when (or (= 0 (length (shell-command-to-string
                            (format "git -C '%s' status --porcelain" dir))))
              (while (pcase (downcase
                             (read-key (concat
                                        "The package `"
                                        (symbol-name (plist-get package :name))
                                        "' with local repo at ["
                                        dir
                                        "] is dirty."
                                        " Choose an action:\n"
                                        "[R]eset to HEAD and continue\n"
                                        "[S]kip repo and continue\n"
                                        "[A]bort\n"
                                        "? ")))
                       (?r (= 0 (shell-command-to-string
                                 (format "git -C '%s' reset HEAD --hard" dir))))
                       (?s nil)
                       (?a (user-error "Aborted package upgrade"))
                       (_ t))))
      (shell-command (format "git -C '%s' fetch" dir) "*use-package-git*")
      (shell-command
       (format "git -C '%s' checkout '%s'" dir (or (plist-get package :ref) "master"))
       "*use-package-git*")
      (use-package-git--byte-compile-package package))))

(defun use-package-git-upgrade-all-packages ()
  "Upgrade all git ensured packages."
  (interactive)
  (dolist (package use-package-git--packages)
    (use-package-git-upgrade-package (cdr package))))

(defun use-package-ensure-git (name config)
  "Ensure that the git package NAME is cloned.

CONFIG is the Plist supplied as the value to the :git key, then
normalized with `use-package-normalize/:git'."
  (add-to-list 'use-package-git--packages (cons name config))
  (unless (and (file-exists-p use-package-git-user-dir)
               (file-directory-p use-package-git-user-dir))
    (make-directory use-package-git-user-dir t))
  (let ((dir (expand-file-name (plist-get config :dir) use-package-git-user-dir)))
    (unless (file-exists-p dir)
      ;; Clone the repo.
      (message "use-package-ensure-git is cloning package %s..." name)
      (shell-command (format "git -C %s clone %s %s"
                             use-package-git-user-dir
                             (plist-get config :uri)
                             dir)
                     "*use-package-git*")
      ;; Check out the ref (should work for branch, hash, or tag).
      (when-let (ref (plist-get config :ref))
        (shell-command (format "git -C %s checkout %s" dir ref)))
      (use-package-git--byte-compile-package config))
    (add-to-list 'load-path dir))
  name)

(defun use-package-git--dispatch-ensure (name args state &optional no-refresh)
  "Dispatch package NAME to the git and elpa ensure functions.))))))

ARGS, STATE, and NO-REFRESH are passed through."
  (unless (plist-get state :ensured)
    (use-package-ensure-elpa name args state no-refresh)))

(defun use-package-normalize/:git (name keyword args)
  "Normalize the :git property list for package NAME.

KEYWORD is the keyword that caused this function to be called
so... it's :git.

ARGS is a list of forms following the KEYWORD--in this case a
list of one."
  (use-package-only-one (symbol-name keyword) args
    (lambda (_label config)
      (cond
       ((stringp config)
        (list :name name
              :uri config
              :dir (file-name-base config)
              :files '("*.el")))
       ((and (listp config) (stringp (plist-get config :uri)))
        (let ((c config))
          (setq c (plist-put c :name (or (plist-get c :name) name)))
          (setq c (plist-put c :dir (or (plist-get c :dir)
                                        (file-name-base (plist-get c :uri)))))
          (setq c (plist-put c :files (let ((d (plist-get c :files)))
                                        (cond
                                         ((stringp d) (list d))
                                         ((listp d) d)
                                         ((not d) '("*.el"))))))
          c))
       (t
        (use-package-error
         (concat ":git wants either a string or a Plist with a :uri key")))))))

(defun use-package-handler/:git (name _keyword config rest state)
  "Simply return `body' of package NAME.

STATE is updated to tell `use-package-git--dispatch-ensure' that this
package is already ensured and does not need to dispatch to
`ensure'.

CONFIG is the PList containing the git configuration.

NAME and REST are passed to `use-package-process-keywords'."
  (let* ((state (plist-put state :ensured t))
         (body (use-package-process-keywords name rest state)))
    ;; We use the same logic as `use-package-handler/:ensure'.
    (if (bound-and-true-p byte-compile-current-file)
        ;; Eval when byte-compiling,
        (use-package-ensure-git name config)
      ;;  or else wait until runtime.
      (push `(use-package-ensure-git ',name ',config) body))
    body))

(add-to-list 'use-package-keywords :git)

(setq use-package-git--previous-ensure-function use-package-ensure-function
      use-package-ensure-function #'use-package-git--dispatch-ensure)

(provide 'use-package-git)

;;; use-package-git.el ends here
