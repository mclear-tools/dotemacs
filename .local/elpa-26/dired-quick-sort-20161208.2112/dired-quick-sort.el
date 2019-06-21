;;; dired-quick-sort.el --- Persistent quick sorting of dired buffers in various ways. -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Hong Xu <hong@topbug.net>

;; Author: Hong Xu <hong@topbug.net>
;; URL: https://gitlab.com/xuhdev/dired-quick-sort#dired-quick-sort
;; Package-Version: 20161208.2112
;; Version: 0.1+
;; Package-Requires: ((hydra "0.13.0"))
;; Keywords: convenience, files

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides ways to quickly sort dired buffers in various ways.
;; With `savehist-mode' enabled (strongly recommended), the last used sorting
;; criteria are automatically used when sorting, even after restarting Emacs.  A
;; hydra is defined to conveniently change sorting criteria.
;;
;; For a quick setup, Add the following configuration to your "~/.emacs" or
;; "~/.emacs.d/init.el":
;;
;;     (require 'dired-quick-sort)
;;     (dired-quick-sort-setup)
;;
;; This will bind "S" in dired-mode to invoke the quick sort hydra and new Dired
;; buffers are automatically sorted according to the setup in this package.  See
;; the document of `dired-quick-sort-setup` if you need a different setup.  It
;; is recommended that at least "-l" should be put into
;; `dired-listing-switches'.  If used with dired+, you may want to set
;; `diredp-hide-details-initially-flag' to nil.
;;
;; To make full use of this extensions, please make sure that the variable
;; `insert-directory-program' points to the GNU version of ls.
;;
;; To report bugs and make feature requests, please open a new ticket at the
;; issue tracker <https://gitlab.com/xuhdev/dired-quick-sort/issues>. To
;; contribute, please create a merge request at
;; <https://gitlab.com/xuhdev/dired-quick-sort/merge_requests>.
;;
;; For any other questions and comments, please send them to
;; <https://www.topbug.net/blog/2016/08/17/dired-quick-sort-sort-dired-buffers-quickly-in-emacs/>.

;;; Code:

(require 'dired)
(require 'ls-lisp)
(require 'savehist)
(require 'hydra)

(defvar dired-quick-sort-sort-by-last "version"
  "The main sort criteria used last time.

The value should be one of none, time, size, version (natural, an improved
version of name and extension.

See the documentation of the \"--sort\" option of GNU ls for details.")
(add-to-list 'savehist-additional-variables 'dired-quick-sort-sort-by-last)
(defvar dired-quick-sort-reverse-last ?n
  "Whether reversing was enabled when sorting was used last time.

The value should be either ?y or ?n.")
(add-to-list 'savehist-additional-variables 'dired-quick-sort-reverse-last)
(defvar dired-quick-sort-group-directories-last ?n
  "Whether directories are grouped together when sorting was used last time.

The value should either be ?y or ?n.")
(add-to-list 'savehist-additional-variables
             'dired-quick-sort-group-directories-last)
(defvar dired-quick-sort-time-last "default"
  "The time option used last time.

The value should be one of default (modified time), atime, access, use, ctime or
status.  If the sort-by option is set as \"time\", the specified time will be
used as the key for sorting.

See the documentation of the \"--time\" option of GNU ls for details.")
(add-to-list 'savehist-additional-variables 'dired-quick-sort-time-last)

;;;###autoload
(defun dired-quick-sort (&optional sort-by reverse group-directories time)
  "Sort dired by the given criteria.

The possible values of SORT-BY, REVERSE, GROUP-DIRECTORIES and TIME are
explained in the variable `dired-quick-sort-reverse-last',
`dired-quick-sort-reverse-last', `dired-quick-sort-group-directories-last' and
`dired-quick-sort-time-last' respectively.  Besides, passing nil to any of these
arguments to use the value used last time (that is, the values of the four
variables mentioned before), even after restarting Emacs if `savehist-mode' is
enabled.  When invoked interactively, nil's are passed to all arguments."
  (interactive)
  (setq dired-quick-sort-sort-by-last (or sort-by dired-quick-sort-sort-by-last)
        dired-quick-sort-reverse-last (or reverse dired-quick-sort-reverse-last)
        dired-quick-sort-group-directories-last
        (or group-directories dired-quick-sort-group-directories-last)
        dired-quick-sort-time-last (or time dired-quick-sort-time-last))
  (dired-sort-other
   (format "%s --sort=%s %s %s %s" dired-listing-switches
           dired-quick-sort-sort-by-last
           (if (char-equal dired-quick-sort-reverse-last ?y)
               "-r" "")
           (if (char-equal dired-quick-sort-group-directories-last ?y)
               "--group-directories-first" "")
           (if (not (string= dired-quick-sort-time-last "default"))
               (concat "--time=" dired-quick-sort-time-last) ""))))

(defun dired-quick-sort--sort-by-last (field)
  (if (string= dired-quick-sort-sort-by-last field) "[X]" "[ ]"))

(defhydra hydra-dired-quick-sort (:hint none :color pink)
  "
^Sort by^                   ^Reverse^               ^Group Directories^            ^Time
^^^^^^^^^----------------------------------------------------------------------------------------------------------------
_n_: ?n? none               _r_: ?r? yes            _g_: ?g? yes                   _d_: ?d? default (last modified time)
_t_: ?t? time               _R_: ?R? no             _G_: ?G? no                    _A_: ?A? atime
_s_: ?s? size               ^ ^                     ^ ^                            _a_: ?a? access
_v_: ?v? version (natural)  ^ ^                     ^ ^                            _u_: ?u? use
_e_: ?e? extension          ^ ^                     ^ ^                            _c_: ?c? ctime
_q_: quit                   ^ ^                     ^ ^                            _S_: ?S? status
"
  ("n" (dired-quick-sort "none" nil nil nil)
       (dired-quick-sort--sort-by-last "none"))
  ("t" (dired-quick-sort "time" nil nil nil)
       (dired-quick-sort--sort-by-last "time"))
  ("s" (dired-quick-sort "size" nil nil nil)
       (dired-quick-sort--sort-by-last "size"))
  ("v" (dired-quick-sort "version" nil nil nil)
       (dired-quick-sort--sort-by-last "version"))
  ("e" (dired-quick-sort "extension" nil nil nil)
       (if (string= dired-quick-sort-sort-by-last "extension") "[X]" "[ ]"))
  ("r" (dired-quick-sort nil ?y nil nil)
       (if (char-equal dired-quick-sort-reverse-last ?y) "[X]" "[ ]"))
  ("R" (dired-quick-sort nil ?n nil nil)
       (if (char-equal dired-quick-sort-reverse-last ?n) "[X]" "[ ]"))
  ("g" (dired-quick-sort nil nil ?y nil)
       (if (char-equal dired-quick-sort-group-directories-last ?y) "[X]" "[ ]"))
  ("G" (dired-quick-sort nil nil ?n nil)
       (if (char-equal dired-quick-sort-group-directories-last ?n) "[X]" "[ ]"))
  ("d" (dired-quick-sort nil nil nil "default")
       (if (string= dired-quick-sort-time-last "default") "[X]" "[ ]"))
  ("A" (dired-quick-sort nil nil nil "atime")
       (if (string= dired-quick-sort-time-last "atime") "[X]" "[ ]"))
  ("a" (dired-quick-sort nil nil nil "access")
       (if (string= dired-quick-sort-time-last "access") "[X]" "[ ]"))
  ("u" (dired-quick-sort nil nil nil "use")
       (if (string= dired-quick-sort-time-last "use") "[X]" "[ ]"))
  ("c" (dired-quick-sort nil nil nil "ctime")
       (if (string= dired-quick-sort-time-last "ctime") "[X]" "[ ]"))
  ("S" (dired-quick-sort nil nil nil "status")
       (if (string= dired-quick-sort-time-last "status") "[X]" "[ ]"))
  ("q" nil "quit" :hint t :color blue))

;;;###autoload
(defun dired-quick-sort-setup ()
  "Run the default setup.

This will bind the key S in `dired-mode' to run
`hydra-dired-quick-sort/body', and automatically run the sorting
criteria after entering `dired-mode'.  You can choose to not call
this setup function and run a modified version of this function
to use your own preferred setup:

  ;; Replace \"S\" with other keys to invoke the dired-quick-sort hydra.
  (define-key dired-mode-map \"S\" 'hydra-dired-quick-sort/body)
  ;; Automatically use the sorting defined here to sort.
  (add-hook 'dired-mode-hook 'dired-quick-sort)"

  (if (not ls-lisp-use-insert-directory-program)
      (display-warning 'dired-quick-sort "`ls-lisp-use-insert-directory-program'
is nil. The package `dired-quick-sort' will not work and thus is not set up by
`dired-quick-sort-setup'." :warning)
    (define-key dired-mode-map "S" 'hydra-dired-quick-sort/body)
    (add-hook 'dired-mode-hook 'dired-quick-sort)))

(provide 'dired-quick-sort)

;;; dired-quick-sort.el ends here

;; Local Variables:
;; coding: utf-8
;; fill-column: 80
;; indent-tabs-mode: nil
;; sentence-end-double-space: t
;; End:
