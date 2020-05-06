;;; gited-tests.el --- Tests for gited.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>,
;; Keywords:

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; code:

(require 'ert)
(require 'gited)
(eval-when-compile (require 'cl-lib))

;; Settings for a test repository.
(defvar gited-user-name "John Doe")
(defvar gited-user-email "john.doe@example.com")
(defvar gited-initial-commit-msg "Initialize repository.")
(defvar gited-initial-filename "foo")
(defvar gited-initial-file-content "Test file")

(defvar gited-remote-repo "https://github.com/calancha/foo")
(defvar gited-remote-repo-branch "fail-say-foo-test")
(defvar gited-remote-repo-file "do_not_delete.el")

(defun gited-create-new-repo (dir)
  "Create a new repository at DIR and return its gited buffer."
  (let ((inhibit-message t))
    (write-region gited-initial-file-content
                  nil
                  (expand-file-name gited-initial-filename dir))
    (dired dir)
    (gited-git-command '("init"))
    (gited-git-command `("config" "user.email" ,gited-user-email))
    (gited-git-command `("config" "user.name" ,gited-user-name))
    (gited-git-command `("add" ,gited-initial-filename))
    (gited-git-command `("commit" "-m" ,gited-initial-commit-msg))
    (gited-list-branches "local")
    gited-buffer))

(defmacro with-gited-repo (dir &rest body)
  "Create a new Git repository at DIR and evaluate BODY.
The repository consists of just one file with content
`gited-initial-file-content'.
The forms in BODY are evaluated with DIR as `default-directory'."
  (declare (indent 1) (debug (form body)))
  `(let* ((gited-expert t)
          (inhibit-message t))
     (unwind-protect
         (progn
           (gited-create-new-repo ,dir)
           ,@body)
       (delete-directory ,dir 'recursive))))

(defmacro with-specified-completion-branch (branch &rest body)
  "Fix branch completions to BRANCH and evaluate BODY.
This macro uses `cl-letf' to temporary fix the completions.
Return the last evaled BODY form."
  (declare (indent 1) (debug (form body)))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (&rest _) ,branch)))
     ,@body))

(ert-deftest gited-test1 ()
  (skip-unless (executable-find vc-git-program))
  (let ((dir (make-temp-file "gited" 'dir)))
    (with-gited-repo dir
      (progn
        (should (gited-dir-under-Git-control-p))
        (should (gited-buffer-p))
        (should (equal gited-initial-commit-msg (gited--last-commit-title)))
        (should (equal "master" (gited-current-branch)))
        ;; Only master branch do exist
        (should-not (gited-branch-exists-p gited-initial-filename))
        ;; Create a new branch (copy of master)
        (gited-copy-branch "master" gited-initial-filename)
        (should (gited-branch-exists-p gited-initial-filename))
        (gited-toggle-marks)
        (should (= 2 (gited-number-marked)))
        (gited-unmark-all-marks)
        (should (zerop (gited-number-marked)))
        ;; Update the file in the current branch and commit the changes
        (gited-with-current-branch gited-initial-filename
          (write-region "Changed this file" nil gited-initial-filename)
          (gited-git-command `("add" ,gited-initial-filename))
          (gited-git-command '("commit" "-m" "Update file"))
          (let ((hash
                 (with-temp-buffer
                   (gited-git-command
                    '("rev-parse" "HEAD") (current-buffer))
                   (buffer-substring 1 (1- (point-max))))))
            ;; gited-mark-branches-containing-commit
            (gited-mark-branches-containing-commit hash)
            (should (= 1 (gited-number-marked))))
          ;; gited-mark-branches-regexp
          (gited-unmark-all-marks)
          (gited-mark-branches-regexp gited-initial-filename)
          (should (= 1 (gited-number-marked)))
          ;; gited-mark-branches-containing-regexp
          (gited-unmark-all-marks)
          (gited-mark-branches-containing-regexp "Update")
          (should (= 1 (gited-number-marked)))
          ;; gited-mark-branches-by-date
          (gited-unmark-all-marks)
          (gited-mark-branches-by-date
           (format-time-string "%F" (current-time)))
          (should (= (length (gited-listed-branches))
                     (gited-number-marked)))
          (gited-unmark-all-marks)
          (gited-mark-branches-by-date
           (format-time-string
            "%F"
            (time-add (current-time) (seconds-to-time (* 7 24 60 60)))))
          (should (zerop (gited-number-marked)))
          (gited-unmark-all-marks))
        ;; Copy the updated branch into a new branch "bar"
        (gited-copy-branch gited-initial-filename "bar")
        ;; Test delete/rename branch features
        (gited-delete-branch gited-initial-filename 'force)
        (gited-update)
        (should-not (gited-branch-exists-p gited-initial-filename))
        (gited-rename-branch "bar" gited-initial-filename) ; Asynchronous.
        (while gited-branch-after-op
          (sit-for 0.05))
        (should (gited-branch-exists-p gited-initial-filename))))))

(ert-deftest gited-test2 ()
  (skip-unless (executable-find vc-git-program))
  (let* ((dir (make-temp-file "gited" 'dir))
         (gited-expert t)
         (inhibit-message t)
         dired-buf)
	(cd dir)
    (unwind-protect
        (progn
          (gited-git-command `("clone" ,gited-remote-repo))
          (setq dired-buf (dired (expand-file-name "foo")))
          (gited-list-branches "local")
          (should (equal "origin" gited-current-remote-rep))
          (should-error (gited-change-current-remote-rep)) ; Only 1 remote rep
          (gited-list-branches "remote")
          (gited-copy-branch (concat "origin/" gited-remote-repo-branch)
                             gited-remote-repo-branch)
          (gited-list-branches "local")
          (gited-goto-branch "master")
          (with-specified-completion-branch gited-remote-repo-branch
            (gited-merge-branch "master"))
          (load-file gited-remote-repo-file)
          ;; Now it fails: At master branch, `say-foo' returns 'foo
          ;; But at branch `gited-remote-repo-file', `say-foo' returns 'bar.
          (should-not (eq 'foo (say-foo))))
      (delete-directory dir 'recursive)
      (kill-buffer dired-buf))))

(ert-deftest gited-ci-load ()
  "Tests to see whether gited-ci has been loaded."
  (should (fboundp 'gited-parse-ci-status)))

(ert-deftest gited-test-add-patch-bug ()
  "Tests for bug in `gited-add-patched-files'."
  (skip-unless (executable-find vc-git-program))
  (let* ((dir1 (make-temp-file "gited-1" 'dir))
         (dir2 (make-temp-file "gited-2" 'dir))
         (gited-buffer-1 (gited-create-new-repo dir1))
         (gited-buffer-2 (gited-create-new-repo dir2))
         (inhibit-message t))
    (unwind-protect
        (progn
          (pop-to-buffer gited-buffer-1)
          (write-region "Changed this file" nil gited-initial-filename)
          (pop-to-buffer gited-buffer-2)
          ;; Add a new file inside a subdirectory
          (mkdir (expand-file-name "subdir" dir2))
          (write-region "New nested file" nil (concat "subdir/" gited-initial-filename))
          (should (gited-add-patched-files (gited-untracked-files)))
          (pop-to-buffer gited-buffer-1)
          ;; The bug causes the following to fail
          (should (gited-add-patched-files (gited-modified-files))))
      ;; Clean up
      (delete-directory dir1 'recursive)
      (delete-directory dir2 'recursive))))

(ert-deftest gited-diff-test ()
  "Test for `gited-diff'.
Create a repository with 2 identical branches, `master' and `foo'.
Each branch contains just one file `foo'.  Since this file has same name as one
of the branches, then we must add '--' after the versions.
For example, the Git command to get the diff between the 2 branches is as follows:
git diff master foo --."
  (skip-unless (executable-find vc-git-program))
  (let ((dir (make-temp-file "gited" 'dir))
        (inhibit-message t))
    (with-gited-repo dir
      (gited-copy-branch "master" gited-initial-filename)
      (with-specified-completion-branch gited-initial-filename
        (should-not (gited-diff "master"))))))

(ert-deftest gited-output-buffer-default-directory-test ()
  "Test that `gited-output-buffer' `default-directory' equals `gited-top-level-dir'."
  (skip-unless (executable-find vc-git-program))
  (let* ((dir (make-temp-file "gited" 'dir))
         (subdir (expand-file-name "subdir" dir))
         (new-file (expand-file-name "new-file" subdir))
         (inhibit-message t)
         toplevel-dir)
    (with-gited-repo dir
      (setq toplevel-dir gited-toplevel-dir)
      (make-directory subdir)
      (write-region "hello" nil new-file)
      (gited-git-command `("add" ,new-file))
      (gited-git-command `("commit" "-m" "Add new-file inside a subdir"))
      ;; Create `gited-output-buffer' from subdir; this new buffer must
      ;; have `default-directory' set to `gited-toplevel-dir'
      (cd subdir)
      (with-current-buffer (gited--output-buffer)
        (should (equal default-directory toplevel-dir))))))

(provide 'gited-tests)
;;; gited-tests.el ends here
