;;; gited-tests.el --- Tests for gited.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

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

(ert-deftest gited-test1 ()
  (when (executable-find vc-git-program)
    (let* ((dir (make-temp-file "gited" 'dir))
           (file (expand-file-name "foo" dir))
           (gited-expert t))
      (unwind-protect
          (let ((str "Initialize repository."))
            (write-region "Test file" nil file)
	        (dired dir)
            (gited-git-command '("init"))
	        (gited-git-command '("config" "user.email" "john.doe@example.com"))
	        (gited-git-command '("config" "user.name" "John Doe"))
            (gited-git-command '("add" "foo"))
            (gited-git-command `("commit" "-m" ,str))
            (gited-list-branches "local")
            (should (gited-dir-under-Git-control-p))
            (should (gited-buffer-p))
            (should (equal str (gited--last-commit-title)))
            (should (equal "master" (gited-current-branch)))
            (should-not (gited-branch-exists-p "foo"))
            (gited-copy-branch "master" "foo")
            (should (gited-branch-exists-p "foo"))
            (gited-toggle-marks)
            (should (= 2 (gited-number-marked)))
            (gited-unmark-all-marks)
            (should (= 0 (gited-number-marked)))
            (gited-with-current-branch "foo"
              ;; (gited-checkout-branch "foo")
              (write-region "Changed this file" nil file)
              (gited-git-command '("add" "foo"))
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
              (gited-mark-branches-regexp "foo")
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
              (should (= 0 (gited-number-marked)))
              (gited-unmark-all-marks))
            (gited-copy-branch "foo" "bar")
            (gited-delete-branch "foo" 'force)
            (gited-update)
            (should-not (gited-branch-exists-p "foo"))
            (gited-rename-branch "bar" "foo") ; Asynchronous.
            (while gited-branch-after-op
              (sit-for 0.05))
            (should (gited-branch-exists-p "foo")))
        (delete-directory dir 'recursive)))))

(provide 'gited-tests)
;;; gited-tests.el ends here
