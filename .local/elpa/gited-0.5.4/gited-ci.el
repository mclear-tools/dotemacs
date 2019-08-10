;;; gited-ci.el --- Obtain CI status of the trunk branch  -*- lexical-binding:t -*-
;;
;; Filename: gited-ci.el
;; Description: Obtain CI status of the trunk branch
;;
;; Author: Tino Calancha <tino.calancha@gmail.com>
;; Maintainer: Tino Calancha <tino.calancha@gmail.com>
;; URL: https://github.com/calancha/Gited
;; Copyright (C) 2016-2018 Free Software Foundation, Inc.
;;
;;;
;;  Internal variables defined here:
;;
;;   `gited-last-trunk-commit', `gited-trunk-ci-status',
;;   `gited-trunk-ci-status-fail-face', `gited-trunk-ci-status-pending-face',
;;   `gited-trunk-ci-status-running-face',
;;   `gited-trunk-ci-status-success-face',
;;   `gited-trunk-ci-status-unknown-face'.
;;
;;  Coustom variables defined here:
;;
;;   `gited-show-trunk-ci-status'.
;;
;;  Non-interactive functions defined here:
;;
;;   `gited--show-trunk-ci-status', `gited--trunk-ci-status',
;;   `gited-parse-ci-status', `gited-pull-callback',
;;   `gited-trunk-ci-last-commit-uri', `gited-trunk-ci-status'.
;;
;;  Faces defined here:
;;
;;   `gited-trunk-ci-status-fail', `gited-trunk-ci-status-pending',
;;   `gited-trunk-ci-status-running', `gited-trunk-ci-status-success',
;;   `gited-trunk-ci-status-unknown'.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Show the CI status of the trunk branch in the Gited buffer.
;; Different status displays the commit line with different
;; faces.  For instance, green if all the test are OK and red
;; if any of them fails.
;;

;;; Code:



;; TODO: Support more CI services.
(defcustom gited-show-trunk-ci-status nil
  "Show CI status for the last commit in the trunk branch.
A list of elements (TOPLEVEL_DIR . CI-URI).
TOPLEVEL_DIR is the toplevel directory for the local Git repository.
CI_URI is the URI to access the Continous Integration system.

Supported CI are Gitlab, Travis and CircleCI: for Gitlab, you need to provide
all but the commit hash, for instance, in the case of the Emacs Gitlab CI,
the value is
`https://gitlab.com/emacs-ci/emacs/commit/'.
For Travis, the format is as follows:
`https://api.travis-ci.org/calancha/Gited.svg?branch=master'.
For circleci:
`https://circleci.com/gh/calancha/foo/tree/master'."
  :type '(repeat
          (choice
           (cons :tag "Show trunk CI status"
                 (string :tag "gited-toplevel-dir" :value "")
                 (string :tag "gited-ci-uri" :value ""))))
  :group 'gited)

(defvar-local gited-trunk-ci-status nil
  "Status of the last commit in the trunk branch for the CI.")
(put 'gited-trunk-ci-status 'permanent-local t)

(defface gited-trunk-ci-status-fail
  '((t (:foreground "Red")))
  "Face for trunk branch with last commit fail in the CI."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-trunk-ci-status-fail-face 'gited-trunk-ci-status-fail)

(defface gited-trunk-ci-status-running
  '((t (:foreground "LightSkyBlue")))
  "Face for trunk branch with last commit running in the CI."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-trunk-ci-status-running-face 'gited-trunk-ci-status-running)

(defface gited-trunk-ci-status-success
  '((((background dark)) (:foreground "green"))
    (t                   (:foreground "white" :background "forest green")))
  "Face for trunk branch with last commit succeded in the CI."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-trunk-ci-status-success-face 'gited-trunk-ci-status-success)

(defface gited-trunk-ci-status-unknown
  '((((background dark)) (:foreground "orange"))
    (t                   (:foreground "black")))
  "Face for trunk branch with last commit status in the CI unknown."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-trunk-ci-status-unknown-face 'gited-trunk-ci-status-unknown)

(defface gited-trunk-ci-status-pending
  '((((background dark)) (:foreground "hotpink"))
    (t                   (:foreground "deeppink")))
  "Face for trunk branch with last commit status in the CI pending."
  :group 'gited :group 'font-lock-highlighting-faces)
(defvar gited-trunk-ci-status-pending-face 'gited-trunk-ci-status-pending)

(defun gited-parse-ci-status (&rest args)
  "Parse the status of the trunk last commit in the Gitlab CI.
`gited-buffer' is passed as the last element of ARGS list."
  (goto-char 1)
  (let* ((toplevel-dir (with-current-buffer (car (last args))
                         gited-toplevel-dir))
         (ci-uri (cdr (assoc toplevel-dir gited-show-trunk-ci-status)))
         (success-regexp
          (cond ((string-match "gitlab" ci-uri) "ci-status-icon-success")
                ((string-match "\\(travis-ci\\)\\|\\(circleci\\)" ci-uri) "passing")))
         (failed-regexp
          (cond ((string-match "gitlab" ci-uri) "ci-status-icon-failed")
                ((string-match "travis-ci" ci-uri) "failed")
                ((string-match "circleci" ci-uri) "failing")))
         (pending-regexp
          (cond ((string-match "gitlab" ci-uri) "ci-status-icon-pending")
                ((string-match "travis-ci" ci-uri) "pending")
                ((string-match "circleci" ci-uri) "pending")))
         (running-regexp
          (cond ((string-match "gitlab" ci-uri) "ci-status-icon-running")
                ((string-match "\\(travis-ci\\)\\|\\(circleci\\)" ci-uri) "running"))) ; This one always fail
         (ci-status
          (cond ((save-excursion
                   (re-search-forward success-regexp nil t))
                 'success)
                ((save-excursion
                   (re-search-forward failed-regexp nil t))
                 'failed)
                ((save-excursion
                   (re-search-forward running-regexp nil t))
                 'running)
                ((save-excursion
                   (re-search-forward pending-regexp nil t))
                 'pending)
                (t 'unknown))))
    (message "Parse CI status done!")
    ;; Show the staus in the Gited buffer.
    (with-current-buffer (car (last args))
      (setq gited-trunk-ci-status ci-status)
      (gited--show-trunk-ci-status))))

(declare-function gited--last-trunk-commit "gited.el")

(defun gited-trunk-ci-last-commit-uri ()
  (let ((ci-uri (cdr (assoc gited-toplevel-dir gited-show-trunk-ci-status))))
    (cond ((string-match "\\(travis-ci\\)\\|\\(circleci\\)" ci-uri) ci-uri)
          ((string-match "gitlab" ci-uri)
           (format "%s%s" ci-uri (gited--last-trunk-commit)))
          (t (user-error "Dont know this CI service uri '%s'" ci-uri)))))

(defun gited--trunk-ci-status ()
  "Return the status of the Gitlab CI for the last commit in the trunk branch."
  (let ((url (gited-trunk-ci-last-commit-uri)))
    (url-retrieve url 'gited-parse-ci-status (list gited-buffer))))
    
(defun gited-trunk-ci-status ()
  "Return the status of the CI for the last commit in the trunk branch."
  (unless (derived-mode-p 'gited-mode) (user-error "Not a Gited buffer"))
  (gited--trunk-ci-status))

(defun gited--show-trunk-ci-status ()
  (save-excursion
    (gited-goto-branch (gited-trunk-branch))
    (gited--move-to-column (1+ gited-commit-idx))
    (let ((inhibit-read-only t))
      (let* ((start (point))
             (end (point-at-eol))
             (status-face
              (cond ((eq gited-trunk-ci-status 'success)
                     'gited-trunk-ci-status-success)
                    ((eq gited-trunk-ci-status 'failed)
                     'gited-trunk-ci-status-fail)
                    ((eq gited-trunk-ci-status 'running)
                     'gited-trunk-ci-status-running)
                    ((eq gited-trunk-ci-status 'pending)
                     'gited-trunk-ci-status-pending)
                    (t
                     'gited-trunk-ci-status-unknown))))
        (put-text-property start end 'face status-face)
        (put-text-property
         start end 'help-echo (format "CI status %S: %s"
                                      gited-trunk-ci-status
                                      (gited-trunk-ci-last-commit-uri)))))))


(defvar-local gited-last-trunk-commit "" "Last commit hash in trunk branch.")
(put 'gited-last-trunk-commit 'permanent-local t)

;; Update trunk CI status if `gited-show-trunk-ci-status' is non-nil
;; and we have fetched new commits from the trunk branch.
(defun gited-pull-callback ()
  "Run `gited-trunk-ci-status' after remote fetching or reverting buffer."
  (when (and gited-show-trunk-ci-status
             (car-safe (assoc gited-toplevel-dir gited-show-trunk-ci-status))
             (equal (directory-file-name (car (assoc gited-toplevel-dir gited-show-trunk-ci-status)))
                    (directory-file-name gited-toplevel-dir)))
    (let ((last-trunk-commit (gited--last-trunk-commit)))
      (if (equal gited-last-trunk-commit last-trunk-commit)
          (gited--show-trunk-ci-status)
        (setq gited-last-trunk-commit last-trunk-commit)
        (gited-trunk-ci-status)))))


(provide 'gited-ci)
;;; gited-ci.el ends here
