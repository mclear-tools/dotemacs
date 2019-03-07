;;; epkg-org.el --- various Org mode utilities    -*- lexical-binding: t -*-

;; Copyright (C) 2016-2019  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU GPL see http://www.gnu.org/licenses.

;;; Commentary:

;; Epkg itself does not use the utilities defined here.  They are
;; only intended for third-party packages and the Emir tools used
;; to generate reports about the information in the Epkg database.

;;; Code:

(require 'epkg)

(defmacro epkg-with-org-header (header &rest body)
  (declare (indent defun))
  `(when-let (rows (progn ,@body))
     (let ((header ',header)
           (n 0) prev)
       (dolist (row rows)
         (unless (equal (car row) prev)
           (cl-incf n))
         (setq prev (car row)))
       (append (list (cons (format "%s (%s)" (car header) n)
                           (cdr header)))
               (list 'hline)
               (--map (--map (or it "") it) rows)))))

(defun epkg-org-link (name)
  (let ((pkg (epkg name)))
    (if-let (repopage (oref pkg repopage))
        (format "[[%s][%s/%s]]" repopage
                (oref pkg upstream-user)
                (oref pkg upstream-name))
      (when-let (homepage (oref pkg homepage))
        (format "[[%s]]" homepage)))))

(defun melpa-org-link (name)
  (let ((rcp (melpa-get name)))
    (when-let (repopage (oref rcp repopage))
      (format "[[%s][%s]]" repopage (oref rcp repo)))))

;;; _
(provide 'epkg-org)
;;; epkg-org.el ends here

