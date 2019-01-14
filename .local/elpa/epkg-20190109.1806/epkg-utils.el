;;; epkg-utils.el --- auxiliary commands and utilities  -*- lexical-binding: t -*-

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

;;; Code:

(require 'epkg)

;;; Submodule Utilities

(defmacro with-epkg-repository (arg &rest body)
  "Evaluate BODY in the repository specified by ARG.
Determine the repository by calling function `epkg-repository'
with ARG as only argument.  When ARG is t then evaluate in the
repository specified by variable `epkg-repository'."
  (declare (indent defun))
  `(let ((default-directory
           ,(if (eq arg t)
                'epkg-repository
              `(or (epkg-repository ,arg)
                   (error "Need package or string")))))
     ,@body))

(cl-defgeneric epkg-repository (arg)
  "Return the repository specified by ARG.")

(cl-defmethod epkg-repository ((pkg epkg-mirrored-package))
  "For a mirrored package the repository is located below \"mirror/\",
inside the super-repository specified by `epkg-repository'."
  (expand-file-name (format "mirror/%s/" (oref pkg name)) epkg-repository))

(cl-defmethod epkg-repository ((pkg epkg-shelved-package))
  "For a shelved package the repository is located below \"attic/\",
inside the super-repository specified by `epkg-repository'."
  (expand-file-name (format "attic/%s/" (oref pkg name)) epkg-repository))

;;; Find-File Commands

;;;###autoload
(defun epkg-find-file (filename &optional wildcards)
  "Visit a file in a submodule of `epkg-repository'."
  (declare (interactive-only find-file))
  (interactive (epkg-find-file-read-args "Find file: "))
  (epkg-find-file-noselect filename #'switch-to-buffer wildcards))

;;;###autoload
(defun epkg-find-file-other-window (filename &optional wildcards)
  "Visit a file in a submodule of `epkg-repository' in another window."
  (declare (interactive-only find-file-other-window))
  (interactive (epkg-find-file-read-args "Find file in other window: "))
  (epkg-find-file-noselect filename #'switch-to-buffer-other-window wildcards))

;;;###autoload
(defun epkg-find-file-other-frame (filename &optional wildcards)
  "Visit a file in a submodule of `epkg-repository' in another frame."
  (declare (interactive-only find-file-other-frame))
  (interactive (epkg-find-file-read-args "Find file in other frame: "))
  (epkg-find-file-noselect filename #'switch-to-buffer-other-frame wildcards))

(defun epkg-find-file-read-args (prompt)
  (let* ((pkg  (epkg (epkg-read-package "Find file of package: ")))
         (repo (epkg-repository pkg)))
    (unless (file-exists-p (expand-file-name ".git" repo))
      (if (y-or-n-p (format "Submodule %s isn't checked out.  Check out?"
                            default-directory))
          (epkg--call-git "submodule" "update" "--init" repo)
        (user-error "Abort")))
    (let ((default-directory repo))
      (find-file-read-args prompt (confirm-nonexistent-file-or-buffer)))))

(defun epkg-find-file-noselect (filename:s switch &optional wildcards)
  (let ((value (find-file-noselect filename:s nil nil wildcards)))
    (if (listp value)
	(let ((buffers (nreverse value)))
	  (funcall switch (car buffers))
	  (mapc #'switch-to-buffer (cdr buffers))
	  buffers)
      (funcall switch value))))

;;; _
(provide 'epkg-utils)
;;; epkg-utils.el ends here

