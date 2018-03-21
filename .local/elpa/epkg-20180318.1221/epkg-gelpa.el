;;; epkg-gelpa.el --- GNU Elpa recipes            -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018  Jonas Bernoulli

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

(defclass gelpa-recipe (closql-object)
  ((closql-table         :initform gelpa-recipes)
   (closql-primary-key   :initform name)
   (closql-foreign-key   :initform epkg-package)
   (closql-foreign-table :initform packages)
   (closql-class-prefix  :initform "gelpa-")
   (closql-class-suffix  :initform "-recipe")
   (name                 :initform nil :initarg :name)
   (url                  :initform nil)
   (method               :initform nil)
   (released             :initform nil)
   (epkg-package         :initform nil))
  :abstract t)

(defclass gelpa-builtin-recipe (gelpa-recipe) ())
(defclass gelpa-subtree-recipe (gelpa-recipe) ())
(defclass gelpa-external-recipe (gelpa-recipe) ())

(defun gelpa-recipes (&optional select predicates)
  (closql-query (epkg-db) select predicates 'gelpa-recipe))

(defun gelpa-get (name)
  (closql-get (epkg-db) name 'gelpa-recipe))

;;; _
(provide 'epkg-gelpa)
;;; epkg-gelpa.el ends here

