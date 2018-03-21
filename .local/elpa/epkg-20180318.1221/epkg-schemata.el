;;; epkg-schemata.el --- table schemata             -*- lexical-binding: t -*-

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

;;; Commentary:

;; This file is only needed to bootstrap the Epkg database.  Since
;; that has already been done, it mainly serves as documentation.

;;; Code:

(require 'epkg)

(defconst epkg--db-table-schemata
  '((packages          [(class :not-null)
                        (name :not-null :primary-key)
                        hash
                        url
                        mirror-url
                        mirror-name
                        upstream-user
                        upstream-name
                        upstream-branch
                        upstream-tree
                        library
                        repopage
                        homepage
                        mirrorpage
                        wikipage
                        license
                        created
                        updated
                        summary
                        commentary
                        libraries
                        provided
                        required
                        keywords
                        authors
                        maintainers
                        ;; The schema dsl doesn't properly handle
                        ;; `:default', so `closql--db-init' adds
                        ;; these columns using `:alter-table'.
                        ;; (melpa-recipes :default eieio-unbound)
                        ;; (gelpa-recipes :default eieio-unbound)
                        ;; (builtin-libraries :default eieio-unbound)
                        ;; patched
                        ;; stars
                        ;; downloads
                        ])
    (libraries         [(package :not-null)
                        (library :not-null)]
                       (:primary-key [package library])
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    (provided          [(package :not-null)
                        (feature :not-null)
                        drop
                        join]
                       (:primary-key [package feature])
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    (required          [(package :not-null)
                        (feature :not-null)
                        hard
                        ease
                        drop]
                       (:primary-key [package feature])
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    (keywords          [(package :not-null)
                        (keyword :not-null)]
                       (:primary-key [package keyword])
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    (authors           [(package :not-null)
                        name
                        email]
                       (:primary-key [package name email])
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    (maintainers       [(package :not-null)
                        name
                        email]
                       (:primary-key [package name email])
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    (melpa-recipes     [(class :not-null)
                        (name :not-null :primary-key)
                        url
                        repo
                        repopage
                        files
                        branch
                        commit
                        module
                        version-regexp
                        old-names
                        epkg-package]
                       (:foreign-key
                        [epkg-package] :references packages [name]
                        :on-delete :set-null))
    (gelpa-recipes     [(class :not-null)
                        (name :not-null :primary-key)
                        url method released epkg-package]
                       (:foreign-key
                        [epkg-package] :references packages [name]
                        :on-delete :set-null))
    (builtin-libraries [(package :not-null)
                        (library :not-null)
                        feature]
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    ;; The above tables are linked to the `epkg-package' class.
    ;; The below tables are only accessible using `epkg-sql'.
    (pkg-homepages     [(package :not-null :primary-key)
                        (page :not-null)])
    (pkg-wikipages     [(package :not-null :primary-key)
                        (page :not-null)])
    (raw-wikipages     [(page :not-null :primary-key)
                        (normalized :not-null)])
    ))

(cl-defmethod closql--db-init ((db epkg-database))
  (emacsql-enable-debugging db)
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema)
                   epkg--db-table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (let ((add-column [:alter-table packages :add-column $i1 :default $s2]))
      (emacsql db add-column 'melpa-recipes     'eieio-unbound)
      (emacsql db add-column 'gelpa-recipes     'eieio-unbound)
      (emacsql db add-column 'builtin-libraries 'eieio-unbound)
      (emacsql db add-column 'patched           nil)
      (emacsql db add-column 'stars             nil)
      (emacsql db add-column 'downloads         nil)
      )))

;;; _
(provide 'epkg-schemata)
;;; epkg-schemata.el ends here
