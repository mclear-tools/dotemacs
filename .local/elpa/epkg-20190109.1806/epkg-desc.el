;;; epkg-desc.el --- show Epkg descriptions       -*- lexical-binding: t -*-

;; Copyright (C) 2016-2019  Jonas Bernoulli

;; This file contains code from GNU Emacs, which is
;; Copyright (C) 1976-2016 Free Software Foundation, Inc.

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
(require 'find-func)

;;; Options

(defconst epkg--custom-slot-choices
  (nconc (list (list 'const 'type)
               (list 'const 'class))
         (--map (list 'const (cl--slot-descriptor-name it))
                (eieio-class-slots (cl--find-class 'epkg-package)))))

(defcustom epkg-describe-package-slots
  '(epkg-insert-unsafe-warning
    summary
    epkg-insert-homepage
    epkg-insert-repopage
    epkg-insert-mirrorpage
    nil
    type
    license
    updated
    stars
    downloads
    epkg-insert-authors
    epkg-insert-maintainers
    nil
    epkg-insert-provided
    epkg-insert-keywords
    epkg-insert-commentary
    epkg-insert-dependencies
    epkg-insert-reverse-dependencies)
  "Slots that are displayed when describing an Epkg package.

The value is a list.  Each element can be a slot symbol, a
function, or nil.  Functions are called with one argument, the
Epkg object.  They should insert something at point.  Raw slot
symbols cause its non-nil value to be inserted as-is.  If a
slot's value is nil, then nothing is inserted.  Elements that
are nil stand for empty lines."
  :group 'epkg
  :type `(repeat
          (choice :format "%[Value Menu%] %v"
                  (const :tag " Newline" nil)
                  (choice :tag "Function"
                          (const epkg-insert-homepage)
                          (const epkg-insert-repopage)
                          (const epkg-insert-wikipage)
                          (const epkg-insert-mirrorpage)
                          (const epkg-insert-authors)
                          (const epkg-insert-maintainers)
                          (const epkg-insert-provided)
                          (const epkg-insert-keywords)
                          (const epkg-insert-commentary)
                          (const epkg-insert-dependencies)
                          (const epkg-insert-reverse-dependencies)
                          (function :tag "Other"))
                  (choice :tag "    Slot" ,@epkg--custom-slot-choices))))

(defcustom epkg-describe-package-slots-width 12
  "Display width of Epkg slots in Epkg help."
  :group 'epkg
  :type 'integer)

(defface epkg-help-slot
  '((t :inherit (bold font-lock-function-name-face)))
  "Face used for slot names when describing an Epkg package."
  :group 'epkg)

(defface epkg-help-name
  '((t :height 1.6))
  "Face used for the name of the described Epkg package."
  :group 'epkg)

;;; Commands

;;;###autoload
(defun epkg-describe-package (package)
  "Display the full documentation of PACKAGE."
  (interactive (list (epkg-read-package "Describe package: ")))
  (help-setup-xref (list #'epkg-describe-package package)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (epkg-describe-package-1 (epkg package)))))

(defun epkg-list-describe-package (&optional _button)
  "Display the full documentation of the package on the current line."
  (interactive)
  (if-let ((package (tabulated-list-get-id)))
      (epkg-describe-package package)
    (call-interactively 'epkg-describe-package)))

;;; Inserters

(defun epkg-describe-package-1 (pkg &optional slots)
  (with-slots (name commentary) pkg
    (insert (propertize (capitalize (oref pkg name)) 'face 'epkg-help-name))
    (insert " is a ")
    (insert (pcase (eieio-object-class pkg)
              ('epkg-builtin-package "built-in")
              ('epkg-shelved-package "shelved")
              (_ "mirrored")))
    (insert " package.\n\n")
    (dolist (slot (or slots epkg-describe-package-slots))
      (unless (= (char-before) ?\n)
        (insert ?\n))
      (cl-typecase slot
        (null (insert ?\n))
        (function (funcall slot pkg))
        (t (--when-let (if (eq slot 'type)
                           (closql--abbrev-class (eieio-object-class pkg))
                         (slot-value pkg slot))
             (epkg--insert-slot slot)
             (insert (format "%s\n" it))))))))

(defun epkg--insert-slot (slot)
  (insert (format (format "%%%ss: " epkg-describe-package-slots-width)
                  (propertize (capitalize (symbol-name slot))
                              'face 'epkg-help-slot))))

(defun epkg-insert-person (value)
  (indent-to (+ epkg-describe-package-slots-width 2))
  (-let [(name email) value]
    (when name
      (insert-button name 'type 'epkg-author 'help-args (list name)))
    (when email
      (when name
        (insert " "))
      (insert "<")
      (insert-button email 'type 'epkg-email 'help-args
                     (list (format "%s <%s>" name email)))
      (insert ">")))
  (insert "\n"))

(defun epkg-insert-authors (pkg)
  (--when-let (oref pkg authors)
    (epkg--insert-slot 'authors)
    (mapc #'epkg-insert-person it)))

(defun epkg-insert-maintainers (pkg)
  (--when-let (oref pkg maintainers)
    (epkg--insert-slot 'maintainers)
    (mapc #'epkg-insert-person it)))

(defun epkg-insert-keywords (pkg)
  (--when-let (oref pkg keywords)
    (epkg--insert-slot 'keywords)
    (while it
      (let ((symbol (pop it)))
        (insert-button (symbol-name symbol)
                       'type 'epkg-keyword 'help-args (list symbol))
        (when it (insert ", "))))
    (insert ?\n)))

(defun epkg-insert-homepage (pkg)
  (--when-let (oref pkg homepage)
    (epkg--insert-slot 'homepage)
    (insert-button it 'type 'help-url 'help-args (list it))
    (insert ?\n)))

(defun epkg-insert-repopage (pkg)
  (--when-let (oref pkg repopage)
    (epkg--insert-slot 'repopage)
    (insert-button it 'type 'help-url 'help-args (list it))
    (insert ?\n)))

(defun epkg-insert-wikipage (pkg)
  (--when-let (oref pkg wikipage)
    (epkg--insert-slot 'wikipage)
    (insert-button it 'type 'help-url 'help-args (list it))))

(defun epkg-insert-mirrorpage (pkg)
  (--when-let (oref pkg mirrorpage)
    (epkg--insert-slot 'mirrorpage)
    (insert-button it 'type 'help-url 'help-args (list it))))

(defun epkg-insert-commentary (pkg)
  (--when-let (oref pkg commentary)
    (insert ?\n it)))

(defun epkg-insert-provided (pkg)
  (-when-let (provided (oref pkg provided))
    (epkg--insert-slot 'provided)
    (require 'find-func)
    (while provided
      (-let* (((library drop join) (pop provided))
              (face (cond (drop 'font-lock-warning-face)
                          (join 'font-lock-constant-face)
                          (t    'default))))
        (setq library (symbol-name library))
        (when (> (+ (- (point) (line-beginning-position)) (length library) 2)
                 (window-width))
          (insert ?\n)
          (insert (make-string (+ epkg-describe-package-slots-width 2) ?\s)))
        (if (ignore-errors (find-library-name library))
            (insert-button library
                           'type 'epkg-library
                           'help-args (list library)
                           'face (list 'button face))
          (insert (propertize library 'face face)))
        (when provided
          (insert ", "))))
    (insert ?\n)))

(defun epkg-insert-dependencies (pkg)
  (require 'tree-widget)
  (when (oref pkg required)
    (insert ?\n)
    (widget-create
     (list 'epkg-dependency-tree
           :get-dependencies 'epkg-required
           :open t
           :node (list 'epkg-dependency-node
                       :value (list pkg)
                       :format "%{Dependencies:%}\n"
                       :sample-face 'epkg-help-slot)))))

(defun epkg-insert-reverse-dependencies (pkg)
  (require 'tree-widget)
  (when (epkg-reverse-dependencies pkg)
    (insert ?\n)
    (widget-create
     (list 'epkg-dependency-tree
           :get-dependencies 'epkg-reverse-dependencies
           :open t
           :node (list 'epkg-dependency-node
                       :value (list pkg)
                       :format "%{Reverse dependencies:%}\n"
                       :sample-face 'epkg-help-slot)))))

(defun epkg-insert-unsafe-warning (pkg)
  (cond
   ((epkg-wiki-package-p pkg)
    (insert
     (propertize
      (concat
       "WARNING: Anyone can edit any packages on the Emacswiki by\n"
       "         design, making it trivial to inject malicious code.\n\n")
      'face 'error)))
   ((epkg-orphaned-package-p pkg)
    (insert
     (propertize
      (concat
       "WARNING: The Emacsorphanage might import this package over an\n"
       "         insecure connection, in which case an attacker could\n"
       "         inject malicious code.\n\n")
      'face 'warning)))
   ((epkg-shelved-package-p pkg)
    (insert
     (propertize
      (concat
       "WARNING: This shelved package might have been imported over an\n"
       "         insecure connection or from an insecure source before\n"
       "         it was moved to the Emacsattic.\n\n")
      'face 'warning)))
   ((string-match-p "\\`\\(http\\|git\\)://" (oref pkg url))
    (insert
     (propertize
      (concat
       "WARNING: This package is being mirrored over an insecure\n"
       "         connection.  An attacker could inject malicious code.\n\n")
      'face 'warning)))))

;;; Buttons

(define-button-type 'epkg-menu-package
  :supertype 'help-xref
  'help-function 'epkg-menu-describe-package
  'help-echo (purecopy "mouse-2, RET: View package"))

(define-button-type 'epkg-revision
  :supertype 'help-xref
  'help-function 'epkg-describe-package
  'help-echo (purecopy "mouse-2, RET: View this revision"))

(define-button-type 'epkg-keyword
  :supertype 'help-xref
  'help-function 'epkg-list-keyworded-packages
  'help-echo (purecopy "mouse-2, RET: List keyworded packages"))

(define-button-type 'epkg-package
  :supertype 'help-xref
  'help-function 'epkg-describe-package
  'help-echo (purecopy "mouse-2, RET: View package"))

(define-button-type 'epkg-library
  :supertype 'help-xref
  'help-function 'find-library
  'help-echo (purecopy "mouse-2, RET: View library"))

(define-button-type 'epkg-author
  :supertype 'help-xref
  'help-function 'epkg-list-packages-by-author
  'help-echo (purecopy "mouse-2, RET: List packages by author"))

(define-button-type 'epkg-email
  :supertype 'help-url
  'help-function #'compose-mail
  'help-echo (purecopy "mouse-2, RET: Compose mail"))

;;; Widgets

(declare-function widget-default-format-handler "wid-edit" (_widget escape))
(declare-function widget-type "wid-edit" (widget))

(define-widget 'epkg-dependency-tree 'tree-widget
  "The Epkg Dependency Tree widget."
  :expander 'epkg-dependency-tree-expander)

(defun epkg-dependency-tree-expander (widget)
  (let ((node (widget-get widget :node))
        (getter (widget-get widget :get-dependencies)))
    (-when-let (pkg (car (widget-get node :value)))
      (-map (-lambda ((name . features))
              (list 'epkg-dependency-tree
                    :get-dependencies getter
                    :node (list (widget-type node)
                                :value (cons (and name (epkg name)) features))))
            (funcall getter pkg)))))

(define-widget 'epkg-dependency-node 'default
  "The Epkg Dependency Node widget."
  :format "%P %T %H\n"
  :format-handler 'epkg-dependency-node-format-handler
  :value-get 'widget-value-value-get
  :keymap widget-keymap)

(defun epkg-dependency-node-format-handler (widget escape)
  (-let [(pkg . features) (widget-get widget :value)]
    (pcase escape
      (?P (if pkg
              (insert-button (oref pkg name) 'type 'epkg-package
                             'face (and (-any #'symbolp features) 'bold)
                             'help-args (list (oref pkg name)))
            (insert (propertize "unknown" 'face
                                (if (symbolp (car features))
                                    (list 'font-lock-warning-face 'bold)
                                  'font-lock-warning-face)))))
      (?T (insert (propertize " " 'display '(space :align-to 30)))
          (insert (if pkg
                      (let ((abbrev (symbol-name
                                     (closql--abbrev-class
                                      (eieio-object-class pkg)))))
                        (if (epkg-shelved-package-p pkg)
                            (propertize abbrev 'face 'font-lock-warning-face)
                          abbrev))
                    (propertize "unknown" 'face 'font-lock-warning-face))))
      (?H (insert (propertize " " 'display '(space :align-to 43)))
          (while features
            (let (hard (library (pop features)))
              (when (symbolp library)
                (setq hard t)
                (setq library (symbol-name library)))
              (if (ignore-errors (find-library-name library))
                  (insert-button library
                                 'type 'epkg-library
                                 'help-args (list library)
                                 'face (and hard 'bold))
                (insert library)))
            (when features (insert ", "))))
      (_  (widget-default-format-handler widget escape)))))

(provide 'epkg-desc)
;;; epkg-desc.el ends here
