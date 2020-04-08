;;; org-roam.el --- Roam Research replica with Org-mode -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.0.0-rc1
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite "1.0.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library is an attempt at injecting Roam functionality into Org-mode.
;; This is achieved primarily through building caches for forward links,
;; backward links, and file titles.
;;
;;
;;; Code:
;;;; Library Requires
(require 'org)
(require 'org-element)
(require 'ob-core)  ;for org-babel-parse-header-arguments
(require 'subr-x)
(require 'dash)
(require 's)
(require 'f)
(require 'cl-lib)
;;;; org-roam features
(require 'org-roam-macs)
(require 'org-roam-db)
(require 'org-roam-capture)
(require 'org-roam-graph)
(require 'org-roam-completion)

;; To detect cite: links
(require 'org-ref nil t)

;;;; Customizable Variables
(defgroup org-roam nil
  "Roam Research replica in Org-mode."
  :group 'org
  :prefix "org-roam-"
  :link '(url-link :tag "Github" "https://github.com/jethrokuan/org-roam")
  :link '(url-link :tag "Online Manual" "https://org-roam.readthedocs.io/"))

(defgroup org-roam-faces nil
  "Faces used by Org-roam."
  :group 'org-roam
  :group 'faces)

(defcustom org-roam-directory (expand-file-name "~/org-roam/")
  "Default path to Org-roam files.

All Org files, at any level of nesting, is considered part of the Org-roam."
  :type 'directory
  :group 'org-roam)

(defcustom org-roam-buffer-position 'right
  "Position of `org-roam' buffer.
Valid values are
 * left,
 * right,
 * top,
 * bottom."
  :type '(choice (const left)
                 (const right)
                 (const top)
                 (const bottom))
  :group 'org-roam)

(defcustom org-roam-link-title-format "%s"
  "The formatter used when inserting Org-roam links that use their title.
Formatter may be a function that takes title as its only argument."
  :type '(choice
          (string :tag "String Format" "%s")
          (function :tag "Custom function"))
  :group 'org-roam)

(defcustom org-roam-buffer-width 0.33
  "Width of `org-roam' buffer.
Has an effect if and only if `org-roam-buffer-position' is `left' or `right'."
  :type 'number
  :group 'org-roam)

(defcustom org-roam-buffer-height 0.27
  "Height of `org-roam' buffer.
Has an effect if and only if `org-roam-buffer-position' is `top' or `bottom'."
  :type 'number
  :group 'org-roam)

(defcustom org-roam-buffer "*org-roam*"
  "Org-roam buffer name."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-encrypt-files nil
  "Whether to encrypt new files.  If true, create files with .org.gpg extension."
  :type 'boolean
  :group 'org-roam)

;;;; Dynamic variables
(defvar org-roam--current-buffer nil
  "Currently displayed file in `org-roam' buffer.")

(defvar org-roam-last-window nil
  "Last window `org-roam' was called from.")

(defcustom org-roam-verbose t
  "Echo messages that are not errors."
  :type 'boolean
  :group 'org-roam)

;;; Utilities
;;;; General Utilities
(defun org-roam--plist-to-alist (plist)
  "Return an alist of the property-value pairs in PLIST."
  (let (res)
    (while plist
      (let ((prop (intern (substring (symbol-name (pop plist)) 1 nil)))
            (val (pop plist)))
        (push (cons prop val) res)))
    res))

(defun org-roam--aliases-str-to-list (str)
  "Function to transform string STR into list of alias titles.

This snippet is obtained from ox-hugo:
https://github.com/kaushalmodi/ox-hugo/blob/a80b250987bc770600c424a10b3bca6ff7282e3c/ox-hugo.el#L3131"
  (when (stringp str)
    (let* ((str (org-trim str))
           (str-list (split-string str "\n"))
           ret)
      (dolist (str-elem str-list)
        (let* ((format-str ":dummy '(%s)") ;The :dummy key is discarded in the `lst' var below.
               (alist (org-babel-parse-header-arguments (format format-str str-elem)))
               (lst (cdr (car alist)))
               (str-list2 (mapcar (lambda (elem)
                                    (cond
                                     ((symbolp elem)
                                      (symbol-name elem))
                                     (t
                                      elem)))
                                  lst)))
          (setq ret (append ret str-list2))))
      ret)))

;;;; File functions and predicates
(defun org-roam--touch-file (path)
  "Touches an empty file at PATH."
  (make-directory (file-name-directory path) t)
  (f-touch path))

(defun org-roam--file-name-extension (filename)
  "Return file name extension for FILENAME.
Like `file-name-extension', but does not strip version number."
  (save-match-data
    (let ((file (file-name-nondirectory filename)))
      (if (and (string-match "\\.[^.]*\\'" file)
               (not (eq 0 (match-beginning 0))))
          (substring file (+ (match-beginning 0) 1))))))

(defun org-roam--org-file-p (path)
  "Check if PATH is pointing to an org file."
  (let ((ext (org-roam--file-name-extension path)))
    (or (string= ext "org")
        (and
         (string= ext "gpg")
         (string= (org-roam--file-name-extension (file-name-sans-extension path)) "org")))))

(defun org-roam--org-roam-file-p (&optional file)
  "Return t if FILE is part of Org-roam system, nil otherwise.
If FILE is not specified, use the current buffer's file-path."
  (let ((path (or file
                  (buffer-file-name))))
    (and path
         (org-roam--org-file-p path)
         (f-descendant-of-p (file-truename path)
                            (file-truename org-roam-directory)))))

(defun org-roam--list-files (dir)
  "Return all Org-roam files located within DIR, at any nesting level.
Ignores hidden files and directories."
  (if (file-exists-p dir)
      (let ((files (directory-files dir t "." t))
            (dir-ignore-regexp (concat "\\(?:"
                                       "\\."
                                       "\\|\\.\\."
                                       "\\)$"))
            result)
        (dolist (file files)
          (cond
           ((file-directory-p file)
            (unless (string-match dir-ignore-regexp file)
              (setq result (append (org-roam--list-files file) result))))
           ((and (file-readable-p file)
                 (org-roam--org-file-p file))
            (setq result (cons (file-truename file) result)))))
        result)))

(defun org-roam--list-all-files ()
  "Return a list of all Org-roam files within `org-roam-directory'."
  (org-roam--list-files (file-truename org-roam-directory)))

;;;; Org extraction functions
(defun org-roam--extract-global-props (props)
  "Extract PROPS from the current org buffer.
The search terminates when the first property is encountered."
  (let ((buf (org-element-parse-buffer))
        res)
    (dolist (prop props)
      (let ((p (org-element-map buf 'keyword
                 (lambda (kw)
                   (when (string= (org-element-property :key kw) prop)
                     (org-element-property :value kw)))
                 :first-match t)))
        (push (cons prop p) res)))
    res))

(defun org-roam--extract-links (&optional file-path)
  "Extracts all link items within the current buffer.
Link items are of the form:

    [from to type properties]

This is the format that emacsql expects when inserting into the database.
FILE-FROM is typically the buffer file path, but this may not exist, for example
in temp buffers.  In cases where this occurs, we do know the file path, and pass
it as FILE-PATH."
  (let ((file-path (or file-path
                       (file-truename (buffer-file-name)))))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (let* ((type (org-element-property :type link))
               (path (org-element-property :path link))
               (start (org-element-property :begin link))
               (link-type (cond ((and (string= type "file")
                                      (org-roam--org-file-p path))
                                 "roam")
                                ((string= type "cite")
                                 "cite")
                                (t nil))))
          (when link-type
            (goto-char start)
            (let* ((element (org-element-at-point))
                   (begin (or (org-element-property :content-begin element)
                              (org-element-property :begin element)))
                   (content (or (org-element-property :raw-value element)
                                (buffer-substring
                                 begin
                                 (or (org-element-property :content-end element)
                                     (org-element-property :end element)))))
                   (content (string-trim content)))
              (vector file-path
                      (cond ((string= link-type "roam")
                             (file-truename (expand-file-name path (file-name-directory file-path))))
                            ((string= link-type "cite")
                             path))
                      link-type
                      (list :content content :point begin)))))))))

(defun org-roam--extract-titles ()
  "Extract the titles from current buffer.
Titles are obtained via the #+TITLE property, or aliases
specified via the #+ROAM_ALIAS property."
  (let* ((props (org-roam--extract-global-props '("TITLE" "ROAM_ALIAS")))
         (aliases (cdr (assoc "ROAM_ALIAS" props)))
         (title (cdr (assoc "TITLE" props)))
         (alias-list (org-roam--aliases-str-to-list aliases)))
    (if title
        (cons title alias-list)
      alias-list)))

(defun org-roam--extract-ref ()
  "Extract the ref from current buffer."
  (cdr (assoc "ROAM_KEY" (org-roam--extract-global-props '("ROAM_KEY")))))
;;;; Title/Path/Slug conversion
(defun org-roam--path-to-slug (path)
  "Return a slug from PATH."
  (-> path
      (file-relative-name (file-truename org-roam-directory))
      (file-name-sans-extension)))

(defun org-roam--get-title-or-slug (path)
  "Convert `PATH' to the file title, if it exists.  Else, return the path."
  (or (car (org-roam-db--get-titles path))
      (org-roam--path-to-slug path)))

(defun org-roam--title-to-slug (title)
  "Convert TITLE to a filename-suitable slug."
  (cl-flet* ((nonspacing-mark-p (char)
                                (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                 (ucs-normalize-NFD-string s))))
             (cl-replace (title pair)
                         (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_")  ;; convert anything not alphanumeric
                    ("__*" . "_")  ;; remove sequential underscores
                    ("^_" . "")  ;; remove starting underscore
                    ("_$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
      (s-downcase slug))))

;;; Interactive Commands
;;;; org-roam-insert
(defun org-roam--format-link-title (title)
  "Return the link title, given the file TITLE."
  (if (functionp org-roam-link-title-format)
      (funcall org-roam-link-title-format title)
    (format org-roam-link-title-format title)))

(defun org-roam--format-link (target &optional description)
  "Formats an org link for a given file TARGET and link DESCRIPTION."
  (let* ((here (-> (or (buffer-base-buffer)
                       (current-buffer))
                   (buffer-file-name)
                   (file-truename)
                   (file-name-directory))))
    (org-link-make-string
     (concat "file:" (file-relative-name target here))
     description)))

(defun org-roam-insert (prefix)
  "Find an Org-roam file, and insert a relative org link to it at point.
If PREFIX, downcase the title before insertion."
  (interactive "P")
  (unless (org-roam--org-roam-file-p
           (buffer-file-name (buffer-base-buffer)))
    (user-error "Not in an Org-roam file"))
  (let* ((region (and (region-active-p)
                      ;; following may lose active region, so save it
                      (cons (region-beginning) (region-end))))
         (region-text (when region
                        (buffer-substring-no-properties
                         (car region) (cdr region))))
         (completions (org-roam--get-title-path-completions))
         (title (org-roam-completion--completing-read "File: " completions
                                                      :initial-input region-text))
         (region-or-title (or region-text title))
         (target-file-path (cdr (assoc title completions)))
         (link-description (org-roam--format-link-title (if prefix
                                                            (downcase region-or-title)
                                                          region-or-title))))
    (if (and target-file-path
             (file-exists-p target-file-path))
        (progn
          (when region ;; Remove previously selected text.
            (delete-region (car region) (cdr region)))
          (insert (org-roam--format-link target-file-path link-description)))
      (if org-roam-capture--in-process
          (user-error "Nested Org-roam capture processes not supported")
        (let ((org-roam-capture--info (list (cons 'title title)
                                            (cons 'slug (org-roam--title-to-slug title))))
              (org-roam-capture--context 'title))
          (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--insert-link-h)
          (setq org-roam-capture-additional-template-props (list :region region
                                                                 :link-description link-description
                                                                 :capture-fn 'org-roam-insert))
          (org-roam--capture))))))

;;;; org-roam-find-file
(defun org-roam--get-title-path-completions ()
  "Return a list of cons pairs for titles to absolute path of Org-roam files."
  (let* ((rows (org-roam-db-query [:select [file titles] :from titles]))
         res)
    (dolist (row rows)
      (let ((file-path (car row))
            (titles (cadr row)))
        (if titles
            (dolist (title titles)
              (push (cons title file-path) res))
          (push (cons (org-roam--path-to-slug file-path)
                      file-path) res))))
    res))

(defun org-roam-find-file (&optional initial-prompt)
  "Find and open an Org-roam file.
INITIAL-PROMPT is the initial title prompt."
  (interactive)
  (let* ((completions (org-roam--get-title-path-completions))
         (title (org-roam-completion--completing-read "File: " completions
                                                      :initial-input initial-prompt))
         (file-path (cdr (assoc title completions))))
    (if file-path
        (find-file file-path)
      (if org-roam-capture--in-process
          (user-error "Org-roam capture in process")
        (let ((org-roam-capture--info (list (cons 'title title)
                                            (cons 'slug (org-roam--title-to-slug title))))
              (org-roam-capture--context 'title))
          (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--find-file-h)
          (org-roam--capture))))))

;;;; org-roam-find-ref
(defun org-roam--get-ref-path-completions ()
  "Return a list of cons pairs for titles to absolute path of Org-roam files."
  (let ((rows (org-roam-db-query [:select [ref file] :from refs])))
    (mapcar (lambda (row)
              (cons (car row)
                    (cadr row))) rows)))

(defun org-roam-find-ref (&optional info)
  "Find and open an Org-roam file from a ref.
INFO is an alist containing additional information."
  (interactive)
  (let* ((completions (org-roam--get-ref-path-completions))
         (ref (or (cdr (assoc 'ref info))
                  (org-roam-completion--completing-read "Ref: "
                                                        (org-roam--get-ref-path-completions)
                                                        :require-match t))))
    (find-file (cdr (assoc ref completions)))))

;;;; org-roam-switch-to-buffer
(defun org-roam--get-roam-buffers ()
  "Return a list of buffers that are Org-roam files."
  (--filter (and (with-current-buffer it (derived-mode-p 'org-mode))
                 (buffer-file-name it)
                 (org-roam--org-roam-file-p (buffer-file-name it)))
            (buffer-list)))

(defun org-roam-switch-to-buffer ()
  "Switch to an existing Org-roam buffer."
  (interactive)
  (let* ((roam-buffers (org-roam--get-roam-buffers))
         (names-and-buffers (mapcar (lambda (buffer)
                                      (cons (or (org-roam--get-title-or-slug
                                                 (buffer-file-name buffer))
                                                (buffer-name buffer))
                                            buffer))
                                    roam-buffers)))
    (unless roam-buffers
      (user-error "No roam buffers"))
    (when-let ((name (org-roam-completion--completing-read "Buffer: " names-and-buffers
                                                           :require-match t)))
      (switch-to-buffer (cdr (assoc name names-and-buffers))))))

;;;; org-roam-capture
(defun org-roam-capture ()
  "Launches an org-capture process for a new or existing note.
This uses the templates defined at `org-roam-capture-templates'."
  (interactive)
  (when org-roam-capture--in-process
    (user-error "Nested Org-roam capture processes not supported"))
  (let* ((completions (org-roam--get-title-path-completions))
         (title (org-roam-completion--completing-read "File: " completions))
         (file-path (cdr (assoc title completions))))
    (let ((org-roam-capture--info (list (cons 'title title)
                                        (cons 'slug (org-roam--title-to-slug title))
                                        (cons 'file file-path)))
          (org-roam-capture--context 'capture))
      (setq org-roam-capture-additional-template-props (list :capture-fn 'org-roam-capture))
      (org-roam--capture))))

;;;; Daily notes
(defcustom org-roam-date-title-format "%Y-%m-%d"
  "Format string passed to `format-time-string' for getting a date file's title."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-date-filename-format "%Y-%m-%d"
  "Format string passed to `format-time-string' for getting a date file's filename."
  :type 'string
  :group 'org-roam)

(defun org-roam--file-path-from-id (id)
  "The file path for an Org-roam file, with identifier ID."
  (file-truename
   (expand-file-name
    (if org-roam-encrypt-files
        (concat id ".org.gpg")
      (concat id ".org"))
    org-roam-directory)))

(defun org-roam--file-for-time (time)
  "Create and find file for TIME."
  (let* ((title (format-time-string org-roam-date-title-format time))
         (filename (format-time-string org-roam-date-filename-format time))
         (file-path (org-roam--file-path-from-id filename)))
    (if (file-exists-p file-path)
        (find-file file-path)
      (let ((org-roam-capture-templates (list (list "d" "daily" 'plain (list 'function #'org-roam-capture--get-point)
                                                    ""
                                                    :immediate-finish t
                                                    :file-name "${filename}"
                                                    :head "#+TITLE: ${title}")))
            (org-roam-capture--context 'title)
            (org-roam-capture--info (list (cons 'title title)
                                          (cons 'filename filename))))
        (add-hook 'org-capture-after-finalize-hook #'org-roam-capture--find-file-h)
        (org-roam--capture)))))

(defun org-roam-today ()
  "Create and find file for today."
  (interactive)
  (org-roam--file-for-time (current-time)))

(defun org-roam-tomorrow (n)
  "Create and find the file for tomorrow.
With numeric argument N, use N days in the future."
  (interactive "p")
  (org-roam--file-for-time (time-add (* n 86400) (current-time))))

(defun org-roam-yesterday (n)
  "Create and find the file for yesterday.
With numeric argument N, use N days in the past."
  (interactive "p")
  (org-roam-tomorrow (- n)))

(defun org-roam-date ()
  "Create the file for any date using the calendar."
  (interactive)
  (let ((time (org-read-date nil 'to-time nil "Date:  ")))
    (org-roam--file-for-time time)))

;;; The org-roam buffer
;;;; org-roam-link-face
(defface org-roam-link
  '((t :inherit org-link))
  "Face for org-roam link."
  :group 'org-roam-faces)

(defface org-roam-backlink
  '((t :inherit org-block))
  "Face for org-roam backlinks in backlinks buffer"
  :group 'org-roam-faces)

(defun org-roam--roam-link-face (path)
  "Conditional face for org file links.
Applies `org-roam-link-face' if PATH corresponds to a Roam file."
  (if (org-roam--org-roam-file-p path)
      'org-roam-link
    'org-link))

;;;; org-roam-backlinks-mode
(defvar org-roam-backlinks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'org-open-at-point)
    (define-key map (kbd "RET") 'org-open-at-point)
    map)
  "Keymap for `org-roam-backlinks-mode'.")

(define-minor-mode org-roam-backlinks-mode
  "Minor mode for the `org-roam-buffer'.
\\{org-roam-backlinks-mode-map}"
  :lighter " Backlinks"
  :keymap org-roam-backlinks-mode-map

  (if org-roam-backlinks-mode
      (add-hook 'org-open-at-point-functions
                'org-roam-open-at-point nil 'local)
    (remove-hook 'org-open-at-point-functions
                 'org-roam-open-at-point 'local)))

(defun org-roam-open-at-point ()
  "Open an Org-roam link or visit the text previewed at point.
When point is on an Org-roam link, open the link in the Org-roam window.
When point is on the Org-roam preview text, open the link in the Org-roam
window, and navigate to the point.
This function hooks into `org-open-at-point' via `org-open-at-point-functions'."
  (cond
   ;; Org-roam link
   ((let* ((context (org-element-context))
           (type (org-element-property :type context))
           (path (org-element-property :path context)))
      (when (and (eq (org-element-type context) 'link)
                 (string= "file" type)
                 (org-roam--org-roam-file-p (file-truename path)))
        (org-roam--find-file path)
        (org-show-context)
        t)))
   ;; Org-roam preview text
   ((when-let ((file-from (get-text-property (point) 'file-from))
               (p (get-text-property (point) 'file-from-point)))
      (org-roam--find-file file-from)
      (goto-char p)
      (org-show-context)
      t))
   ;; If called via `org-open-at-point', fall back to default behavior.
   (t nil)))

(defun org-roam--find-file (file)
  "Open FILE in the window `org-roam' was called from."
  (if (and org-roam-last-window (window-valid-p org-roam-last-window))
      (progn (with-selected-window org-roam-last-window
               (find-file file))
             (select-window org-roam-last-window))
    (find-file file)))

(defun org-roam--get-backlinks (target)
  "Return the backlinks for TARGET.
TARGET may be a file, for Org-roam file links, or a citation key,
for Org-ref cite links."
  (org-roam-db-query [:select [from, to, properties] :from links
                      :where (= to $s1)
                      :order-by (asc from)]
                     target))

;;;; Updating the org-roam buffer
(defun org-roam--buffer-insert-backlinks (file-path)
  "Insert the backlinks for FILE-PATH into the current buffer."
  (if-let* ((file-backlinks (org-roam--get-backlinks file-path))
              (grouped-backlinks (--group-by (nth 0 it) file-backlinks)))
    (progn
      (insert (format "\n\n* %d Backlinks\n"
                      (length file-backlinks)))
      (dolist (group grouped-backlinks)
        (let ((file-from (car group))
              (bls (cdr group)))
          (insert (format "** [[file:%s][%s]]\n"
                          file-from
                          (org-roam--get-title-or-slug file-from)))
          (dolist (backlink bls)
            (pcase-let ((`(,file-from _ ,props) backlink))
              (insert (propertize
                       (s-trim (s-replace "\n" " "
                                          (plist-get props :content)))
                       'help-echo "mouse-1: visit backlinked note"
                       'file-from file-from
                       'file-from-point (plist-get props :point)))
              (insert "\n\n"))))))
    (insert "\n\n* No backlinks!")))

(defun org-roam--buffer-insert-citelinks (file-path)
  "Insert citation backlinks for FILE-PATH into the current buffer."
  (if-let* ((roam-key (with-temp-buffer
                        (insert-file-contents file-path)
                        (org-roam--extract-ref)))
            (key-backlinks (org-roam--get-backlinks (s-chop-prefix "cite:" roam-key)))
            (grouped-backlinks (--group-by (nth 0 it) key-backlinks)))
      (progn
        (insert (format "\n\n* %d Cite backlinks\n"
                        (length key-backlinks)))
        (dolist (group grouped-backlinks)
          (let ((file-from (car group))
                (bls (cdr group)))
            (insert (format "** [[file:%s][%s]]\n"
                            file-from
                            (org-roam--get-title-or-slug file-from)))
            (dolist (backlink bls)
              (pcase-let ((`(,file-from _ ,props) backlink))
                (insert (propertize
                         (s-trim (s-replace "\n" " "
                                            (plist-get props :content)))
                         'help-echo "mouse-1: visit backlinked note"
                         'file-from file-from
                         'file-from-point (plist-get props :point)))
                (insert "\n\n"))))))
    (insert "\n\n* No cite backlinks!")))

(defun org-roam-update (file-path)
  "Show the cite-backlinks for given org file for file at `FILE-PATH'."
  (org-roam-db--ensure-built)
  (let* ((source-org-roam-directory org-roam-directory))
    (let ((buffer-title (org-roam--get-title-or-slug file-path)))
      (with-current-buffer org-roam-buffer
        ;; When dir-locals.el is used to override org-roam-directory,
        ;; org-roam-buffer should have a different local org-roam-directory and
        ;; default-directory, as relative links are relative from the overridden
        ;; org-roam-directory.
        (setq-local org-roam-directory source-org-roam-directory)
        (setq-local default-directory source-org-roam-directory)
        ;; Locally overwrite the file opening function to re-use the
        ;; last window org-roam was called from
        (setq-local
         org-link-frame-setup
         (cons '(file . org-roam--find-file) org-link-frame-setup))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (unless (eq major-mode 'org-mode)
            (org-mode))
          (unless org-roam-backlinks-mode
            (org-roam-backlinks-mode))
          (make-local-variable 'org-return-follows-link)
          (setq org-return-follows-link t)
          (insert
           (propertize buffer-title 'font-lock-face 'org-document-title))
          (org-roam--buffer-insert-backlinks file-path)
          (org-roam--buffer-insert-citelinks file-path))
        (read-only-mode 1)))))

(cl-defun org-roam--maybe-update-buffer (&key redisplay)
  "Reconstructs `org-roam-buffer'.
This needs to be quick or infrequent, because this is run at
`post-command-hook'.  If REDISPLAY, force an update of
`org-roam-buffer'."
  (let ((buffer (window-buffer)))
    (when (and (or redisplay
                   (not (eq org-roam--current-buffer buffer)))
               (eq 'visible (org-roam--current-visibility))
               (buffer-local-value 'buffer-file-truename buffer))
      (setq org-roam--current-buffer buffer)
      (org-roam-update (expand-file-name
                        (buffer-local-value 'buffer-file-truename buffer))))))

;;;; Toggling the org-roam buffer
(define-inline org-roam--current-visibility ()
  "Return whether the current visibility state of the org-roam buffer.
Valid states are 'visible, 'exists and 'none."
  (declare (side-effect-free t))
  (inline-quote
   (cond
    ((get-buffer-window org-roam-buffer) 'visible)
    ((get-buffer org-roam-buffer) 'exists)
    (t 'none))))

(defun org-roam--set-width (width)
  "Set the width of `org-roam-buffer' to `WIDTH'."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (w (max width window-min-width)))
      (cond
       ((> (window-width) w)
        (shrink-window-horizontally  (- (window-width) w)))
       ((< (window-width) w)
        (enlarge-window-horizontally (- w (window-width))))))))

(defun org-roam--set-height (height)
  "Set the height of `org-roam-buffer' to `HEIGHT'."
  (unless (one-window-p)
    (let ((window-size-fixed)
          (h (max height window-min-height)))
      (cond
       ((> (window-height) h)
        (shrink-window  (- (window-height) h)))
       ((< (window-height) h)
        (enlarge-window (- h (window-height))))))))

(defun org-roam--set-up-buffer ()
  "Set up the `org-roam' buffer at the `org-roam-buffer-position'."
  (let ((window (get-buffer-window))
        (position
         (if (member org-roam-buffer-position '(right left top bottom))
             org-roam-buffer-position
           (let ((text-quoting-style 'grave))
             (lwarn '(org-roam) :error
                    "Invalid org-roam-buffer-position: %s. Defaulting to \\='right"
                    org-roam-buffer-position))
           'right)))
    (-> (get-buffer-create org-roam-buffer)
        (display-buffer-in-side-window
         `((side . ,position)))
        (select-window))
    (pcase position
      ((or 'right 'left)
       (org-roam--set-width  (round (* (frame-width)  org-roam-buffer-width))))
      ((or 'top  'bottom)
       (org-roam--set-height (round (* (frame-height) org-roam-buffer-height)))))
    (select-window window)))

(defun org-roam ()
  "Pops up the window `org-roam-buffer' accordingly."
  (interactive)
  (setq org-roam-last-window (get-buffer-window))
  (pcase (org-roam--current-visibility)
    ('visible (delete-window (get-buffer-window org-roam-buffer)))
    ('exists (org-roam--set-up-buffer))
    ('none (org-roam--set-up-buffer))))

;;; The global minor org-roam-mode
(defvar org-roam-mode-map
  (make-sparse-keymap)
  "Keymap for mode `org-roam-mode'.")

;;;###autoload
(define-minor-mode org-roam-mode
  "Minor mode for Org-roam.

This mode sets up several hooks, to ensure that the cache is updated on file
changes, renames and deletes. It is also in charge of graceful termination of
the database connection.

When called interactively, toggle `org-roam-mode'. with prefix
ARG, enable `org-roam-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `org-roam-mode' if ARG is omitted,
nil, or positive. If ARG is `toggle', toggle `org-roam-mode'.
Otherwise, behave as if called interactively."
  :lighter " Org-roam"
  :keymap  org-roam-mode-map
  :group 'org-roam
  :require 'org-roam
  :global t
  (cond
   (org-roam-mode
    (org-roam-db-build-cache)
    (add-hook 'find-file-hook #'org-roam--find-file-hook-function)
    (add-hook 'kill-emacs-hook #'org-roam-db--close-all)
    (advice-add 'rename-file :after #'org-roam--rename-file-advice)
    (advice-add 'delete-file :before #'org-roam--delete-file-advice))
   (t
    (remove-hook 'find-file-hook #'org-roam--find-file-hook-function)
    (remove-hook 'kill-emacs-hook #'org-roam-db--close-all)
    (advice-remove 'rename-file #'org-roam--rename-file-advice)
    (advice-remove 'delete-file #'org-roam--delete-file-advice)
    (org-roam-db--close-all)
    ;; Disable local hooks for all org-roam buffers
    (dolist (buf (org-roam--get-roam-buffers))
      (with-current-buffer buf
        (org-link-set-parameters "file" :face 'org-link)
        (remove-hook 'post-command-hook #'org-roam--maybe-update-buffer t)
        (remove-hook 'after-save-hook #'org-roam-db--update-file t))))))

(defun org-roam--find-file-hook-function ()
  "Called by `find-file-hook' when mode `org-roam-mode' is on."
  (when (org-roam--org-roam-file-p)
    (setq org-roam-last-window (get-buffer-window))
    (add-hook 'post-command-hook #'org-roam--maybe-update-buffer nil t)
    (add-hook 'after-save-hook #'org-roam-db--update-file nil t)
    (org-link-set-parameters "file" :face 'org-roam--roam-link-face)
    (org-roam--maybe-update-buffer :redisplay nil)))

(defun org-roam--delete-file-advice (file &optional _trash)
  "Advice for maintaining cache consistency when FILE is deleted."
  (when (and (not (auto-save-file-name-p file))
             (org-roam--org-roam-file-p file))
    (org-roam-db--clear-file (file-truename file))))

(defun org-roam--rename-file-advice (file new-file &rest _args)
  "Rename backlinks of FILE to refer to NEW-FILE."
  (when (and (not (auto-save-file-name-p file))
             (not (auto-save-file-name-p new-file))
             (org-roam--org-roam-file-p new-file))
    (org-roam-db--ensure-built)
    (let* ((files-to-rename (org-roam-db-query [:select :distinct [from]
                                                :from links
                                                :where (= to $s1)
                                                :and (= type $s2)]
                                               file
                                               "roam"))
           (path (file-truename file))
           (new-path (file-truename new-file))
           (slug (org-roam--get-title-or-slug file))
           (old-title (org-roam--format-link-title slug))
           (new-slug (or (car (org-roam-db--get-titles path))
                         (org-roam--path-to-slug new-path)))
           (new-title (org-roam--format-link-title new-slug)))
      (org-roam-db--clear-file file)
      (dolist (file-from files-to-rename)
        (let* ((file-from (car file-from))
               (file-from (if (string-equal (file-truename file-from)
                                            path)
                              new-path
                            file-from))
               (file-dir (file-name-directory file-from))
               (relative-path (file-relative-name new-path file-dir))
               (old-relative-path (file-relative-name path file-dir))
               (slug-regex (regexp-quote (format "[[file:%s][%s]]" old-relative-path old-title)))
               (named-regex (concat
                             (regexp-quote (format "[[file:%s][" old-relative-path))
                             "\\(.*\\)"
                             (regexp-quote "]]"))))
          (with-temp-file file-from
            (insert-file-contents file-from)
            (while (re-search-forward slug-regex nil t)
              (replace-match (format "[[file:%s][%s]]" relative-path new-title)))
            (goto-char (point-min))
            (while (re-search-forward named-regex nil t)
              (replace-match (format "[[file:%s][\\1]]" relative-path))))
          (org-roam-db--update-file file-from)))
      (org-roam-db--update-file new-path))))
;;; -
(provide 'org-roam)
;;; org-roam.el ends here

;; Local Variables:
;; outline-regexp: ";;;+ "
;; End:
