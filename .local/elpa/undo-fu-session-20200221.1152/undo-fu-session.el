;;; undo-fu-session.el --- Persistent undo, available between sessions -*- lexical-binding: t -*-

;; Copyright (C) 2020  Campbell Barton
;; Copyright (C) 2009-2015  Tomohiro Matsuyama

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://gitlab.com/ideasman42/emacs-undo-fu-session
;; Package-Version: 20200221.1152
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension provides a way to use undo steps of
;; individual file buffers persistently.
;;

;;; Usage

;;
;; Write the following code to your .emacs file:
;;
;;   (require 'undo-fu-session)
;;   (global-undo-fu-session-mode)
;;
;; Or with `use-package':
;;
;;   (use-package undo-fu-session)
;;   (global-undo-fu-session-mode)
;;
;; If you prefer to enable this per-mode, you may do so using
;; mode hooks instead of calling `global-undo-fu-session-mode'.
;; The following example enables this for org-mode:
;;
;;   (add-hook 'org-mode-hook (lambda () (undo-fu-session-mode))
;;

;;; Code:

;; ---------------------------------------------------------------------------
;; Custom variables.

(defgroup undo-fu-session nil
  "Persistent undo steps, stored on disk between sessions."
  :group 'convenience)

(defcustom undo-fu-session-directory
  (locate-user-emacs-file "undo-fu-session" ".emacs-undo-fu-session")
  "The directory to store undo data."
  :group 'undo-fu-session
  :type 'string)

(defcustom undo-fu-session-compression t
  "Store files compressed."
  :group 'undo-fu-session
  :type 'boolean)

(defcustom undo-fu-session-incompatible-files '()
  "List of REGEXP or FUNCTION for matching files to ignore for undo session."
  :group 'undo-fu-session
  :type '(repeat (choice regexp function)))

(defcustom undo-fu-session-incompatible-major-modes nil
  "List of major-modes in which saving undo data should not be performed."
  :type '(repeat symbol)
  :group 'undo-fu-session)

(defcustom undo-fu-session-file-limit nil
  "Number of files to store, nil to disable limiting entirely.

Enforcing removes the oldest files."
  :type 'integer
  :group 'undo-fu-session)

;; ---------------------------------------------------------------------------
;; Undo Encode/Decode Functionality

(defun undo-fu-session--walk-tree (fn tree)
  "Operate recursively on undo-list, calling FN TREE."
  (cond
    ((consp tree)
      (let ((value (funcall fn tree)))
        (if (eq value tree)
          (let*
            (
              (cons (cons (undo-fu-session--walk-tree fn (car tree)) nil))
              (cur cons)
              cdr)
            (while tree
              (setq cdr (cdr tree))
              (if (consp cdr)
                (let ((next (cons (undo-fu-session--walk-tree fn (car cdr)) nil)))
                  (setcdr cur next)
                  (setq cur next)
                  (setq tree cdr))
                (setcdr cur (undo-fu-session--walk-tree fn cdr))
                (setq tree nil)))
            cons)
          value)))
    ((vectorp tree)
      (let ((value (funcall fn tree)))
        (if (eq value tree)
          (let*
            (
              (length (length tree))
              (vector (make-vector length nil)))
            (dotimes (i (1- length))
              (aset vector i (undo-fu-session--walk-tree fn (aref tree i))))
            vector)
          value)))
    (tree
      (funcall fn tree))))

(defun undo-fu-session--encode (tree)
  "Encode `TREE' so that it can be stored as a file."
  (undo-fu-session--walk-tree
    (lambda (a)
      (cond
        ((markerp a)
          (cons
            (if (marker-insertion-type a)
              'marker*
              'marker)
            (marker-position a)))
        ((overlayp a)
          `(overlay ,(overlay-start a) ,(overlay-end a)))
        ((stringp a)
          (substring-no-properties a))
        (t
          a)))
    tree))

(defun undo-fu-session--decode (tree)
  "Decode `TREE' so that it can be recovered as undo data."
  (undo-fu-session--walk-tree
    (lambda (a)
      (if (consp a)
        (cond
          ((eq (car a) 'marker)
            (set-marker (make-marker) (cdr a)))
          ((eq (car a) 'marker*)
            (let ((marker (make-marker)))
              (set-marker marker (cdr a))
              (set-marker-insertion-type marker t)
              marker))
          ((eq (car a) 'overlay)
            (let
              (
                (start (cadr a))
                (end (caddr a)))
              (if (and start end)
                (make-overlay (cadr a) (caddr a))
                ;; Make deleted overlay
                (let ((overlay (make-overlay (point-min) (point-min))))
                  (delete-overlay overlay)
                  overlay))))
          (t
            a))
        a))
    tree))

(defun undo-fu-session--next-step (list)
  "Get the next undo step in LIST.

Argument LIST compatible list `buffer-undo-list'."
  (while (car list)
    (setq list (cdr list)))
  (while (and list (null (car list)))
    (setq list (cdr list)))
  list)

(defun undo-fu-session--list-to-index-map (list index index-step step-to-index-hash)
  "Populate the STEP-TO-INDEX-HASH with LIST element as keys mapping to INDEX by INDEX-STEP."
  (unless (eq list t)
    (while list
      (puthash list index step-to-index-hash)
      (setq index (+ index index-step))
      (setq list (undo-fu-session--next-step list)))))

(defun undo-fu-session--list-from-index-map (list index index-step step-from-index-hash)
  "Populate the STEP-FROM-INDEX-HASH with INDEX by INDEX-STEP as keys mapping to LIST elements."
  (unless (eq list t)
    (while list
      (puthash index list step-from-index-hash)
      (setq index (+ index index-step))
      (setq list (undo-fu-session--next-step list)))))

(defun undo-fu-session--equivtable-encode (equiv-table buffer-list pending-list)
  "Convert the EQUIV-TABLE into an alist of buffer list indices.
Argument BUFFER-LIST typically `undo-buffer-list'.
Argument PENDING-LIST typically `pending-undo-list'."
  (let
    ( ;; Map undo-elem -> index.
      ;; Negative indices for 'pending-list'.
      (step-to-index-hash (make-hash-table :test 'eq))
      (equiv-table-alist (list)))

    (undo-fu-session--list-to-index-map buffer-list 0 1 step-to-index-hash)
    (undo-fu-session--list-to-index-map pending-list -1 -1 step-to-index-hash)

    (maphash
      (lambda (key val)
        (let
          (
            (key-num (gethash key step-to-index-hash))
            (val-num
              (if (eq t val)
                t
                (gethash val step-to-index-hash))))
          (when (and key-num val-num)
            (push (cons key-num val-num) equiv-table-alist))))
      equiv-table)
    equiv-table-alist))


(defun undo-fu-session--equivtable-decode (equiv-table-alist buffer-list pending-list)
  "Convert EQUIV-TABLE-ALIST into a hash compatible with `undo-equiv-table'.
Argument BUFFER-LIST an `undo-buffer-list' compatible list.
Argument PENDING-LIST an `pending-undo-list'. compatible list."

  (let*
    (
      (equiv-table-length (length equiv-table-alist))
      ;; Map index -> undo-elem.
      ;; Negative indices for 'pending-list'.
      (step-from-index-hash (make-hash-table :test 'eq))
      (equiv-table-hash (make-hash-table :test 'eq :weakness t :size equiv-table-length)))

    (unless (zerop equiv-table-length)

      (undo-fu-session--list-from-index-map buffer-list 0 1 step-from-index-hash)
      (undo-fu-session--list-from-index-map pending-list -1 -1 step-from-index-hash)

      (dolist (item equiv-table-alist)
        (pcase-let ((`(,key-num . ,val-num) item))
          (let
            (
              (key (gethash key-num step-from-index-hash))
              (val
                (if (eq t val-num)
                  t
                  (gethash val-num step-from-index-hash))))
            (puthash key val equiv-table-hash)))))
    equiv-table-hash))

;; ---------------------------------------------------------------------------
;; Undo Session Limiting Functionality

(defun undo-fu-session--file-limit-enforce ()
  "Limit the number of session files to the `undo-fu-session-file-limit' newest."
  ;; While errors are highly unlikely in this case,
  ;; clearing old files should _never_ interfere with other operations,
  ;; so surround with error a check & error message.
  (condition-case err_1
    (when (file-directory-p undo-fu-session-directory)
      (dolist
        (file-with-attrs
          (nthcdr ;; Skip new files, removing old.
            undo-fu-session-file-limit
            (sort ;; Sort new files first.
              (delq
                nil ;; Non-file.
                (mapcar
                  #'
                  (lambda (x)
                    (unless (nth 1 x)
                      x))
                  (directory-files-and-attributes undo-fu-session-directory t nil t)))
              (lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))
        (let ((file (car file-with-attrs)))
          (condition-case err_2
            (delete-file file)
            (error (message "Undo-Fu-Session error deleting '%s' for '%s'" err_2 file))))))
    (error (message "Undo-Fu-Session error limiting files '%s'" err_1))))

;; ---------------------------------------------------------------------------
;; Undo Save/Restore Functionality

(defun undo-fu-session--make-file-name (filename)
  "Take the path FILENAME and return a name base on this."
  (concat
    (expand-file-name
      (url-hexify-string (convert-standard-filename (expand-file-name filename)))
      undo-fu-session-directory)
    (if undo-fu-session-compression
      ".gz"
      ".el")))

(defun undo-fu-session--match-file-name (filename test-files)
  "Return t if FILENAME match any item in TEST-FILES."
  (catch 'found
    (dolist (matcher test-files)
      (when
        (if (stringp matcher)
          (string-match-p matcher filename)
          (funcall matcher filename))
        (throw 'found t)))))

(defun undo-fu-session--recover-buffer-p (buffer)
  "Return t if undo data of BUFFER should be recovered."
  (let
    (
      (filename (buffer-file-name buffer))
      (test-files undo-fu-session-incompatible-files)
      (test-modes undo-fu-session-incompatible-major-modes))
    (cond
      ((null filename)
        nil)
      ((and test-files (undo-fu-session--match-file-name filename test-files))
        nil)
      ((and test-modes (memq (buffer-local-value 'major-mode buffer) test-modes))
        nil)
      (t
        t))))

(defun undo-fu-session--save-impl ()
  "Internal save logic, resulting in t on success."
  (let
    (
      (buffer (current-buffer))
      (filename (buffer-file-name))
      (undo-file nil)
      (content-header nil)
      (content-data nil)

      ;; Quiet compression messages for 'with-auto-compression-mode'.
      (jka-compr-verbose nil))

    (catch 'exit
      ;; No need for a message, exit silently since there is nothing to do.
      (unless (undo-fu-session--recover-buffer-p buffer)
        (throw 'exit nil))

      (unless (or (consp buffer-undo-list) (consp pending-undo-list))
        (throw 'exit nil))

      (setq content-header `((buffer-size . ,(buffer-size)) (buffer-checksum . ,(sha1 buffer))))
      (setq content-data
        `
        ((emacs-pending-undo-list . ,(undo-fu-session--encode pending-undo-list))
          (emacs-buffer-undo-list . ,(undo-fu-session--encode buffer-undo-list))
          (emacs-undo-equiv-table
            .
            ,
            (undo-fu-session--equivtable-encode
              undo-equiv-table
              buffer-undo-list
              pending-undo-list)))))

    (when content-data
      (setq undo-file (undo-fu-session--make-file-name filename))

      ;; Only enforce the file limit when saving new files,
      ;; this avoids scanning the undo session directory on every successive save.
      (when undo-fu-session-file-limit
        (unless (file-exists-p undo-file)
          (undo-fu-session--file-limit-enforce)))

      (with-auto-compression-mode
        (with-temp-buffer
          (prin1 content-header (current-buffer))
          (write-char ?\n (current-buffer))
          (prin1 content-data (current-buffer))
          (write-region nil nil undo-file nil 0)
          t)))))

(defun undo-fu-session-save-safe ()
  "Public save function, typically called by `before-save-hook'."
  (when (bound-and-true-p undo-fu-session-mode)
    (condition-case err
      (undo-fu-session--save-impl)
      (error (message "Undo-Fu-Session can not save undo data: %s" err)))))

(defun undo-fu-session-save ()
  "Save undo data."
  (interactive)
  (undo-fu-session-save-safe))

(defun undo-fu-session--recover-impl ()
  "Internal restore logic, resulting in t on success."
  (let
    (
      (buffer (current-buffer))
      (filename (buffer-file-name))
      (undo-file nil)
      (content-header nil)
      (content-data nil)

      ;; Quiet compression messages for 'with-auto-compression-mode'.
      (jka-compr-verbose nil))

    (catch 'exit
      ;; No need for a message, exit silently since there is nothing to do.
      (unless (undo-fu-session--recover-buffer-p buffer)
        (throw 'exit nil))

      (setq undo-file (undo-fu-session--make-file-name filename))

      (unless (file-exists-p undo-file)
        (throw 'exit nil))

      (with-auto-compression-mode
        (with-temp-buffer
          (insert-file-contents undo-file)
          (goto-char (point-min))
          (setq content-header (read (current-buffer)))

          (unless (eq (buffer-size buffer) (assoc-default 'buffer-size content-header))
            (message "Undo-Fu-Session discarding undo data: file length mismatch")
            (throw 'exit nil))

          (unless (string-equal (sha1 buffer) (assoc-default 'buffer-checksum content-header))
            (message "Undo-Fu-Session discarding undo data: file checksum mismatch")
            (throw 'exit nil))

          ;; No errors... decode all undo data.
          (setq content-data (read (current-buffer))))))

    (when content-data
      (let*
        (
          (emacs-pending-undo-list
            (undo-fu-session--decode (assoc-default 'emacs-pending-undo-list content-data #'eq t)))
          (emacs-buffer-undo-list
            (undo-fu-session--decode (assoc-default 'emacs-buffer-undo-list content-data #'eq t)))
          (emacs-undo-equiv-table
            (undo-fu-session--equivtable-decode
              (assoc-default 'emacs-undo-equiv-table content-data #'eq '())
              emacs-buffer-undo-list
              emacs-pending-undo-list)))

        ;; Assign undo data to the current buffer.
        (setq pending-undo-list emacs-pending-undo-list)
        (setq buffer-undo-list emacs-buffer-undo-list)
        (if (hash-table-p emacs-undo-equiv-table)
          (setq undo-equiv-table emacs-undo-equiv-table)
          (clrhash undo-equiv-table))
        t))))

(defun undo-fu-session-recover-safe ()
  "Public restore function, typically called by `find-file-hook'."
  (when (bound-and-true-p undo-fu-session-mode)
    (condition-case err
      (undo-fu-session--recover-impl)
      (error (message "Undo-Fu-Session can not recover undo data: %s" err)))))

(defun undo-fu-session-recover ()
  "Recover undo data."
  (interactive)
  (undo-fu-session-recover-safe))

;; ---------------------------------------------------------------------------
;; Define Minor Mode
;;
;; Developer note, use global hooks since these run before buffers are loaded.
;; Each function checks if the local mode is active before operating.

(defun undo-fu-session-mode-enable ()
  "Turn on 'undo-fu-session-mode' for the current buffer."
  (unless (file-directory-p undo-fu-session-directory)
    (make-directory undo-fu-session-directory t))
  (add-hook 'before-save-hook #'undo-fu-session-save-safe)
  (add-hook 'find-file-hook #'undo-fu-session-recover-safe))

(defun undo-fu-session-mode-disable ()
  "Turn off 'undo-fu-session-mode' for the current buffer."
  (remove-hook 'before-save-hook #'undo-fu-session-save-safe t)
  (remove-hook 'find-file-hook #'undo-fu-session-recover-safe t))

(defun undo-fu-session-mode-turn-on ()
  "Enable command `undo-fu-session-mode'."
  (undo-fu-session-mode 1))

;;;###autoload
(define-minor-mode undo-fu-session-mode
  "Toggle saving the undo data in the current buffer (Undo-Fu Session Mode)."
  :global nil

  (cond
    (undo-fu-session-mode
      (undo-fu-session-mode-enable))
    (t
      (undo-fu-session-mode-disable))))

;;;###autoload
(define-globalized-minor-mode
  global-undo-fu-session-mode
  undo-fu-session-mode
  undo-fu-session-mode-turn-on)

(provide 'undo-fu-session)
;;; undo-fu-session.el ends here
