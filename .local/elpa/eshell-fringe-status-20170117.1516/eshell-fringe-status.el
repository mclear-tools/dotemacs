;;; eshell-fringe-status.el --- Show last status in fringe  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Tom Willemse

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords:
;; Package-Version: 20170117.1516
;; Version: 1.0.0
;; URL: http://projects.ryuslash.org/eshell-fringe-status/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Show an indicator of the status of the last command run in eshell.
;; To use, enable `eshell-fringe-status-mode' in `eshell-mode'.  The
;; easiest way to do this is by adding a hook:

;; : (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode)

;; This mode uses a rather hackish way to try and keep everything
;; working in regard to `eshell-prompt-regexp', so if anything breaks
;; please let me know.

;; Some extra fringe bitmaps are provided.  In case you prefer either
;; or both of them over the default arrow bitmap.  These are
;; `efs-plus-bitmap' and `efs-minus-bitmap'.  These show a `+' and `-'
;; in the fringe respectively, instead of an arrow.  These can be used
;; by setting the `eshell-fringe-status-success-bitmap' and the
;; `eshell-fringe-status-failure-bitmap' options.

;;; Code:

(require 'em-prompt)

(defgroup eshell-fringe-status
  nil
  "Settings for command exit status shown in Emacs' fringe."
  :group 'eshell
  :prefix "eshell-fringe")

(defcustom eshell-fringe-status-success-bitmap 'efs-arrow-bitmap
  "The fringe bitmap to use when indicating a succesfull operation.

In combination with the `eshell-fringe-status-success' face this
determines the look of the fringe indicator."
  :group 'eshell-fringe-status
  :type '(choice
          (const :tag "Arrow" efs-arrow-bitmap)
          (const :tag "Minus" efs-minus-bitmap)
          (const :tag "Plus" efs-plus-bitmap)))

(defcustom eshell-fringe-status-failure-bitmap 'efs-arrow-bitmap
  "The fringe bitmap to use when indicating a failed operation.

In combination with the `eshell-fringe-status-failure' face this
determines the look of the fringe indicator."
  :group 'eshell-fringe-status
  :type '(choice
          (const :tag "Arrow" efs-arrow-bitmap)
          (const :tag "Minus" efs-minus-bitmap)
          (const :tag "Plus" efs-plus-bitmap)))

(defcustom eshell-fringe-status-before-insert-hook nil
  "A list of functions to call before inserting the fringe status."
  :group 'eshell-fringe-status
  :type 'hook)

(defcustom eshell-fringe-status-after-insert-hook nil
  "A list of functions to call after inserting the fringe status."
  :group 'eshell-fringe-status
  :type 'hook)

(defface eshell-fringe-status-success
  '((t (:foreground "#00aa00")))
  "Face used to indicate success status.

In combination with the `eshell-fringe-status-success-bitmap'
this determines the look of the fringe indicator."
  :group 'eshell-fringe-status)

(defface eshell-fringe-status-failure
  '((t (:foreground "#aa0000")))
  "Face used to indicate failed status.

In combination withh the `eshell-fringe-status-failure-bitmap'
this determines the look of the fringe indicator."
  :group 'eshell-fringe-status)

(define-fringe-bitmap 'efs-arrow-bitmap
  [#b10000
   #b11000
   #b11100
   #b11110
   #b11111
   #b11110
   #b11100
   #b11000
   #b10000] 9 5 'center)

(define-fringe-bitmap 'efs-plus-bitmap
  [#b001100
   #b001100
   #b111111
   #b111111
   #b001100
   #b001100] 6 6 'center)

(define-fringe-bitmap 'efs-minus-bitmap
  [#b111111
   #b111111] 2 6 'center)

;; efs--depending on command (status)
(defsubst efs--doc (a b)
  "Depending on the `eshell-last-command-status' use either A or B."
  (if (zerop eshell-last-command-status) a b))

(defun efs--extend-prompt-regexp ()
  "Add a space at the beginning of `eshell-prompt-regexp'.

Since the fringe bitmap is added as a space with a special
display value, any existing regexp in `eshell-prompt-regexp'
which doesn't accept at least one space will break."
  (let ((first (aref eshell-prompt-regexp 0)))
    (when (eql first ?^)
      (setq eshell-prompt-regexp
            (format "%c ?%s" first (substring eshell-prompt-regexp 1))))))

(defun efs--propertized-text ()
  "Return the propertized text to insert into the eshell bufffer."
  (let ((face (efs--doc 'eshell-fringe-status-success
                        'eshell-fringe-status-failure))
        (bitmap (efs--doc eshell-fringe-status-success-bitmap
                          eshell-fringe-status-failure-bitmap)))
    (propertize " " 'display `((left-fringe ,bitmap ,face)))))

(defun efs--revert-prompt-regexp ()
  "The counterpart for `efs--extend-prompt-regexp', remove a space.

Since when the mode is started a space is added to the beginning
of `eshell-prompt-regexp' it should also be removed when
disabling the mode."
  (let ((first (aref eshell-prompt-regexp 0)))
    (when (and (eql first ?^)
               (eql (aref eshell-prompt-regexp 1) ?\s))
      (setq eshell-prompt-regexp
            (format "%c%s" first (substring eshell-prompt-regexp 3))))))

(defun eshell-fringe-status ()
  "Display an indication of the last command's exit status.

This indication is shown as a bitmap in the left fringe of the
window."
  (when eshell-last-command-name
    (save-excursion
      (beginning-of-line)
      (let ((inhibit-read-only t))
        (run-hooks 'eshell-fringe-status-before-insert-hook)
        (insert (efs--propertized-text))
        (run-hooks 'eshell-fringe-status-after-insert-hook)))))

;;;###autoload
(define-minor-mode eshell-fringe-status-mode
  "Show exit status of last command in fringe."
  nil nil nil
  (if eshell-fringe-status-mode
      (progn
        (efs--extend-prompt-regexp)
        (add-hook 'eshell-after-prompt-hook
                  #'eshell-fringe-status nil :local))
    (efs--revert-prompt-regexp)
    (remove-hook 'eshell-after-prompt-hook
                 #'eshell-fringe-status :local)))

(provide 'eshell-fringe-status)
;;; eshell-fringe-status.el ends here
