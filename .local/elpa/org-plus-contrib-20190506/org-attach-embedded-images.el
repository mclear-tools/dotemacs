;;; org-attach-embedded-images.el --- Transmute images to attachments
;;
;; Copyright 2018 Free Software Foundation, Inc.
;;
;; Author: Marco Wahl
;; Version: 0.0
;; Keywords: org, media
;;
;; This file is not part of GNU Emacs.
;;
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; There are occasions when images are displayed in a subtree which
;; are not (yet) org attachments.  For example if you copy and paste a
;; part of a web page (containing images) from eww to an org subtree.

;; This module provides command `org-attach-embedded-images-in-subtree'
;; to save such images as attachments and insert org links to them.

;; To use you might put the following in your .emacs:

;; (require 'org-attach-embedded-images)

;; Use

;;     M-x org-attach-embedded-images-in-subtree

;; in a subtree with embedded images.  The images get attached and can
;; later be reviewed.

;; Note: Possibly

;;     M-x org-toggle-inline-images is needed to see inline

;; images in Org mode.


;; Code:

(require 'org)
(require 'org-attach)


;; Auxiliary functions

(defun org-attach-embedded-images--next-property-display-data (position limit)
  "Return position of the next property-display location with image data.
Return nil if there is no next display property.
POSITION and LIMIT as in `next-single-property-change'."
  (let ((pos (next-single-property-change position 'display nil limit)))
    (while (and (< pos limit)
		(let ((display-prop
		       (plist-get (text-properties-at pos) 'display)))
		  (or (not display-prop)
		      (not (plist-get (cdr display-prop) :data)))))
      (setq pos (next-single-property-change pos 'display nil limit)))
    pos))

(defun org-attach-embedded-images--attach-with-sha1-name (data)
  "Save the image given as DATA as org attachment with its sha1 as name.
Return the filename."
  (let* ((extension (symbol-name (image-type-from-data data)))
         (basename (concat (sha1 data) "." extension))
         (org-attach-filename
          (concat (org-attach-dir t) "/" basename)))
    (unless (file-exists-p org-attach-filename)
      (with-temp-file org-attach-filename
        (setq buffer-file-coding-system 'binary)
        (set-buffer-multibyte nil)
        (insert data)))
    (org-attach-sync)
    org-attach-filename))


;; Command

;;;###autoload
(defun org-attach-embedded-images-in-subtree ()
  "Save the displayed images as attachments and insert links to them."
  (interactive)
  (if (org-before-first-heading-p)
      (message "Before first heading.  Nothing has been attached.")
    (save-excursion
      (let ((beg (progn (org-back-to-heading) (point)))
            (end (progn (org-end-of-subtree) (point)))
	    (names nil))
	;; pass 1
	(goto-char beg)
	(while (< (goto-char (org-attach-embedded-images--next-property-display-data (point) end)) end)
	  (let ((data (plist-get (cdr (plist-get (text-properties-at (point)) 'display)) :data)))
	    (assert data)
	    (push (org-attach-embedded-images--attach-with-sha1-name data)
		  names)))
	;; pass 2
	(setq names (nreverse names))
	(goto-char beg)
	(while names
	  (goto-char (org-attach-embedded-images--next-property-display-data (point) end))
          (while (get-text-property (point) 'display)
	    (goto-char (next-property-change (point) nil end)))
          (skip-chars-forward "]")
          (insert (concat "\n[[" (pop names) "]]")))))))


(provide 'org-attach-embedded-images)


;;; org-attach-embedded-images.el ends here
