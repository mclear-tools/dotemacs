;;; mixed-pitch.el --- Use a variable pitch, keeping fixed pitch where it's sensible -*- lexical-binding: t; -*-

;;; Copyright (C) 2017, 2018 by J. Alexander Branham

;; Author: J. Alexander Branham <branham@utexas.edu>
;; Maintainer: J. Alexander Branham <branham@utexas.edu>
;; URL: https://github.com/jabranham/mixed-pitch
;; Package-Version: 20180410.917
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>


;;; Commentary:

;; `mixed-pitch-mode' is a minor mode that enables mixing variable-pitch and
;; fixed-pitch fonts in the same buffer.  The list
;; `mixed-pitch-fixed-pitch-faces' defines the faces that are kept fixed-pitch,
;; everything else becomes variable-pitch.

;; Original idea came from https://ogbe.net/blog/toggle-serif.html
;; Shared with permission.

;; Usage:
;; (require 'mixed-pitch)
;; (mixed-pitch-mode)
;; Or, to apply mixed-pitch-mode in all text modes:
;; (add-hook 'text-mode-hook #'mixed-pitch-mode)

;;; Code:

(require 'face-remap)
(require 'cl-lib)

(defgroup mixed-pitch nil
  "Mix variable and fixed pitch in a single buffer."
  :tag "Mixed pitch"
  :prefix "mixed-pitch"
  :group 'mixed-pitch)

(defcustom mixed-pitch-fixed-pitch-faces
  '(diff-added
    diff-context
    diff-file-header
    diff-function
    diff-header
    diff-hunk-header
    diff-removed
    font-latex-math-face
    font-latex-sedate-face
    font-latex-warning-face
    font-latex-sectioning-5-face
    font-lock-builtin-face
    font-lock-comment-delimiter-face
    font-lock-constant-face
    font-lock-doc-face
    font-lock-function-name-face
    font-lock-keyword-face
    font-lock-negation-char-face
    font-lock-preprocessor-face
    font-lock-regexp-grouping-backslash
    font-lock-regexp-grouping-construct
    font-lock-string-face
    font-lock-type-face
    font-lock-variable-name-face
    markdown-code-face
    markdown-gfm-checkbox-face
    markdown-inline-code-face
    markdown-language-info-face
    markdown-language-keyword-face
    markdown-math-face
    message-header-name
    message-header-to
    message-header-cc
    message-header-newsgroups
    message-header-xheader
    message-header-subject
    message-header-other
    mu4e-header-key-face
    mu4e-header-value-face
    mu4e-link-face
    mu4e-contact-face
    mu4e-compose-separator-face
    mu4e-compose-header-face
    org-block
    org-block-begin-line
    org-block-end-line
    org-document-info-keyword
    org-code
    org-latex-and-related
    org-checkbox
    org-meta-line
    org-table
    org-verbatim)
  "This is a list holding names of faces that will not be variable pitch when function `mixed-pitch-mode' is enabled."
  :type '(repeat face)
  :group 'mixed-pitch)

(defcustom mixed-pitch-variable-pitch-cursor 'bar
  "If non-nil, function `mixed-pitch-mode' changes the cursor.
When disabled, switch back to what it was before.

See `cursor-type' for a list of acceptable types."
  :type 'symbol
  :group 'mixed-pitch)

(defvar-local mixed-pitch-fixed-cookie nil)
(defvar-local mixed-pitch-variable-cookie nil)
(defvar-local mixed-pitch-cursor-type nil)

;;;###autoload
(define-minor-mode mixed-pitch-mode
  "Change the default face of the current buffer to a variable pitch, while keeping some faces fixed pitch.

See the variable `mixed-pitch-fixed-pitch-faces' for a list of
which faces remain fixed pitch. The height and pitch of faces is
inherited from `variable-pitch' and `default'."
  :lighter " MPM"
  (let ((var-pitch (face-attribute 'variable-pitch :family))
        (var-height (face-attribute 'variable-pitch :height))
        (fix-pitch (face-attribute 'default :family))
        (fix-height (face-attribute 'default :height)))
    ;; Turn mixed-pitch-mode on:
    (if mixed-pitch-mode
        (progn
          ;; remember cursor type
          (when mixed-pitch-variable-pitch-cursor
            (setq mixed-pitch-cursor-type cursor-type))
          ;; remap default face to variable pitch
          (setq mixed-pitch-variable-cookie
                (face-remap-add-relative
                 'default :family var-pitch :height var-height))
          (setq mixed-pitch-fixed-cookie nil)
          ;; keep fonts in `mixed-pitch-fixed-pitch-faces' as fixed-pitch.
          (dolist (face mixed-pitch-fixed-pitch-faces)
            (add-to-list 'mixed-pitch-fixed-cookie
                         (face-remap-add-relative
                          face :family fix-pitch :height fix-height)))
          ;; Change the cursor if the user requested:
          (when mixed-pitch-variable-pitch-cursor (setq cursor-type mixed-pitch-variable-pitch-cursor)))
      ;; Turn mixed-pitch-mode off:
      (progn (face-remap-remove-relative mixed-pitch-variable-cookie)
             (dolist (cookie mixed-pitch-fixed-cookie)
               (face-remap-remove-relative cookie))
             ;; Restore the cursor if we changed it:
             (when mixed-pitch-variable-pitch-cursor
               (setq cursor-type mixed-pitch-cursor-type))))))

(provide 'mixed-pitch)
;;; mixed-pitch.el ends here
