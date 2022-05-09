;;; cpm-setup-try.el --- summary -*- lexical-binding: t -*-

;; Author: Colin McLear
;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Setup file for code or packages I'm just trying out

;;;; Simple splash screen
(use-package kisses
  :disabled
  :straight (kisses :type git :host github :repo "jsilve24/kisses")
  :config
  (setq kisses-banner " [a] Agenda\n[c] Config\n[m] Mail\n[n] Notes\n[p] Projects")
  ;; I just use the default banner and add some nice text property to it
  (put-text-property 0 (length kisses-banner) 'face 'lambda-purple
		             kisses-banner)
  ;; This makes it a nice initial buffer (not the default just a nice fallback)
  (setq initial-buffer-choice 'kisses-initial-buffer))

;;;; Emacs Inspector
(use-package emacs-inspector
  :straight (:type git :host github :repo "mmontone/emacs-inspector")
  :commands (inspect-expression
             inspect-last-sexp))

;;; Code:



(provide 'cpm-setup-try)
;;; cpm-setup-try.el ends here
