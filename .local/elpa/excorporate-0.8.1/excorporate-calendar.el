;;; excorporate-calendar.el --- Exchange for calendar -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Keywords: calendar

;; This program is free software: you can redistribute it and/or modify
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

;; Add a calendar keybinding for Excorporate.  Default to the
;; excorporate-org interface.

;;; Code:

(require 'calendar)

(defcustom excorporate-calendar-show-day-function 'exco-org-show-day
  "A function to be called by pressing `e' in Calendar."
  :type 'function
  :group 'excorporate)

(defun exco-calendar-show-day ()
  "Show meetings for the selected date."
  (interactive)
  (apply excorporate-calendar-show-day-function (calendar-cursor-to-date t)))

;; I arrogantly claim "e" for now, but irresponsibly reserve the right
;; to change it later.
(define-key calendar-mode-map "e" #'exco-calendar-show-day)

(provide 'excorporate-calendar)

;;; excorporate-calendar.el ends here
