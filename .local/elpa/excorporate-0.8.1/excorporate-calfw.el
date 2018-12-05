;;; excorporate-calfw.el --- Exchange calendar view   -*- lexical-binding: t -*-

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

;; Use the Calfw calendar framework to display daily meetings.

;; To use this handler, set excorporate-calendar-show-day to
;; exco-calfw-show-day using `customize-variable'.

;; This Excorporate handler requires the Calfw package, which is not
;; included in GNU ELPA because not all Calfw contributors have
;; copyright assignment papers on file with the FSF.

;;; Code:

;; calfw is not FSF-assigned yet so it is not in GNU ELPA.  The
;; following workarounds allow excorporate-calfw.elc to be built
;; regardless.
(require 'calfw nil t)

(declare-function cfw:component-model "ext:calfw" t)
(declare-function cfw:cp-add-selection-change-hook "ext:calfw" t)
(declare-function cfw:cp-get-contents-sources "ext:calfw" t)
(declare-function cfw:create-calendar-component-buffer "ext:calfw" t)
(declare-function cfw:cursor-to-nearest-date "ext:calfw" t)
(declare-function cfw:date "ext:calfw" t)
(declare-function cfw:model-set-contents-sources "ext:calfw" t)
(declare-function cfw:refresh-calendar-buffer "ext:calfw" t)
(declare-function make-cfw:event "ext:calfw" t)
(declare-function make-cfw:source "ext:calfw" t)

(defvar cfw:component)

;; Fix a bad bug in calfw.  See:
;; https://github.com/kiwanami/emacs-calfw/pull/79
(defun cfw:cp-set-contents-sources (component sources)
  "Set content SOURCES for COMPONENT.
SOURCES is a list of content sources."
  (cfw:model-set-contents-sources
   sources (cfw:component-model component)))

(require 'excorporate)

(defvar excorporate-calfw-buffer-name "*Excorporate*"
  "The buffer into which Calfw output is inserted.")

(defun exco-calfw-initialize-buffer (month day year)
  "Set up an initial blank Calfw buffer for date MONTH DAY YEAR."
  (with-current-buffer (get-buffer-create excorporate-calfw-buffer-name)
    (display-buffer (current-buffer))
    (let ((status-source (make-cfw:source :name "Updating..."
					  :data (lambda (_b _e) nil))))
      (cfw:create-calendar-component-buffer
       :date (cfw:date month day year) :view 'day
       :contents-sources (list status-source)
       :buffer (current-buffer)))))

(defun exco-calfw-add-meeting (subject start end location
				       main-invitees optional-invitees)
  "Add a scheduled meeting to the event list.
SUBJECT is a string, the subject of the meeting.  START is the
meeting start time in Emacs internal date time format, and END is
the end of the meeting in the same format.  LOCATION is a string
representing the location.  MAIN-INVITEES and OPTIONAL-INVITEES
are the requested participants."
  (let ((start-list (decode-time start))
	(end-list (decode-time end)))
    (make-cfw:event :title (concat
			    (format "\n\t%s" subject)
			    (format "\n\tLocation: %s" location)
			    (when main-invitees
			      (format "\n\tInvitees: %s"
				      (mapconcat 'identity
						 main-invitees "; ")))
			    (when optional-invitees
			      (format "\n\tOptional: %s"
				      (mapconcat 'identity
						 optional-invitees "; "))))
		    :start-date  (list (elt start-list 4)
				       (elt start-list 3)
				       (elt start-list 5))
		    :start-time  (list (elt start-list 2)
				       (elt start-list 1))
		    :end-date	 (list (elt end-list 4)
				       (elt end-list 3)
				       (elt end-list 5))
		    :end-time	 (list (elt end-list 2)
				       (elt end-list 1)))))

(defun exco-calfw-add-meetings (identifier response)
  "Add the connection IDENTIFIER's meetings from RESPONSE."
  (let ((event-list (exco-calendar-item-iterate response
						#'exco-calfw-add-meeting)))
    (with-current-buffer (get-buffer-create excorporate-calfw-buffer-name)
      (let* ((new-source (make-cfw:source
			  :name (format "%S (as of %s)"
					identifier
					(format-time-string "%F %H:%M"))
			  :data (lambda (_b _e)
				  event-list)))
	     (sources (cfw:cp-get-contents-sources cfw:component))
	     (new-sources (append sources (list new-source))))
	(cfw:cp-set-contents-sources cfw:component new-sources)))))

(defun exco-calfw-finalize-buffer ()
  "Finalize the Calfw widget after retrievals have completed."
  (with-current-buffer (get-buffer-create excorporate-calfw-buffer-name)
    (let ((sources (cfw:cp-get-contents-sources cfw:component))
	  (status-source (make-cfw:source :name "Done."
					  :data (lambda (_b _e) nil))))
      (cfw:cp-set-contents-sources cfw:component
				   (cons status-source (cdr sources))))
    (cfw:cp-add-selection-change-hook cfw:component
				      (lambda ()
					(apply #'exco-calfw-show-day
					       (cfw:cursor-to-nearest-date))))
    (cfw:refresh-calendar-buffer nil)))

;;;###autoload
(defun exco-calfw-show-day (month day year)
  "Show meetings for the date specified by MONTH DAY YEAR."
  (exco-connection-iterate
   (lambda ()
     (exco-calfw-initialize-buffer month day year))
   (lambda (identifier callback)
     (exco-get-meetings-for-day identifier month day year
				callback))
   #'exco-calfw-add-meetings
   #'exco-calfw-finalize-buffer))

(provide 'excorporate-calfw)

;;; excorporate-calfw.el ends here
