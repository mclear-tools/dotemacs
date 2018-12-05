;;; excorporate.el --- Exchange integration           -*- lexical-binding: t -*-

;; Copyright (C) 2014-2018 Free Software Foundation, Inc.

;; Author: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Maintainer: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Created: 2014-09-19
;; Version: 0.8.1
;; Keywords: calendar
;; Homepage: https://www.fitzsim.org/blog/
;; Package-Requires: ((emacs "24.1") (fsm "0.2") (soap-client "3.1.4") (url-http-ntlm "2.0.3") (nadvice "0.3"))

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

;; Excorporate provides Exchange integration for Emacs.

;; To create a connection to a web service:

;; M-x excorporate

;; Excorporate will prompt for an email address that it will use to
;; automatically discover settings.  Then it will connect to two or
;; three separate hosts: the autodiscovery host, the web service host
;; or load balancer, and the actual server if there is a load
;; balancer.  Therefore you may be prompted for your credentials two
;; or three times.

;; You should see a message indicating that the connection is ready
;; either in the minibuffer or failing that in the *Messages* buffer.

;; Finally, run M-x calendar, and press 'e' to show today's meetings.

;; Please try autodiscovery first and report issues not yet listed
;; below.  When autodiscovery works it is very convenient; the goal is
;; to make it work for as many users as possible.

;; If autodiscovery fails, customize `excorporate-configuration' to
;; skip autodiscovery.

;; Autodiscovery will fail if:

;; - Excorporate is accessing the server through a proxy (Emacs
;;   bug#10).

;; - The server is not configured to support autodiscovery.

;; - The email address is at a different domain than the server, e.g.,
;;   user@domain1.com, autodiscover.domain2.com.

;; - Authentication is Kerberos/GSSAPI.

;; Excorporate does know about the special case where the mail address
;; is at a subdomain, e.g., user@sub.domain.com, and the server is at
;; the main domain, e.g., autodiscover.domain.com.  Autodiscovery will
;; work in that case.

;; Excorporate must be loaded before any other package that requires
;; `soap-client'.  The version of `soap-client' that Excorporate
;; bundles is backward compatible.

;; Acknowledgments:

;; Alexandru Harsanyi <AlexHarsanyi@gmail.com> provided help and
;; guidance on how to extend soap-client.el's WSDL and XSD handling,
;; enabling support for the full Exchange Web Services API.

;; Alex Luccisano <casual.lexicon@gmail.com> tested early versions of
;; this library against a corporate installation of Exchange.

;; Jon Miller <jonebird@gmail.com> tested against Exchange 2013.  He
;; also tracked down and reported a bad interaction with other
;; packages that require soap-client.

;; Nicolas Lamirault <nicolas.lamirault@gmail.com> tested the
;; autodiscovery feature.

;; Trey Jackson <bigfaceworm@gmail.com> confirmed autodiscovery worked
;; for him.

;; Joakim Verona <joakim@verona.se> tested autodiscovery in a
;; Kerberos/GSSAPI environment.

;; Wilfred Hughes <me@wilfred.me.uk> tested on Exchange 2007 and
;; suggested documentation improvements.

;; Erik Hetzner <egh@e6h.org> tested on Office 365 and helped debug
;; Office 365 support.

;; Fabio Leimgruber <fabio.leimgruber@web.de> tested NTLM
;; authentication against a challenging server configuration.

;; Stefan Monnier <monnier@iro.umontreal.ca> wrote a variant of
;; nadvice.el for GNU ELPA so that Excorporate could continue
;; supporting Emacs versions 24.1, 24.2 and 24.3.

;;; Code:

;; Implementation-visible functions and variables.

;; Add NTLM authorization scheme.
(require 'url-http-ntlm)
(require 'soap-client)
(require 'fsm)
(require 'excorporate-calendar)
(require 'org)

(defconst exco--autodiscovery-templates
  '("https://%s/autodiscover/autodiscover.svc"
    "https://autodiscover.%s/autodiscover/autodiscover.svc")
  "Autodiscovery URL templates.
URL templates to be formatted with a domain name, then searched
for autodiscovery files.")

(defvar exco--connections nil
  "A hash table of finite state machines.
The key is the identifier passed to `exco-connect'.  Each finite
state machine represents a service connection.")

(defvar exco--connection-identifiers nil
  "An ordered list of connection identifiers.")

(defun exco--parse-xml-in-current-buffer ()
  "Decode and parse the XML contents of the current buffer."
  (let ((mime-part (mm-dissect-buffer t t)))
    (unless mime-part
      (error "Failed to decode response from server"))
    (unless (equal (car (mm-handle-type mime-part)) "text/xml")
      (error "Server response is not an XML document"))
    (with-temp-buffer
      (mm-insert-part mime-part)
      (prog1
	  (car (xml-parse-region (point-min) (point-max)))
	(kill-buffer)
	(mm-destroy-part mime-part)))))

(defun exco--bind-wsdl (wsdl service-url port-name target-namespace
			     binding-name)
  "Create a WSDL binding.
Create a binding port for WSDL from SERVICE-URL, PORT-NAME,
TARGET-NAMESPACE and BINDING-NAME."
  (let* ((namespace (soap-wsdl-find-namespace target-namespace wsdl))
	 (port (make-soap-port
		:name port-name
		:binding (cons target-namespace binding-name)
		:service-url service-url)))
    (soap-namespace-put port namespace)
    (push port (soap-wsdl-ports wsdl))
    (soap-resolve-references port wsdl)
    wsdl))

(defun exco--handle-url-error (url status)
  "Handle an error that occurred when retrieving URL.
The details of the error are in STATUS, in the same format as the
argument to a `url-retrieve' callback.  Return non-nil to retry,
nil to continue."
  (if (eq (cl-third (plist-get status :error)) 500)
      ;; The server reported an internal server error.  Try to recover
      ;; by re-requesting the target URL and its most recent redirect.
      ;; I'm not sure what conditions cause the server to get into
      ;; this state -- it might be because the server has stale
      ;; knowledge of old keepalive connections -- but this should
      ;; recover it.  We need to disable ntlm in
      ;; url-registered-auth-schemes so that it doesn't prevent
      ;; setting keepalives to nil.
      (let ((url-registered-auth-schemes nil)
	    (url-http-attempt-keepalives nil)
	    (redirect (plist-get status :redirect)))
	(fsm-debug-output "exco--fsm received 500 error for %s" url)
	(url-debug 'excorporate "Attempting 500 recovery")
	(ignore-errors
	  ;; Emacs's url-retrieve does not respect the values of
	  ;; url-http-attempt-keepalives and
	  ;; url-registered-auth-schemes in asynchronous contexts.
	  ;; Unless url.el is eventually changed to do so, the
	  ;; following requests must be synchronous so that they run
	  ;; entirely within url-http-attempt-keepalives's dynamic
	  ;; extent.  These calls block the main event loop,
	  ;; unfortunately, but only in this rare error recovery
	  ;; scenario.
	  (url-retrieve-synchronously url)
	  (when redirect (url-retrieve-synchronously redirect)))
	(url-debug 'excorporate "Done 500 recovery attempt")
	;; Retry.
	t)
    ;; We received some other error, which just
    ;; means we should try the next URL.
    (fsm-debug-output "exco--fsm didn't find %s" url)
    ;; Don't retry.
    nil))

(defun exco--retrieve-next-import (fsm state-data return-for next-state)
  "Retrieve the next XML schema import.
FSM is the finite state machine, STATE-DATA is FSM's state data,
and RETURN-FOR is one of :enter or :event to indicate what return
type the calling function expects.  NEXT-STATE is the next state
the FSM should transition to on success."
  (let* ((url (plist-get state-data :service-url))
	 (xml (plist-get state-data :service-xml))
	 (wsdl (plist-get state-data :service-wsdl))
	 (imports (soap-wsdl-xmlschema-imports wsdl))
	 (next-state (if imports :parsing-service-wsdl next-state)))
    (when imports
      (let ((import-url (url-expand-file-name (pop imports) url)))
	(let ((url-request-method "GET")
	      (url-package-name "soap-client.el")
	      (url-package-version "1.0")
	      (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
	      (url-http-attempt-keepalives t))
	  (url-retrieve
	   import-url
	   (lambda (status)
	     (let ((data-buffer (current-buffer)))
	       (unwind-protect
		   (progn
		     (url-debug 'excorporate "Processing import %s" status)
		     (if (eq (car status) :error)
			 ;; There is an error.  It may be recoverable
			 ;; if it's HTTP 500 (internal server error).
			 (if (and (exco--handle-url-error import-url status)
				  ;; Only retry once.
				  (not (plist-get state-data :retrying)))
			     ;; We should retry.  Don't save the
			     ;; popped urls list to state-data, so
			     ;; that this :try-next-url will
			     ;; re-attempt to retrieve the same car as
			     ;; before.  Set the retry flag.
			     (progn
			       (plist-put state-data :retrying t))
			   ;; Save the popped urls list so that the next url
			   ;; is attempted, and clear the retry flag.
			   (plist-put state-data :retrying nil)
			   (setf (soap-wsdl-xmlschema-imports wsdl) imports)
			   (plist-put state-data :failure-message
				      (format "Failed to retrieve %s"
					      import-url))
			   (fsm-send fsm :unrecoverable-error))
		       ;; Success, parse WSDL.
		       (plist-put state-data :retrying nil)
		       (setf (soap-wsdl-xmlschema-imports wsdl) imports)
		       (soap-with-local-xmlns xml
			 (soap-wsdl-add-namespace
			  (soap-parse-schema (soap-parse-server-response) wsdl)
			  wsdl))
		       (plist-put state-data :service-wsdl wsdl)))
		 (and (buffer-live-p data-buffer)
		      (kill-buffer data-buffer))))
	     (fsm-send fsm t))))))
    (if (eq return-for :enter)
	(list state-data nil)
      (list next-state state-data nil))))

(define-state-machine exco--fsm :start
  ((identifier)
   "Start an Excorporate finite state machine."
   (if (stringp identifier)
       (let ((domain (cadr (split-string identifier "@"))))
	 (unless (and domain (not (equal domain "")))
	   (error "Invalid domain for address %s" identifier))
	 (list :retrieving-autodiscovery-xml
	       (list
		;; State machine data.
		;; Unique finite state machine identifier.  Either mail-address
		;; or (mail-address . service-url).  The latter allows multiple
		;; state machines to operate on the same service URL.  Login
		;; credentials are handled separately by auth-source and url,
		;; so these should be the only two identifier types needed here.
		:identifier identifier
		;; User data.
		:mail-address identifier
		;; Error recovery data.
		:retrying nil
		;; Autodiscovery data.
		:autodiscovery-urls
		(append (mapcar (lambda (template)
				  (format template domain))
				exco--autodiscovery-templates)
			;; Handle the user@sub.domain.com =>
			;; autodiscover.domain.com case reported by a
			;; user.  Only try one extra level.
			(let ((domain-parts (split-string domain "\\.")))
			  (when (> (length domain-parts) 2)
			    (mapcar (lambda (template)
				      (format template
					      (mapconcat
					       'identity
					       (cdr domain-parts) ".")))
				    exco--autodiscovery-templates))))
		;; Service data.
		:service-url nil
		:service-xml nil
		:service-wsdl nil
		;; State data.
		:next-state-after-success nil
		:failure-message nil
		:server-version nil)
	       ;; No timeout.
	       nil))
     ;; Go directly to :retrieving-service-xml, skipping autodiscovery.
     (list :retrieving-service-xml
	   (list
	    :identifier identifier
	    :mail-address (car identifier)
	    :retrying nil
	    :autodiscovery-urls nil
	    ;; Use service-url field from identifier.
	    :service-url (cdr identifier)
	    :service-xml nil
	    :service-wsdl nil
	    :next-state-after-success nil
	    :failure-message nil
	    :server-version nil)
	   ;; No timeout.
	   nil))))

(define-state exco--fsm :retrieving-autodiscovery-xml
  (fsm state-data event _callback)
  (cl-case event
    (:try-next-url
     (let ((urls (plist-get state-data :autodiscovery-urls)))
       (if urls
	   (let ((url (pop urls)))
	     (fsm-debug-output "exco--fsm will probe %s" url)
	     (condition-case nil
		 (url-retrieve
		  url
		  (lambda (status)
		    (let ((data-buffer (current-buffer)))
		      (unwind-protect
			  (progn
			    (url-debug 'excorporate
			     "Processing status: %s" status)
			    (if (eq (car status) :error)
				(progn
				  (if (and
				       (exco--handle-url-error url status)
				       ;; Only retry once.
				       (not (plist-get state-data :retrying)))
				      ;; We should retry.  Don't save the popped
				      ;; urls list to state-data, so that this
				      ;; :try-next-url will re-attempt to
				      ;; retrieve the same car as before.  Set
				      ;; the retry flag.
				      (plist-put state-data :retrying t)
				    ;; Save the popped urls list so that the
				    ;; next url is attempted, and clear the
				    ;; retry flag.
				    (plist-put state-data :retrying nil)
				    (plist-put state-data
					       :autodiscovery-urls urls))
				  ;; Try next or retry.
				  (fsm-send fsm :try-next-url))
			      ;; Success, save URL and parse returned XML.
			      (message
			       "Excorporate: Found autodiscovery URL for %S: %s"
			       (plist-get state-data :identifier) url)
			      (plist-put state-data :retrying nil)
			      (plist-put state-data :service-url url)
			      (plist-put state-data :service-xml
					 (exco--parse-xml-in-current-buffer))
			      (fsm-send fsm :success))
			    (url-debug 'excorporate "Done processing status"))
			(and (buffer-live-p data-buffer)
			     (kill-buffer data-buffer))))))
	       (error
		(fsm-debug-output "exco--fsm connection refused for %s" url)
		(plist-put state-data :retrying nil)
		(plist-put state-data :autodiscovery-urls urls)
		(fsm-send fsm :try-next-url)))
	     (list :retrieving-autodiscovery-xml state-data nil))
	 (plist-put state-data :failure-message
		    "Autodiscovery ran out of URLs to try")
	 (list :shutting-down-on-error state-data nil))))
    (:success
     (plist-put state-data :next-state-after-success :retrieving-service-xml)
     (list :parsing-service-wsdl state-data nil))))

(define-enter-state exco--fsm :shutting-down-on-error
  (_fsm state-data)
  (let ((failure-message (plist-get state-data :failure-message)))
    (exco-disconnect (plist-get state-data :identifier))
    (message "Excorporate: %s" failure-message)
    (url-debug 'excorporate "Failed: %s" failure-message)
    (fsm-debug-output "exco--fsm failed: %s" failure-message))
  (list state-data nil))

(define-state exco--fsm :shutting-down-on-error
  (_fsm state-data _event _callback)
  (list :shutting-down-on-error state-data nil))

(define-enter-state exco--fsm :retrieving-service-xml
  (fsm state-data)
  (when (stringp (plist-get state-data :identifier))
    (let* ((xml (plist-get state-data :service-xml))
	   (unbound-wsdl (plist-get state-data :service-wsdl))
	   (wsdl
	    (progn
	      ;; Skip soap-parse-wsdl-phase-fetch-schema to avoid
	      ;; synchronous URL fetches.
	      (soap-parse-wsdl-phase-finish-parsing xml unbound-wsdl)
	      (exco--bind-wsdl
	       (soap-wsdl-resolve-references unbound-wsdl)
	       (plist-get state-data :service-url)
	       "AutodiscoverServicePort"
	       "http://schemas.microsoft.com/exchange/2010/Autodiscover"
	       "DefaultBinding_Autodiscover"))))
      (soap-invoke-async
       (lambda (response)
	 (let ((result-url
		(exco-extract-value '(Response
				      UserResponses
				      UserResponse
				      UserSettings
				      UserSetting
				      Value)
				    response)))
	   (if result-url
	       (progn
		 (plist-put state-data :service-url result-url)
		 (message "Excorporate: Found service URL for %S: %s"
			  (plist-get state-data :identifier)
			  (plist-get state-data :service-url)))
	     ;; No result.  Check for error.
	     (let ((error-message
		    (exco-extract-value '(Response
					  UserResponses
					  UserResponse
					  ErrorMessage)
					response)))
	       (if error-message
		   (message "Excorporate: %s" error-message)
		 (message "Excorporate: Failed to find service URL"))))
	   (fsm-send fsm :retrieve-xml)))
       nil
       wsdl
       "AutodiscoverServicePort"
       "GetUserSettings"
       `((RequestedServerVersion . "Exchange2010")
	 (Request
	  (Users
	   (User
	    (Mailbox . ,(plist-get state-data :mail-address))))
	  (RequestedSettings
	   (Setting . "InternalEwsUrl")))))))
  (list state-data nil))

(define-state exco--fsm :retrieving-service-xml
  (fsm state-data event _callback)
  (cl-case event
    (:unrecoverable-error
     (list :shutting-down-on-error state-data nil))
    (:retrieve-xml
     (let* ((service-url (plist-get state-data :service-url))
	    (wsdl-url (replace-regexp-in-string "/[^/]*$" "/Services.wsdl"
						service-url)))
       (url-retrieve wsdl-url
		     (lambda (status)
		       (let ((data-buffer (current-buffer)))
			 (unwind-protect
			     (if (eq (car status) :error)
				 (progn
				   (plist-put state-data :failure-message
					      (format "Failed to retrieve %s"
						      wsdl-url))
				   (fsm-send fsm :unrecoverable-error))
			       (plist-put state-data
					  :service-xml
					  (exco--parse-xml-in-current-buffer))
			       (fsm-send fsm :success))
			   (and (buffer-live-p data-buffer)
				(kill-buffer data-buffer)))))))
     (list :retrieving-service-xml state-data nil))
    (:success
     (plist-put state-data :next-state-after-success :retrieving-data)
     (list :parsing-service-wsdl state-data nil))))

(define-enter-state exco--fsm :parsing-service-wsdl
  (fsm state-data)
  (let* ((url (plist-get state-data :service-url))
	 (xml (plist-get state-data :service-xml))
	 (next-state (plist-get state-data :next-state-after-success))
	 (wsdl (soap-make-wsdl url)))
    (soap-parse-wsdl-phase-validate-node xml)
    ;; Skip soap-parse-wsdl-phase-fetch-imports to avoid synchronous
    ;; fetches of import URLs.
    (soap-parse-wsdl-phase-parse-schema xml wsdl)
    (plist-put state-data :service-wsdl wsdl)
    (exco--retrieve-next-import fsm state-data :enter next-state)))

(define-state exco--fsm :parsing-service-wsdl
  (fsm state-data event _callback)
  (if (eq event :unrecoverable-error)
      (list :shutting-down-on-error state-data nil)
    (let ((next-state (plist-get state-data :next-state-after-success)))
      (exco--retrieve-next-import fsm state-data :event next-state))))

(defun exco--get-server-version (wsdl)
  "Extract server version from WSDL."
  (let ((warning-message "Excorporate: Failed to determine server version")
	(namespace "http://schemas.microsoft.com/exchange/services/2006/types")
	(name "RequestServerVersion")
	(found-version nil))
    (unwind-protect
	(setq found-version
	      (catch 'found
		(dolist (attribute
			 (soap-xs-type-attributes
			  (soap-xs-element-type (soap-wsdl-get
						 `(,namespace . ,name)
						 wsdl 'soap-xs-element-p))))
		  (when (equal (soap-xs-attribute-name attribute) "Version")
		    (throw 'found (car (soap-xs-simple-type-enumeration
					(soap-xs-attribute-type attribute))))))
		(warn warning-message)
		nil))
      (if found-version
	  found-version
	(warn warning-message)
	nil))))

(define-enter-state exco--fsm :retrieving-data
  (_fsm state-data)
  (let ((wsdl (plist-get state-data :service-wsdl))
	(identifier (plist-get state-data :identifier)))
    ;; Skip soap-parse-wsdl-phase-fetch-schema to avoid synchronous
    ;; URL fetches.
    (soap-parse-wsdl-phase-finish-parsing (plist-get state-data :service-xml)
					  wsdl)
    (exco--bind-wsdl
     (soap-wsdl-resolve-references wsdl)
     (plist-get state-data :service-url)
     "ExchangeServicePort"
     "http://schemas.microsoft.com/exchange/services/2006/messages"
     "ExchangeServiceBinding")
    (plist-put state-data :server-version (exco--get-server-version wsdl))
    (fsm-debug-output "exco--fsm %s server version is %s"
		      identifier (exco-server-version identifier))
    (message "Excorporate: Connection %S is ready" identifier))
  (list state-data nil))

(define-state exco--fsm :retrieving-data
  (_fsm state-data event _callback)
  (let* ((identifier (plist-get state-data :identifier))
	 (wsdl (plist-get state-data :service-wsdl))
	 (name (pop event))
	 (arguments (pop event))
	 (callback (pop event)))
    (apply #'soap-invoke-async
	   (lambda (response)
	     (funcall callback identifier response))
	   nil
	   wsdl
	   "ExchangeServicePort"
	   name
	   arguments))
  (list :retrieving-data state-data nil))

(defun exco--ensure-connection ()
  "Ensure at least one connection exists or throw an error."
  (unless exco--connection-identifiers
    (error "Excorporate: No connections exist.  Run M-x excorporate")))

(defmacro exco--with-fsm (identifier &rest body)
  "With `fsm' set to IDENTIFIER, run BODY.
Run BODY with `fsm' set to the finite state machine specified by
IDENTIFIER."
  (declare (indent 1) (debug t))
  `(progn
     (exco--ensure-connection)
     (let ((fsm (gethash ,identifier exco--connections)))
       (unless fsm
	 (error "Excorporate: Connection %S does not exist" ,identifier))
       ,@body)))

;; Developer-visible functions and variables.

(defun exco-api-version ()
  "Return the Excorporate API version.
Return a non-negative integer representing the current
Excorporate application programming interface version.  Version 0
is subject to change."
  0)

(defun exco-connect (identifier)
  "Connect or reconnect to a web service.
IDENTIFIER is the mail address to use for autodiscovery or a
pair (mail-address . service-url)."
  (if (stringp identifier)
      (message "Excorporate: Starting autodiscovery for %S"
	       identifier))
  (let ((fsm (start-exco--fsm identifier)))
    (unless exco--connections
      (setq exco--connections (make-hash-table :test 'equal)))
    (when (gethash identifier exco--connections)
      (exco-disconnect identifier))
    (puthash identifier fsm exco--connections)
    (push identifier exco--connection-identifiers)
    (if (stringp identifier)
	(fsm-send fsm :try-next-url)
      (fsm-send fsm :retrieve-xml))
    nil))

(defun exco-operate (identifier name arguments callback)
  "Execute a service operation asynchronously.
IDENTIFIER is the connection identifier.  Execute operation NAME
with ARGUMENTS then call CALLBACK with two arguments, IDENTIFIER
and the server's response."
  (exco--with-fsm identifier
    (fsm-send fsm (list name arguments callback)))
  nil)

(defun exco-server-version (identifier)
  "Return the server version for connection IDENTIFIER, as a string.
Examples are \"Exchange2010\", \"Exchange2010_SP1\",
\"Exchange2013\"."
  (exco--with-fsm identifier
    (plist-get (fsm-get-state-data fsm) :server-version)))

(defun exco-disconnect (identifier)
  "Disconnect from a web service.
IDENTIFIER is the mail address used to look up the connection."
  (exco--with-fsm identifier
    (setq exco--connection-identifiers
	  (delete identifier exco--connection-identifiers))
    (remhash identifier exco--connections))
  nil)

(defun exco-extract-value (path result)
  "Extract the value at PATH from RESULT.
PATH is an ordered list of node names."
  (let ((values (nreverse (car result))))
    (dolist (path-element path)
      (setq values (assoc path-element values)))
    (cdr values)))

(defun exco-calendar-item-get-details (identifier item-identifier process-item)
  "Query server for details about ITEM-IDENTIFIER.
IDENTIFIER is the connection identifier.  Call PROCESS-ITEM with
argument ICALENDAR-TEXT."
  (exco-operate identifier
		"GetItem"
		`(((ItemShape
		    (BaseShape . "IdOnly")
		    (IncludeMimeContent . t))
		   (ItemIds ,item-identifier))
		  nil nil nil nil nil nil)
		(lambda (_identifier response)
		  (let* ((mime-path '(ResponseMessages
				      GetItemResponseMessage
				      Items
				      CalendarItem
				      MimeContent))
			 (character-set-path (append mime-path '(CharacterSet)))
			 (coding-system (intern (downcase (exco-extract-value
							   character-set-path
							   response)))))
		    (unless (member coding-system coding-system-list)
		      (error "Unrecognized coding system: %s"
			     (exco-extract-value character-set-path response)))
		    (funcall process-item (decode-coding-string
					   (base64-decode-string
					    (cdr (exco-extract-value
						  mime-path response)))
					   coding-system))))))

(defmacro exco--calendar-item-dolist (item items &rest forms)
  "Iterate through ITEMS.
On each iteration, ITEM is set, and FORMS are run."
  `(dolist (,item ,items)
     (let* ((subject (cdr (assoc 'Subject ,item)))
	    (start (cdr (assoc 'Start ,item)))
	    (start-internal (apply #'encode-time
				   (soap-decode-date-time
				    start 'dateTime)))
	    (end (cdr (assoc 'End ,item)))
	    (end-internal (apply #'encode-time
				 (soap-decode-date-time
				  end 'dateTime)))
	    (location (cdr (assoc 'Location ,item)))
	    (to-invitees (cdr (assoc 'DisplayTo ,item)))
	    (main-invitees (when to-invitees
			     (mapcar 'org-trim
				     (split-string to-invitees ";"))))
	    (cc-invitees (cdr (assoc 'DisplayCc ,item)))
	    (optional-invitees (when cc-invitees
				 (mapcar 'org-trim
					 (split-string cc-invitees ";"))))
	    (item-identifier (assoc 'ItemId ,item)))
       ,@forms)))

(defun exco-calendar-item-with-details-iterate (identifier
						response
						callback
						finalize)
  "Iterate through calendar items in RESPONSE, calling CALLBACK on each.
IDENTIFIER identifies the connection.

CALLBACK takes the following arguments: FINALIZE, which is the
FINALIZE argument to this function wrapped in a countdown,
SUBJECT, a string, the subject of the meeting, START, the start
date and time in Emacs internal representation, END, the start
date and time in Emacs internal representation, LOCATION, the
location of the meeting, MAIN-INVITEES, a list of strings
representing required participants, OPTIONAL-INVITEES, a list of
strings representing optional participants, DETAILS is the
meeting request message body, and ICALENDAR-TEXT, the iCalendar
text representing the meeting series.

CALLBACK must arrange for FINALIZE to be called after its main
processing is done."
  (let* ((items (exco-extract-value '(ResponseMessages
				      FindItemResponseMessage
				      RootFolder
				      Items)
				    response))
	 (countdown (length items))
	 (finalizer
	  (lambda (&rest arguments)
	    (setq countdown (1- countdown))
	    (when (equal countdown 0)
	      (apply finalize arguments)))))
    (if (equal countdown 0)
	(funcall finalize)
      (exco--calendar-item-dolist
       calendar-item items
       (exco-calendar-item-get-details
	identifier item-identifier
	(lambda (icalendar-text)
	  (funcall callback finalizer subject start-internal end-internal
		   location main-invitees optional-invitees
		   icalendar-text)))))))

(defun exco-calendar-item-iterate (response callback)
  "Iterate through calendar items in RESPONSE, calling CALLBACK on each.
Returns a list of results from callback.  CALLBACK takes arguments:
SUBJECT, a string, the subject of the meeting.
START, the start date and time in Emacs internal representation.
END, the start date and time in Emacs internal representation.
LOCATION, the location of the meeting.
MAIN-INVITEES, a list of strings representing required participants.
OPTIONAL-INVITEES, a list of strings representing optional participants."
  (let ((result-list '()))
    (exco--calendar-item-dolist
     calendar-item (exco-extract-value '(ResponseMessages
					 FindItemResponseMessage
					 RootFolder
					 Items)
				       response)
     ;; Silence byte compiler unused warning.
     item-identifier
     (push (funcall callback subject start-internal end-internal
		    location main-invitees optional-invitees)
	   result-list))
    (nreverse result-list)))

;; Date-time utility functions.
(defun exco-extend-timezone (date-time-string)
  "Add a colon to the timezone in DATE-TIME-STRING.
DATE-TIME-STRING must be formatted as if returned by
`format-time-string' with FORMAT-STRING \"%FT%T%z\".  Web
services require the ISO8601 extended format of timezone, which
includes the colon."
  (concat
   (substring date-time-string 0 22) ":" (substring date-time-string 22)))

(defun exco-format-date-time (time-internal)
  "Convert TIME-INTERNAL to an XSD compatible date-time string."
  (exco-extend-timezone
   (format-time-string "%FT%T%z" time-internal)))

;; Use month day year order to be compatible with
;; calendar-cursor-to-date.  I wish I could instead use the ISO 8601
;; ordering, year month day.
(defun exco-get-meetings-for-day (identifier month day year callback)
  "Return the meetings for the specified day.
IDENTIFIER is the connection identifier.  MONTH, DAY and YEAR are
the meeting month, day and year.  Call CALLBACK with two
arguments, IDENTIFIER and the server's response."
  (let* ((start-of-day-time-internal
	  (apply #'encode-time `(0 0 0 ,day ,month ,year)))
	 (start-of-day-date-time
	  (exco-format-date-time start-of-day-time-internal))
	 (start-of-next-day-date-time
	  (exco-extend-timezone
	   (format-time-string "%FT00:00:00%z"
			       (time-add start-of-day-time-internal
					 (seconds-to-time 86400))))))
    (exco-operate
     identifier
     "FindItem"
     `(;; Main arguments.
       (;; RequestVersion is usually overridden by a fixed value in
	;; the WSDL (the RequestServerVersion element); provide the
	;; maximally-compatible Exchange2007 if the fixed value isn't
	;; present.
	(RequestVersion (Version . "Exchange2007"))
	(Traversal . "Shallow")
	(ItemShape
	 (BaseShape . "AllProperties"))
	;; To aid productivity, excorporate-calfw automatically prunes your
	;; meetings to a maximum of 100 per day.
	(CalendarView (MaxEntriesReturned . "100")
		      (StartDate . ,start-of-day-date-time)
		      (EndDate . ,start-of-next-day-date-time))
	(ParentFolderIds
	 (DistinguishedFolderId (Id . "calendar"))))
       ;; Empty arguments.
       ,@(let* ((wsdl (exco--with-fsm identifier
				      (plist-get (fsm-get-state-data fsm)
						 :service-wsdl)))
		(arity (soap-operation-arity wsdl
					    "ExchangeServicePort"
					    "FindItem")))
	   (make-list (- arity 1) nil)))
     callback)))

(defun exco-connection-iterate (initialize-function
				per-connection-function
				per-connection-callback
				finalize-function
				&optional callback-will-call-finalize)
  "Iterate Excorporate connections.
Call INITIALIZE-FUNCTION once before iterating.  It takes no
arguments.

Call PER-CONNECTION-FUNCTION once for each server connection.  It
is run synchronously.  It accepts two arguments, IDENTIFIER, the
current server connection, and CALLBACK, which is a wrapped
version of PER-CONNECTION-CALLBACK.

PER-CONNECTION-CALLBACK takes a variable number of arguments,
depending on which callback it is.  If
CALLBACK-WILL-CALL-FINALIZE is non-nil, it takes a final
FINALIZE-FUNCTION argument, which is a countdown-wrapped
finalizer function that PER-CONNECTION-CALLBACK should call (or
arrange to be called asynchronously) each time it is invoked.

If CALLBACK-WILL-CALL-FINALIZE is non-nil, this function will not
call FINALIZE-FUNCTION itself.  Instead it will wrap
FINALIZE-FUNCTION into a function that can be called once per
connection, then pass the wrapped finalizer to the callback as an
argument.  CALLBACK-WILL-CALL-FINALIZE must be set if the
callback needs to make a recursive asynchronous call."
  (exco--ensure-connection)
  (funcall initialize-function)
  (let* ((countdown (length exco--connection-identifiers))
	 (wrapped-finalizer
	  (lambda (&rest arguments)
	    (setq countdown (1- countdown))
	    (when (equal countdown 0)
	      (apply finalize-function arguments))))
	 (wrapped-callback
	  (lambda (&rest arguments)
	    (apply per-connection-callback
		   (append arguments
			   (when callback-will-call-finalize
			     (list wrapped-finalizer))))
	    (unless callback-will-call-finalize
	      (funcall wrapped-finalizer)))))
    (dolist (identifier exco--connection-identifiers)
      (funcall per-connection-function identifier
	       wrapped-callback))))

;; User-visible functions and variables.
(defgroup excorporate nil
  "Exchange support."
  :version "25.1"
  :group 'comm
  :group 'calendar)

;; Name the excorporate-configuration variable vaguely.  It is currently a
;; MAIL-ADDRESS string, a pair (MAIL-ADDRESS . SERVICE-URL), or nil.  In the
;; future it could allow a list of strings and pairs.
(defcustom excorporate-configuration nil
  "Excorporate configuration.
The mail address to use for autodiscovery."
  :type '(choice
	  (const
	   :tag "Prompt for Exchange mail address to use for autodiscovery" nil)
	  (string :tag "Exchange mail address to use for autodiscovery")
	  (cons :tag "Skip autodiscovery"
		(string :tag "Exchange mail address (e.g., hacker@gnu.org)")
		(string :tag "Exchange Web Services URL\
 (e.g., https://mail.gnu.org/ews/exchange.asmx)"))))

;;;###autoload
(defun excorporate ()
  "Start Excorporate.
Prompt for a mail address to use for autodiscovery, with an
initial suggestion of `user-mail-address'.  However, if
`excorporate-configuration' is non-nil, `excorporate' will use
that without prompting."
  (interactive)
  (cond
   ((eq excorporate-configuration nil)
    (exco-connect (completing-read "Exchange mail address: "
				   (list user-mail-address)
				   nil nil user-mail-address)))
   ((stringp excorporate-configuration)
    (exco-connect excorporate-configuration))
   ((null (consp (cdr excorporate-configuration)))
    (exco-connect excorporate-configuration))
   (t
    (error "Excorporate: Invalid configuration"))))

(provide 'excorporate)

;;; excorporate.el ends here
