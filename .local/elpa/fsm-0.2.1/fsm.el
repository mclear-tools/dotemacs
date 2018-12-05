;;; fsm.el --- state machine library  -*- lexical-binding: t; -*-

;; Copyright (C) 2006, 2007, 2008, 2015  Free Software Foundation, Inc.

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; Maintainer: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5"))
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; fsm.el is an exercise in metaprogramming inspired by gen_fsm of
;; Erlang/OTP.  It aims to make asynchronous programming in Emacs Lisp
;; easy and fun.  By "asynchronous" I mean that long-lasting tasks
;; don't interfer with normal editing.

;; Some people say that it would be nice if Emacs Lisp had threads
;; and/or continuations.  They are probably right, but there are few
;; things that can't be made to run in the background using facilities
;; already available: timers, filters and sentinels.  As the code can
;; become a bit messy when using such means, with callbacks everywhere
;; and such things, it can be useful to structure the program as a
;; state machine.

;; In this model, a state machine passes between different "states",
;; which are actually only different event handler functions.  The
;; state machine receives "events" (from timers, filters, user
;; requests, etc) and reacts to them, possibly entering another state,
;; possibly returning a value.

;; The essential macros/functions are:
;;
;; define-state-machine  - create start-FOO function
;; define-state          - event handler for each state (required)
;; define-enter-state    - called when entering a state (optional)
;; define-fsm            - encapsulates the above three (more sugar!)
;; fsm-send              - send an event to a state machine
;; fsm-call              - send an event and wait for reply

;; fsm.el is similar to but different from Distel:
;; <URL:http://fresh.homeunix.net/~luke/distel/>
;; Emacs' tq library is a similar idea.

;; Here is a simple (not using all the features of fsm.el) example:
;;
;; ;; -*- lexical-binding: t; -*-
;; (require 'fsm)
;; (cl-labels ((hey (n ev)
;;                  (message "%d (%s)\tp%sn%s!" n ev
;;                           (if (zerop (% n 4)) "o" "i")
;;                           (make-string (max 1 (abs n)) ?g))))
;;   (cl-macrolet ((zow (next timeout)
;;                      `(progn (hey (cl-incf count) event)
;;                              (list ,next count ,timeout))))
;;     (define-fsm pingpong
;;       :start ((init) "Start a pingpong fsm."
;;               (interactive "nInit (number, negative to auto-terminate): ")
;;               (list :ping (ash (ash init -2) 2) ; 4 is death
;;                     (when (interactive-p) 0)))
;;       :state-data-name count
;;       :states
;;       ((:ping
;;         (:event (zow :pingg 0.1)))
;;        (:pingg
;;         (:event (zow :pinggg 0.1)))
;;        (:pinggg
;;         (:event (zow :pong 1)))
;;        (:pong
;;         (:event (zow :ping (if (= 0 count)
;;                                (fsm-goodbye-cruel-world 'pingpong)
;;                              3))))))))
;; (fsm-send (start-pingpong -16) t)
;;
;; Copy into a buffer, uncomment, and type M-x eval-buffer RET.
;; Alternatively, you can replace the `fsm-goodbye-cruel-world'
;; form with `nil', eval just the `cl-labels' form and then type
;; M-x start-pingpong RET -16 RET.

;; Version 0.2:
;; -- Delete trailing whitespace.
;; -- Fix formatting.
;; -- Use lexical binding.
;; -- Port to cl-lib.
;; -- Remove unnecessary fsm-debug-output message.
;; -- Add FSM name to fsm-debug-output messages that were not including it.
;; -- Fix checkdoc errors.
;; -- Change FSMs from plists to uninterned symbols.

;; NOTE: This is version 0.1ttn4 of fsm.el, with the following
;; mods (an exercise in meta-meta-programming ;-) by ttn:
;; -- Refill for easy (traditional 80-column) perusal.
;; -- New var `fsm-debug-timestamp-format'.
;; -- Make variables satisfy `user-variable-p'.
;; -- Use `format' instead of `concat'.
;; -- New func `fsm-goodbye-cruel-world'.
;; -- Make start-function respect `interactive' spec.
;; -- Make enter-/event-functions anonymous.
;; -- New macro `define-fsm'.
;; -- Example usage in Commentary.

;;; Code:

;; We require cl-lib at runtime, since we insert `cl-destructuring-bind' into
;; modules that use fsm.el.
(require 'cl-lib)

(defvar fsm-debug "*fsm-debug*"
  "*Name of buffer for fsm debug messages.
If nil, don't output debug messages.")

(defvar fsm-debug-timestamp-format nil
  "*Timestamp format (a string) for `fsm-debug-output'.
Default format is whatever `current-time-string' returns
followed by a colon and a space.")

(defun fsm-debug-output (format &rest args)
  "Append debug output to buffer named by the variable `fsm-debug'.
FORMAT and ARGS are passed to `format'."
  (when fsm-debug
    (with-current-buffer (get-buffer-create fsm-debug)
      (save-excursion
	(goto-char (point-max))
	(insert (if fsm-debug-timestamp-format
		    (format-time-string fsm-debug-timestamp-format)
		  (concat (current-time-string) ": "))
		(apply 'format format args) "\n")))))

(cl-defmacro define-state-machine (name &key start sleep)
  "Define a state machine class called NAME.
A function called start-NAME is created, which uses the argument
list and body specified in the :start argument.  BODY should
return a list of the form (STATE STATE-DATA [TIMEOUT]), where
STATE is the initial state (defined by `define-state'),
STATE-DATA is any object, and TIMEOUT is the number of seconds
before a :timeout event will be sent to the state machine.  BODY
may refer to the instance being created through the dynamically
bound variable `fsm'.

SLEEP-FUNCTION, if provided, takes one argument, the number of
seconds to sleep while allowing events concerning this state
machine to happen.  There is probably no reason to change the
default, which is accept-process-output with rearranged
arguments.

\(fn NAME :start ((ARG ...) DOCSTRING BODY) [:sleep SLEEP-FUNCTION])"
  (declare (debug (&define name :name start
			   &rest
			   &or [":start"
				(lambda-list
				 [&optional ("interactive" interactive)]
				 stringp def-body)]
			   [":sleep" function-form])))
  (let ((start-name (intern (format "start-%s" name)))
	interactive-spec)
    (cl-destructuring-bind (arglist docstring &body body) start
      (when (and (consp (car body)) (eq 'interactive (caar body)))
	(setq interactive-spec (list (pop body))))
      (unless (stringp docstring)
	(error "Docstring is not a string"))
      `(progn
	 (put ',name :fsm-enter (make-hash-table :size 11 :test 'eq))
	 (put ',name :fsm-event (make-hash-table :size 11 :test 'eq))
	 (defun ,start-name ,arglist
	   ,docstring
	   ,@interactive-spec
	   (fsm-debug-output "Starting %s" ',name)
	   (let ((fsm (cl-gensym (concat "fsm-" ,(symbol-name name) "-"))))
	     (cl-destructuring-bind (state state-data &optional timeout)
		 (progn ,@body)
	       (put fsm :name ',name)
	       (put fsm :state nil)
	       (put fsm :state-data nil)
	       (put fsm :sleep ,(or sleep '(lambda (secs)
					     (accept-process-output
					      nil secs))))

	       (put fsm :deferred nil)
	       (fsm-update fsm state state-data timeout)
	       fsm)))))))

(cl-defmacro define-state (fsm-name state-name arglist &body body)
  "Define a state called STATE-NAME in the state machine FSM-NAME.
ARGLIST and BODY make a function that gets called when the state
machine receives an event in this state.  The arguments are:

FSM         the state machine instance (treat it as opaque)
STATE-DATA  An object
EVENT       The occurred event, an object.
CALLBACK    A function of one argument that expects the response
            to this event, if any (often `ignore' is used)

If the event should return a response, the state machine should
arrange to call CALLBACK at some point in the future (not necessarily
in this handler).

The function should return a list of the form (NEW-STATE
NEW-STATE-DATA TIMEOUT):

NEW-STATE      The next state, a symbol
NEW-STATE-DATA An object
TIMEOUT        A number: send timeout event after this many seconds
               nil: cancel existing timer
               :keep: let existing timer continue

Alternatively, the function may return the keyword :defer, in
which case the event will be resent when the state machine enters
another state."
  (declare (debug (&define name name :name handler lambda-list def-body)))
  `(setf (gethash ',state-name (get ',fsm-name :fsm-event))
	 (lambda ,arglist ,@body)))

(cl-defmacro define-enter-state (fsm-name state-name arglist &body body)
  "Define a function to call when FSM-NAME enters the state STATE-NAME.
ARGLIST and BODY make a function that gets called when the state
machine enters this state.  The arguments are:

FSM         the state machine instance (treat it as opaque)
STATE-DATA  An object

The function should return a list of the form (NEW-STATE-DATA
TIMEOUT):

NEW-STATE-DATA An object
TIMEOUT        A number: send timeout event after this many seconds
               nil: cancel existing timer
               :keep: let existing timer continue"
  (declare (debug (&define name name :name enter lambda-list def-body)))
  `(setf (gethash ',state-name (get ',fsm-name :fsm-enter))
	 (lambda ,arglist ,@body)))

(cl-defmacro define-fsm (name &key
			      start sleep states
			      (fsm-name 'fsm)
			      (state-data-name 'state-data)
			      (callback-name 'callback)
			      (event-name 'event))
  "Define a state machine class called NAME, along with its STATES.
This macro is (further) syntatic sugar for `define-state-machine',
`define-state' and `define-enter-state' macros, q.v.

NAME is a symbol.  Everything else is specified with a keyword arg.

START and SLEEP are the same as for `define-state-machine'.

STATES is a list, each element having the form (STATE-NAME . STATE-SPEC).
STATE-NAME is a symbol.  STATE-SPEC is an alist with keys `:event' or
`:enter', and values a series of expressions representing the BODY of
a `define-state' or `define-enter-state' call, respectively.

FSM-NAME, STATE-DATA-NAME, CALLBACK-NAME, and EVENT-NAME are symbols,
used to construct the state functions' arglists."
  `(progn
     (define-state-machine ,name :start ,start :sleep ,sleep)
     ,@(cl-loop for (state-name . spec) in states
		if (assq :enter spec) collect
		`(define-enter-state ,name ,state-name
		   (,fsm-name ,state-data-name)
		   ,@(cdr it))
		end
		if (assq :event spec) collect
		`(define-state ,name ,state-name
		   (,fsm-name ,state-data-name
			      ,event-name
			      ,callback-name)
		   ,@(cdr it))
		end)))

(defun fsm-goodbye-cruel-world (name)
  "Unbind functions related to fsm NAME (a symbol).
Includes start-NAME, and each fsm-NAME-STATE and fsm-NAME-enter-STATE.
Functions are `fmakunbound', which will probably give (fatal) pause to
any state machines using them.  Return nil."
  (interactive "SUnbind function definitions for fsm named: ")
  (fmakunbound (intern (format "start-%s" name)))
  (let (ht)
    (when (hash-table-p (setq ht (get name :fsm-event)))
      (clrhash ht)
      (cl-remprop name :fsm-event))
    (when (hash-table-p (setq ht (get name :fsm-enter)))
      (clrhash ht)
      (cl-remprop name :fsm-enter)))
  nil)

(defun fsm-start-timer (fsm secs)
  "Send a timeout event to FSM after SECS seconds.
The timer is canceled if another event occurs before, unless the
event handler explicitly asks to keep the timer."
  (fsm-stop-timer fsm)
  (put fsm
       :timeout (run-with-timer
		 secs nil
		 #'fsm-send-sync fsm :timeout)))

(defun fsm-stop-timer (fsm)
  "Stop the timeout timer of FSM."
  (let ((timer (get fsm :timeout)))
    (when (timerp timer)
      (cancel-timer timer)
      (put fsm :timeout nil))))

(defun fsm-maybe-change-timer (fsm timeout)
  "Change the timer of FSM according to TIMEOUT."
  (cond
   ((numberp timeout)
    (fsm-start-timer fsm timeout))
   ((null timeout)
    (fsm-stop-timer fsm))
   ;; :keep needs no timer change
   ))

(defun fsm-send (fsm event &optional callback)
  "Send EVENT to FSM asynchronously.
If the state machine generates a response, eventually call
CALLBACK with the response as only argument."
  (run-with-timer 0 nil #'fsm-send-sync fsm event callback))

(defun fsm-update (fsm new-state new-state-data timeout)
  "Update FSM with NEW-STATE, NEW-STATE-DATA and TIMEOUT."
  (let ((fsm-name (get fsm :name))
	(old-state (get fsm :state)))
    (put fsm :state new-state)
    (put fsm :state-data new-state-data)
    (fsm-maybe-change-timer fsm timeout)

    ;; On state change, call enter function and send deferred events
    ;; again.
    (unless (eq old-state new-state)
      (fsm-debug-output "%s enters %s" fsm-name new-state)
      (let ((enter-fn (gethash new-state (get fsm-name :fsm-enter))))
	(when (functionp enter-fn)
	  (fsm-debug-output "Found enter function for %s/%s" fsm-name new-state)
	  (condition-case e
	      (cl-destructuring-bind (newer-state-data newer-timeout)
		  (funcall enter-fn fsm new-state-data)
		(put fsm :state-data newer-state-data)
		(fsm-maybe-change-timer fsm newer-timeout))
	    ((debug error)
	     (fsm-debug-output "%s/%s update didn't work: %S"
			       fsm-name new-state e)))))

      (let ((deferred (nreverse (get fsm :deferred))))
	(put fsm :deferred nil)
	(dolist (event deferred)
	  (apply 'fsm-send-sync fsm event))))))

(defun fsm-send-sync (fsm event &optional callback)
  "Send EVENT to FSM synchronously.
If the state machine generates a response, eventually call
CALLBACK with the response as only argument."
  (save-match-data
    (let* ((fsm-name (get fsm :name))
	   (state (get fsm :state))
	   (state-data (get fsm :state-data))
	   (state-fn (gethash state (get fsm-name :fsm-event))))
      ;; If the event is a list, output only the car, to avoid an
      ;; overflowing debug buffer.
      (fsm-debug-output "Sent %S to %s in state %s"
			(or (car-safe event) event) fsm-name state)
      (let ((result (condition-case e
			(funcall state-fn fsm state-data event
				 (or callback 'ignore))
		      ((debug error) (cons :error-signaled e)))))
	;; Special case for deferring an event until next state change.
	(cond
	 ((eq result :defer)
	  (let ((deferred (get fsm :deferred)))
	    (put fsm :deferred (cons (list event callback) deferred))))
	 ((null result)
	  (fsm-debug-output "Warning: event %S ignored in state %s/%s"
			    event fsm-name state))
	 ((eq (car-safe result) :error-signaled)
	  (fsm-debug-output "Error in %s/%s: %s"
			    fsm-name state
			    (error-message-string (cdr result))))
	 ((and (listp result)
	       (<= 2 (length result))
	       (<= (length result) 3))
	  (cl-destructuring-bind (new-state new-state-data &optional timeout)
	      result
	    (fsm-update fsm new-state new-state-data timeout)))
	 (t
	  (fsm-debug-output "Incorrect return value in %s/%s: %S"
			    fsm-name state
			    result)))))))

(defun fsm-call (fsm event)
  "Send EVENT to FSM synchronously, and wait for a reply.
Return the reply.  `with-timeout' might be useful."
  (let (reply)
    (fsm-send-sync fsm event (lambda (r) (setq reply (list r))))
    (while (null reply)
      (fsm-sleep fsm 1))
    (car reply)))

(defun fsm-make-filter (fsm)
  "Return a filter function that sends events to FSM.
Events sent are of the form (:filter PROCESS STRING)."
  (let ((fsm fsm))
    (lambda (process string)
      (fsm-send-sync fsm (list :filter process string)))))

(defun fsm-make-sentinel (fsm)
  "Return a sentinel function that sends events to FSM.
Events sent are of the form (:sentinel PROCESS STRING)."
  (let ((fsm fsm))
    (lambda (process string)
      (fsm-send-sync fsm (list :sentinel process string)))))

(defun fsm-sleep (fsm secs)
  "Sleep up to SECS seconds in a way that lets FSM receive events."
  (funcall (get fsm :sleep) secs))

(defun fsm-get-state-data (fsm)
  "Return the state data of FSM.
Note the absence of a set function.  The fsm should manage its
state data itself; other code should just send messages to it."
  (get fsm :state-data))

;;;; ChangeLog:

;; 2016-07-10  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	packages/fsm: Bump version to 0.2.1
;; 
;; 2016-07-10  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	packages/fsm: Fix compilation error
;; 
;; 	* packages/fsm/fsm.el (define-state-machine): Quote default :sleep 
;; 	lambda (bug#23920).
;; 
;; 2015-09-05  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	fsm: Revert some changes suggested by checkdoc
;; 
;; 2015-09-05  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	fsm: Bump version to 0.2
;; 
;; 2015-09-05  Magnus Henoch  <magnus.henoch@gmail.com>
;; 
;; 	fsm: Change FSMs from plists to uninterned symbols
;; 
;; 2015-09-05  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	fsm: Fix copyright
;; 
;; 2015-09-05  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	fsm: Add packaging fields
;; 
;; 2015-09-05  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	fsm: Fix checkdoc errors
;; 
;; 2015-09-05  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	fsm: Add FSM name to some fsm-debug-output messages
;; 
;; 2015-09-05  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	fsm: Port to cl-lib
;; 
;; 2015-09-05  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	fsm: Use lexical binding
;; 
;; 2015-09-05  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	fsm: Fix formatting
;; 
;; 2015-09-05  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	fsm: Delete trailing whitespace
;; 
;; 2015-09-05  Thomas Fitzsimmons	<fitzsim@fitzsim.org>
;; 
;; 	fsm: Import fsm.el from emacs-jabber
;; 
;; 	Import fsm.el from git://git.code.sf.net/p/emacs-jabber/git, commit 
;; 	1f858cc4f3cdabcd7380a7d08af273bcdd708c15.
;; 


(provide 'fsm)

;;; fsm.el ends here
