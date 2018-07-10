;;; pfuture.el --- a simple wrapper around asynchronous processes -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Homepage: https://github.com/Alexander-Miller/pfuture
;; Package-Requires: ((emacs "25.2"))
;; Package-Version: 20180628.2232
;; Version: 1.2.2

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

;;; Code:

(require 'cl-lib)

;;;###autoload
(defun pfuture-new (cmd &rest cmd-args)
  "Create a new future process for command CMD and arguments CMD-ARGS.
This will return a process object with one additional 'result property which
can be read via \(process-get process 'result\) or alternatively with
\(pfuture-result process\).

Note that CMD-ARGS must be a *sequence* of strings, meaning
this is wrong: (pfuture-new \"git status\")
this is right: (pfuture-new \"git\" \"status\")"
  (let* ((process-connection-type nil)
         (process (apply #'start-process "Process Future" nil cmd cmd-args)))
    (process-put process 'result "")
    (set-process-filter process #'pfuture--append-output)
    process))

(cl-defmacro pfuture-callback (command &key directory on-success on-error on-status-change name connection-type)
  "Pfuture variant that supports a callback-based workflow.
Internally based on `make-process'.

The first - and only required - argument is COMMAND. It is an (unquoted) list of
the command and the arguments for the process that should be started. A vector
is likewise acceptable, the difference is purely cosmetic.

The rest of the argument list is made up of the following keyword arguments:

ON-SUCCESS is the code that will run once the process has finished with an exit
code of 0. In its context, these variables are bound:
`process': The process object, as passed to the sentinel callback function.
`status': The string exit status, as passed to the sentinel callback function.
`output': The output of the process, including both stdin and stdout.

ON-SUCCESS may take one of 3 forms: an unquoted sexp, a quoted function or an
unquoted function. In the former two cases the passed fuction will be called
with `process', `status' and `output' as its parameters.

ON-FAILURE is the inverse to ON-SUCCESS; it will only run if the process has
finished with a non-zero exit code. Otherwise the same conditions apply as for
ON-SUCCESS.

ON-STATUS-CHANGE will run on every status change, even if the process remains
running. It is meant for debugging and has access to the same variables as
ON-SUCCESS and ON-ERROR, including the (potentially incomplete) process output.
Otherwise the same conditions as for ON-SUCCESS and ON-ERROR apply.

DIRECTORY is the value given to `default-directory' for the context of the
process. If not given it will fall back the current value of `default-directory'.

NAME will be passed to the :name property of `make-process'. If not given it will
fall back to \"Pfuture Callback [$COMMAND]\".

CONNECTION-TYPE will be passed to the :connection-process property of
`make-process'. If not given it will fall back to 'pipe."
  (declare (indent 1))
  (let* ((command (if (vectorp command)
                      (cl-map 'list #'identity command)
                    command))
         (name (or name (concat "Pfuture Callback: [" (mapconcat #'identity command " ") "]")))
         (connection-type (or connection-type (quote 'pipe)))
         (directory (or directory default-directory)))
    ;; When we receive a quoted function for on-success or on-error the macro doesn't actually
    ;; see a function symbol, it sees a cons in the form (function name) where `name' is the name
    ;; of the quoted function. If we want to accept quoted functions as well we need to manually bend
    ;; things into shape.
    (setq on-success
          (pcase on-success
            (`(function ,fn) fn)
            (_ on-success))
          on-error
          (pcase on-error
            (`(function ,fn) fn)
            (_ on-error))
          on-status-change
          (pcase on-status-change
            (`(function ,fn) fn)
            (_ on-status-change)))
    `(let ((default-directory ,directory))
       (make-process
        :name ,name
        :command ',command
        :connection-type ,connection-type
        :filter #'pfuture--append-output
        :sentinel (lambda (process status)
                    ,@(when on-status-change
                        `((let ((output (process-get process 'result)))
                            ,@(cl-typecase on-status-change
                                (function
                                 `((funcall (function ,on-status-change) process status output)))
                                (cons
                                 `(,on-status-change))))))
                    (unless (process-live-p process)
                      (let ((output (process-get process 'result)))
                        (if (= 0 (process-exit-status process))
                            ,@(cl-typecase on-success
                                (function
                                 `((funcall (function ,on-success) process status output)))
                                (cons
                                 `(,on-success)))
                          ,@(cl-typecase on-error
                              (function
                               `((funcall (function ,on-error) process status output)))
                              (cons
                               `(,on-error)))))))))))

(cl-defun pfuture-await (process &key (timeout 1) (just-this-one t))
  "Block until PROCESS has produced output and return it.

Will accept the following optional keyword arguments:

TIMEOUT: The timeout in seconds to wait for the process. May be a float to
specify fractional number of seconds. In case of a timeout nil will be returned.

JUST-THIS-ONE: When t only read from the process of FUTURE and no other. For
details see documentation of `accept-process-output'."
  (let (inhibit-quit)
    (accept-process-output
     process timeout nil just-this-one))
  (process-get process 'result))

(defun pfuture-await-to-finish (process)
  "Keep reading the output of PROCESS until it is done.
Same as `pfuture-await', but will keep reading (and blocking) so long as the
process is *alive*.

If the process never quits this method will block forever. Use with caution!"
  (let (inhibit-quit)
    (accept-process-output process nil nil t)
    (while (process-live-p process)
      (accept-process-output process nil nil t)))
  (process-get process 'result))

(defsubst pfuture-result (process)
  "Return the output of PROCESS."
  (process-get process 'result))

(defun pfuture--append-output (process msg)
  "Append PROCESS' MSG to the already saved output."
  (process-put process 'result (concat (process-get process 'result) msg)))

(provide 'pfuture)

;;; pfuture.el ends here
