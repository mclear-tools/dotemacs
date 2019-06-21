;;; pfuture.el --- a simple wrapper around asynchronous processes -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Homepage: https://github.com/Alexander-Miller/pfuture
;; Package-Requires: ((emacs "25.2"))
;; Package-Version: 20190322.752
;; Version: 1.6

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
(require 'inline)

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

(defmacro pfuture--decompose-fn-form (fn &rest args)
  "Expands into the correct call form for FN and ARGS.
FN may either be a (sharp) quoted function, and unquoted function or an sexp."
  (declare (indent 1))
  (pcase fn
    (`(function ,fn)
     `(,fn ,@args))
    (`(quote ,fn)
     `(,fn ,@args))
    ((or `(,_ . ,_) `(,_))
     fn)
    ((pred null)
     (ignore fn))
    (fn
     `(funcall ,fn ,@args))))

(cl-defmacro pfuture-callback
    (command &key
             directory
             on-success
             on-error
             on-status-change
             name
             connection-type
             buffer
             filter)
  "Pfuture variant that supports a callback-based workflow.
Internally based on `make-process'. Requires lexical scope.

The first - and only required - argument is COMMAND. It is an (unquoted) list of
the command and the arguments for the process that should be started. A vector
is likewise acceptable - the difference is purely cosmetic (this does not apply
when command is passed as a variable, in this case it must be a list).

The rest of the argument list is made up of the following keyword arguments:

ON-SUCCESS is the code that will run once the process has finished with an exit
code of 0. In its context, these variables are bound:
`process': The process object, as passed to the sentinel callback function.
`status': The string exit status, as passed to the sentinel callback function.
`pfuture-buffer': The buffer where the output of the process is collected,
 including both stdin and stdout. You can use `pfuture-callback-output' to
 quickly grab the buffer's content.

ON-SUCCESS may take one of 3 forms: an unquoted sexp, a quoted function or an
unquoted function. In the former two cases the passed fuction will be called
with `process', `status' and `buffer' as its arguments.

ON-FAILURE is the inverse to ON-SUCCESS; it will only run if the process has
finished with a non-zero exit code. Otherwise the same conditions apply as for
ON-SUCCESS.

ON-STATUS-CHANGE will run on every status change, even if the process remains
running. It is meant for debugging and has access to the same variables as
ON-SUCCESS and ON-ERROR, including the (potentially incomplete) process output
buffer. Otherwise the same conditions as for ON-SUCCESS and ON-ERROR apply.

DIRECTORY is the value given to `default-directory' for the context of the
process. If not given it will fall back the current value of `default-directory'.

NAME will be passed to the :name property of `make-process'. If not given it will
fall back to \"Pfuture Callback [$COMMAND]\".

CONNECTION-TYPE will be passed to the :connection-process property of
`make-process'. If not given it will fall back to 'pipe.

BUFFER is the buffer that will be used by the process to collect its output,
quickly collectible with `pfuture-output-from-buffer'.
Providing a buffer outside of specific use-cases is not necessary, as by default
pfuture will assign every launched command its own unique buffer and kill it
after ON-SUCCESS or ON-ERROR have finished running. However, no such cleanup
will take place if a custom buffer is provided.

FILTER is a process filter-function (quoted function reference) that can be used
to overwrite pfuture's own filter. By default pfuture uses its filter function
to collect the launched process' output in its buffer, thus when providing a
custom filter output needs to be gathered another way. Note that the process'
buffer is stored in its `buffer' property and is therefore accessible via
\(process-get process 'buffer\)."
  (declare (indent 1))
  (let* ((command (if (vectorp command)
                      `(quote ,(cl-map 'list #'identity command))
                    command))
         (connection-type (or connection-type (quote 'pipe)))
         (directory (or directory default-directory)))
    (unless (or on-success on-error)
      (setq on-success '(function ignore)))
    `(let* ((default-directory ,directory)
            (name (or ,name (format "Pfuture-Callback %s" ,command)))
            (pfuture-buffer (or ,buffer (generate-new-buffer name)))
            (process
             (make-process
              :name name
              :command ,command
              :connection-type ,connection-type
              :filter ,(or filter '(function pfuture--append-output-to-buffer))
              :sentinel (lambda (process status)
                          (ignore status)
                          ,@(when on-status-change
                              `((pfuture--decompose-fn-form ,on-status-change
                                  process status pfuture-buffer)))
                          (unless (process-live-p process)
                            (if (= 0 (process-exit-status process))
                                (pfuture--decompose-fn-form ,on-success
                                  process status pfuture-buffer)
                              (pfuture--decompose-fn-form ,on-error
                                process status pfuture-buffer))
                            ,(unless buffer
                               `(kill-buffer (process-get process 'buffer))))))))
       (process-put process 'buffer pfuture-buffer)
       process)))

(defmacro pfuture-callback-output ()
  "Retrieve the output from the pfuture-buffer variable in the current scope.
Meant to be used with `pfuture-callback'."
  `(pfuture-output-from-buffer pfuture-buffer))

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

(define-inline pfuture-result (process)
  "Return the output of a pfuture PROCESS."
  (declare (side-effect-free t))
  (inline-letevals (process)
    (inline-quote
     (process-get ,process 'result))))

(defun pfuture--append-output (process msg)
  "Append PROCESS' MSG to the already saved output."
  (process-put process 'result (concat (process-get process 'result) msg)))

(defun pfuture--append-output-to-buffer (process msg)
  "Append PROCESS' MSG to its output buffer."
  (with-current-buffer (process-get process 'buffer)
    (goto-char (point-max))
    (insert msg)))

(define-inline pfuture-output-from-buffer (buffer)
  "Return the process output collected in BUFFER."
  (declare (side-effect-free t))
  (inline-letevals (buffer)
    (inline-quote
     (with-current-buffer ,buffer
       (buffer-string)))))

(provide 'pfuture)

;;; pfuture.el ends here
