;;; command-expansion.el --- tests for TeX command expansion

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:

(require 'ert)
(require 'tex-buf)

(ert-deftest TeX-command-expansion ()
  "Check whether \"%%%%\" is correctly expanded when before \"%`\"."
  (should (string=
           (let ((TeX-command-list
		  (list (cons "Test" '("%%%% %`%'" TeX-run-command t t)))))
	     (TeX-command-expand (nth 1 (assoc "Test" TeX-command-list))
				 'TeX-master-file))
           "%%  \"\\input\"")))

(ert-deftest TeX-command-expansion-errors ()
  "Test error handling in `TeX-command-expand'."
  (should-error
   ;; This error is actually thrown by `TeX-engine-in-engine-alist', but we want
   ;; to be sure that `TeX-command-expand' fails when the engine is not valid.
   (let ((TeX-engine 'non-existing-engine))
     (TeX-command-expand "%l" 'TeX-master-file))))

(ert-deftest TeX-view-command-raw-errors ()
  "Tests to trigger errors in `TeX-view-command-raw'."
  ;; Viewer specification should be either a command line string or a Lisp
  ;; function name to be executed.  This test makes sure that the functions
  ;; throws an error if the selected viewer has a wrong specification (for
  ;; example a function call, not the function name) such that the returned
  ;; value `command' isn't a string.  This prevents an infinite loop in
  ;; `TeX-command-expand'.
  (should-error
   (with-temp-buffer
     (let ((TeX-view-program-list '(("viewer"
				     (wrong-specification))))
	   (TeX-view-program-selection
	    '((output-pdf "viewer"))))
       (TeX-mode)
       (TeX-view-command-raw)))
   :type 'error)
  ;; Signal an error when a nonexistent viewer is selected.
  (should-error
   (with-temp-buffer
     (let ((TeX-view-program-selection
	    '((output-pdf "does-not-exist"))))
       (TeX-mode)
       (TeX-view-command-raw)))
   :type 'error)
  ;; Signal an error if the binary associated to the viewer cannot be found.
  (should-error
   (with-temp-buffer
     (let ((TeX-view-program-list
	    '(("viewer" "viewer %o" "**this-program-does-not-exist**")))
	   (TeX-view-program-selection
	    '((output-pdf "viewer"))))
       (TeX-mode)
       (TeX-view-command-raw)))
   :type 'error)
  ;; Error if there is no selected viewer for current buffer.
  (should-error
   (with-temp-buffer
     (let (TeX-view-program-selection)
       (TeX-mode)
       (TeX-view-command-raw)))
   :type 'error))

;;; command-expansion.el ends here
