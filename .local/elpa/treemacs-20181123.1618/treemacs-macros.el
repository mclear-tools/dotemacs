;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

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
;;; General purpose macros, and those used in, but defined outside of
;;; treemacs-impl.el are put here, to prevent using them before their
;;; definition, hopefully preventing issues like #97.

;;; Code:

(require 'dash)
(require 'f)
(require 's)
(require 'pcase)
(require 'cl-lib)
(eval-when-compile
  (require 'gv))

(defmacro treemacs-import-functions-from (file &rest functions)
  "Import FILE's FUNCTIONS.
Creates a list of `declare-function' statements."
  (declare (indent 1))
  (let ((imports (--map (list 'declare-function it file) functions)))
    `(progn ,@imports)))

(defmacro treemacs-log (msg &rest args)
  "Write a log statement given format string MSG and ARGS."
  `(unless treemacs--no-messages
     (message
      "%s %s"
      (propertize "[Treemacs]" 'face 'font-lock-keyword-face)
      (format ,msg ,@args))))

(defmacro treemacs-with-writable-buffer (&rest body)
  "Temporarily turn off read-ony mode to execute BODY."
  (declare (debug t))
  `(let (buffer-read-only)
     ,@body))

(defmacro treemacs-without-messages (&rest body)
  "Temporarily turn off messages to execute BODY."
  (declare (debug t))
  `(let ((treemacs--no-messages t))
     ,@body))

(defmacro treemacs-safe-button-get (button &rest properties)
  "Safely extract BUTTON's PROPERTIES.

Using `button-get' on a button located in a buffer that is not the current
buffer does not work, so this function will run the property extaction from
inside BUTTON's buffer."
  `(with-current-buffer (marker-buffer ,button)
     ,(if (= 1 (length properties))
           `(treemacs-button-get ,button ,(car properties))
         `(--map (treemacs-button-get ,button it) ,properties))))

(defmacro treemacs-with-button-buffer (btn &rest body)
  "Use BTN's buffer to execute BODY.
Required for button interactions (like `treemacs-button-get') that do not work
when called from another buffer than the one the button resides in and
`treemacs-safe-button-get' is not enough."
  (declare (indent 1)
           (debug (form body)))
  `(with-current-buffer (marker-buffer ,btn)
    ,@body))

(defmacro treemacs-unless-let (var-val &rest forms)
  "Same as `-if-let-', but the negative case is handled in the first form.
Delegates VAR-VAL and the given FORMS to `-if-let-'."
  (declare (debug ((sexp form) body))
           (indent 2))
  (let ((then (cdr forms))
        (else (car forms)))
    `(-if-let ,var-val (progn ,@then) ,else)))

(defmacro treemacs-with-current-button (error-msg &rest body)
  "Execute an action with the current button bound to 'current-btn'.
Log ERROR-MSG if no button is selected, otherwise run BODY."
  (declare (debug (form body)))
  `(-if-let (current-btn (treemacs-current-button))
       (progn ,@body)
     (treemacs-pulse-on-failure ,error-msg)))

(defmacro treemacs-without-following (&rest body)
  "Execute BODY with `treemacs--ready-to-follow' set to nil."
  ;; no warnings since `treemacs--ready-to-follow' is defined in treemacs-follow-mode.el
  ;; and should stay there since this file is for macros only
  (declare (debug t))
  `(let ((o (with-no-warnings treemacs--ready-to-follow)))
     (with-no-warnings (setq treemacs--ready-to-follow nil))
     (unwind-protect
         (progn ,@body)
       (with-no-warnings (setq treemacs--ready-to-follow o)))))

(cl-defmacro treemacs-do-for-button-state
    (&key on-root-node-open
          on-root-node-closed
          on-file-node-open
          on-file-node-closed
          on-dir-node-open
          on-dir-node-closed
          on-tag-node-open
          on-tag-node-closed
          on-tag-node-leaf
          on-nil
          no-error)
  "Building block macro to execute a form based on the current node state.
Will bind to current button to 'btn' for the executon of the action forms.
When NO-ERROR is non-nil no error will be thrown if no match for the button
state is achieved."
  (declare (debug (&rest [sexp form])))
  `(-if-let (btn (treemacs-current-button))
       (pcase (treemacs-button-get btn :state)
         ,@(when on-root-node-open
             `((`root-node-open
                ,on-root-node-open)))
         ,@(when on-root-node-closed
             `((`root-node-closed
                ,on-root-node-closed)))
         ,@(when on-file-node-open
             `((`file-node-open
                ,on-file-node-open)))
         ,@(when on-file-node-closed
             `((`file-node-closed
                ,on-file-node-closed)))
         ,@(when on-dir-node-open
             `((`dir-node-open
                ,on-dir-node-open)))
         ,@(when on-dir-node-closed
             `((`dir-node-closed
                ,on-dir-node-closed)))
         ,@(when on-tag-node-open
             `((`tag-node-open
                ,on-tag-node-open)))
         ,@(when on-tag-node-closed
             `((`tag-node-closed
                ,on-tag-node-closed)))
         ,@(when on-tag-node-leaf
             `((`tag-node
                ,on-tag-node-leaf)))
         ,@(unless no-error
             `((state (error "[Treemacs] Unexpected button state %s" state)))))
     ,on-nil))

(cl-defmacro treemacs--execute-button-action
    (&key save-window
          ensure-window-split
          split-function
          window
          dir-action
          file-action
          tag-section-action
          tag-action
          no-match-explanation)
  "Infrastructure macro for setting up actions on different button states.
Fetches the currently selected button and verifies it's in the correct state
based on the given state actions.
If it isn't it will log NO-MATCH-EXPLANATION, if it is it selects WINDOW (or
`next-window' if none is given) and splits it with SPLIT-FUNCTION if given.
DIR-ACTION, FILE-ACTION, TAG-SECTION-ACTION and TAG-ACTION are inserted into a
`pcase' statement matching the buttons state. Project root nodes are treated the
same common directory nodes.
If ENSURE-WINDOW-SPLIT is t treemacs will vertically split the window if
treemacs is the only window to make sure a buffer is opened next to it, not
under or below it."
  (declare (debug (&rest [sexp form])))
  (let ((valid-states (list)))
    (when dir-action
      (push 'root-node-open valid-states)
      (push 'root-node-closed valid-states)
      (push 'dir-node-open valid-states)
      (push 'dir-node-closed valid-states))
    (when file-action
      (push 'file-node-open valid-states)
      (push 'file-node-closed valid-states))
    (when tag-section-action
      (push 'tag-node-open valid-states)
      (push 'tag-node-closed valid-states))
    (when tag-action
      (push 'tag-node valid-states))
    `(-when-let (btn (treemacs-current-button))
       (treemacs-without-following
        (let* ((state (treemacs-button-get btn :state))
               (current-window (selected-window)))
          (if (not (memq state ',valid-states))
              (treemacs-pulse-on-failure "%s" ,no-match-explanation)
            (progn
              ,@(if ensure-window-split
                    `((when (one-window-p)
                        (save-selected-window
                          (split-window nil nil (if (eq 'left treemacs-position) 'right 'left))))))
              (select-window (or ,window (next-window (selected-window) nil nil)))
              ,@(if split-function
                    `((funcall ,split-function)
                      (other-window 1)))
              ;; Return the result of the action
              (prog1 (pcase state
                       ,@(when dir-action
                           `(((or `dir-node-open `dir-node-closed `root-node-open `root-node-closed)
                              ,dir-action)))
                       ,@(when file-action
                           `(((or `file-node-open `file-node-closed)
                              ,file-action)))
                       ,@(when tag-section-action
                           `(((or `tag-node-open `tag-node-closed)
                              ,tag-section-action)))
                       ,@(when tag-action
                           `((`tag-node
                              ,tag-action)))
                       (_ (error "No match achieved even though button's state %s was part of the set of valid states %s"
                                 state ',valid-states)))
                (when ,save-window
                  (select-window current-window))))))))))

(defmacro treemacs--without-filewatch (&rest body)
  "Run BODY without triggering the filewatch callback.
Required for manual interactions with the file system (like deletion), otherwise
the on-delete code will run twice."
  (declare (debug t))
  `(cl-flet (((symbol-function 'treemacs--filewatch-callback) (symbol-function 'ignore)))
     ,@body))

(defmacro treemacs-save-position (main-form &rest final-form)
  "Execute MAIN-FORM without switching position.
Finally execute FINAL-FORM after the code to restore the position has run.

This macro is meant for cases where a simple `save-excursion' will not do, like
a refresh, which can potentially change the entire buffer layout. This means
attempt first to keep point on the same file/tag, and if that does not work keep
it on the same line."
  (declare (debug (form body)))
  `(treemacs-without-following
    (let* ((curr-line      (line-number-at-pos)) ;; TODO(2018/10/29): line in *window*
           (curr-btn       (treemacs-current-button))
           (curr-node-path (when curr-btn (treemacs-button-get curr-btn :path)))
           (curr-state     (when curr-btn (treemacs-button-get curr-btn :state)))
           (curr-file      (when curr-btn (treemacs--nearest-path curr-btn)))
           (curr-tagpath   (when curr-btn (treemacs--tags-path-of curr-btn)))
           (curr-winstart  (window-start (get-buffer-window))))
      ,main-form
      ;; try to stay at the same file/tag
      ;; if the tag no longer exists move to the tag's owning file node
      ;; if the file no longer exists try to stay in the same visual line
      (pcase curr-state
        ((or 'root-node-open 'root-node-closed 'dir-node-open 'dir-node-closed 'file-node-open 'file-node-closed)
         (if (and (file-exists-p curr-file)
                  (or treemacs-show-hidden-files
                      (not (s-matches? treemacs-dotfiles-regex (treemacs--filename curr-file)))))
             (treemacs-goto-file-node curr-file)
           (treemacs-without-messages (with-no-warnings (goto-line curr-line)))))
        ((or 'tag-node-open 'tag-node-closed 'tag-node)
         ;; no correction needed, if the tag does not exist point is left at the next best node
         (treemacs--goto-tag-button-at curr-tagpath))
        ((pred null)
         (goto-char (point-min)))
        (_
         ;; point is on a custom node
         ;; TODO(2018/10/30): custom node exists predicate?
         (condition-case _
             (treemacs-goto-node curr-node-path)
           (error (ignore)))))
      (treemacs--evade-image)
      (set-window-start (get-buffer-window) curr-winstart)

      ;; this part seems to fix the issue of point being reset to the top
      ;; when the buffer is refreshed without the window being selected
      (-when-let (w (get-buffer-window (buffer-name) t))
        (set-window-point w (point)))
      ,@final-form)))

(defmacro treemacs-run-in-every-buffer (&rest body)
  "Run BODY once locally in every treemacs buffer."
  (declare (debug t))
  `(dolist (frame->buffer treemacs--buffer-access)
     (-let [(--frame-- . --buffer--) frame->buffer]
       (when (buffer-live-p --buffer--)
         (with-selected-frame --frame--
             (with-current-buffer --buffer--
               ,@body))))))

(defmacro treemacs--defstruct (name &rest properties)
  "Define a struct with NAME and PROPERTIES.
Delegates to `cl-defstruct', creating a struct with a 'NAME->' `:conc-name' and
foregoing typechecking for its properties for the hope of improved performance."
  (declare (indent 1))
  (-let [prefix (concat (symbol-name name) "->")]
    `(progn
       (cl-defstruct (,name (:conc-name ,(intern prefix)))
         ,@properties)
       ,@(--map
          (let* ((prop-name (symbol-name (nth it properties)))
                 (func-name (intern (concat prefix prop-name))))
            `(progn
               ;; ignore warnings since the accessors are defined twice
               (with-no-warnings
                 ;; redefine the accessors without the type checking
                 (define-inline ,func-name (obj)
                   ,(format "Get the '%s' property of `%s' OBJ." prop-name name)
                   (declare (side-effect-free t))
                   (inline-quote (aref ,',obj ,(1+ it)))))))
          (number-sequence 0 (1- (length properties)))))))

(defmacro treemacs-only-during-init (&rest body)
  "Run BODY only when treemacs has not yet been loaded.
Specifically only run it when (featurep 'treemacs) returns nil."
  (declare (debug t))
  `(unless (featurep 'treemacs)
     ,@body))

(defmacro treemacs--maphash (table names &rest body)
  "Iterate over entries of TABLE in BODY.
Entry variables will bound based on NAMES which is a list of two elements."
  (declare (debug (sexp sexp body))
           (indent 2))
  (let ((key-name (cl-first names))
        (val-name (cl-second names)))
    `(maphash
      (lambda (,key-name ,val-name) ,@body)
      ,table)))

(defmacro treemacs-return (predicate &optional error-msg &rest error-args)
  "Simplifies the common pattern of returning from a `cl-block' named 'body'.
Supports two calling conventions:
First by providing all arguments. The block will only be left when PREDICATE
returns non-nil, ERROR-MSG and ERROR-ARGS will be passed to
`treemacs-pulse-on-failure'.
Second by providing a single string argument. In this case no test is made and
the block is left directly with the given message being passed to
`treemacs-pulse-on-failure'."
  (declare (indent 1))
  (if (and (stringp predicate)
           (null error-msg))
      `(cl-return-from body
         (treemacs-pulse-on-failure ,predicate))
    `(when ,predicate
       (cl-return-from body
         (treemacs-pulse-on-failure ,error-msg ,@error-args)))))

(cl-defmacro treemacs-first-child-node-where (btn &rest predicate)
  "Among the *direct* children of BTN find the first child matching PREDICATE.
For the PREDICATE call the button being checked is bound as 'child-btn'."
  (declare (indent 1) (debug (sexp body)))
  `(cl-block __search__
     (let* ((child-btn (next-button (button-end ,btn) t))
            (depth (treemacs-button-get child-btn :depth)))
       (when (equal (treemacs-button-get child-btn :parent) ,btn)
         (if ,@predicate
             (cl-return-from __search__ child-btn)
           (while child-btn
             (setq child-btn (next-button (button-end child-btn)))
             (-let [child-depth (treemacs-button-get child-btn :depth)]
               (cond
                ((= depth child-depth)
                 (when ,@predicate (cl-return-from __search__ child-btn)) )
                ((> depth child-depth)
                 (cl-return-from __search__ nil))))))))))

(defmacro treemacs-is-path (left op &optional right)
  "Readable utility macro for various path predicates.
LEFT is a file path, RIGHT is either a path, project, or workspace while OP can
take the following forms:

 * `:same-as' will check for string equality
 * `:in' will check will check whether LEFT is a child or the same as RIGHT.
 * `:parent-of' will check whether LEFT is a parent of, and not equal to, RIGHT
 * `:in-project' will check whether LEFT is part of the project RIGHT
 * `:in-workspace' will check whether LEFT is part of the workspace RIGHT and
   return the appropriate project when it is. If RIGHT is not given it will
   default to calling `treemacs-current-workspace'.

LEFT and RIGHT are expected to be in treemacs canonical file path format (see
also `treemacs--canonical-path').

Even if LEFT or RIGHT should be a form and not a variable it is guaranteed that
they will be evaluated only once."
  (cl-assert (memq op '(:same-as :in :parent-of :in-project :in-workspace))
             :show-args
             "Invalid treemacs-is-path operator: `%s'" op)
  (cl-assert (or (eq op :in-workspace) right)
             :show-args
             "Missing right side argument.")
  (declare (debug (form form form)))
  (macroexp-let2* nil
      ((left left)
       (right right))
    (pcase op
      (:same-as
       `(string= ,left ,right))
      (:in
       `(or (string= ,left ,right)
            (s-starts-with? (f-slash ,right) ,left)))
      (:parent-of
       `(and (s-starts-with? (f-slash ,left) ,right)
             (not (string= ,left ,right))))
      (:in-project
       `(treemacs-is-path ,left :in (treemacs-project->path ,right)))
      (:in-workspace
       (-let [ws (or right '(treemacs-current-workspace))]
         `(--first (treemacs-is-path ,left :in-project it)
                   (treemacs-workspace->projects ,ws)))))))

(provide 'treemacs-macros)

;;; treemacs-macros.el ends here
