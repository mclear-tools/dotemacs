;;; el-patch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "el-patch" "el-patch.el" (0 0 0 0))
;;; Generated autoloads from el-patch.el

(let ((loads (get 'el-patch 'custom-loads))) (if (member '"el-patch" loads) nil (put 'el-patch 'custom-loads (cons '"el-patch" loads))))

(defvar el-patch-deftype-alist nil "\
Alist of types of definitions that can be patched with `el-patch'.
The keys are definition types, like `defun', `define-minor-mode',
etc. The values are plists; the following keywords are accepted:

`:classify' - a function which may be called with a full
definition form (a list starting with e.g. `defun') and which
returns an alist detailing what it defines. In this alist, the
keys are symbols; only the values `function' and `variable' are
allowed. The values are names of functions and variables,
respectively, that are defined by the definition form. This
argument is mandatory.

`:locate' - a function which may be called with a full definition
form (a list starting with e.g. `defun') and which returns the
actual source code of the definition, as a list. If the patch
correct and up to date, then this will actually be the same as
the definition which was passed in. This argument is optional,
but required if you want patch validation to work.

`:declare' - a list to be put in a `declare' form of the
resulting `el-patch' macro, like:

    ((doc-string 2) (indent defun))

This argument is optional.

`:macro-name' - normally the name of the macro generated for
patching a `defun' is called `el-patch-defun', but you can
override that by providing this argument. This argument is
optional.")

(custom-autoload 'el-patch-deftype-alist "el-patch" t)

(defvar el-patch--patches (make-hash-table :test 'equal) "\
Hash table of patches that have been defined.
The keys are symbols naming the objects that have been patched.
The values are hash tables mapping definition types (symbols
`defun', `defmacro', etc.) to patch definitions, which are lists
beginning with `defun', `defmacro', etc.

Note that the symbols are from the versions of patches that have
been resolved in favor of the modified version, when a patch
renames a symbol.")

(autoload 'el-patch-add "el-patch" "\
Patch directive for inserting forms.
In the original definition, the ARGS and their containing form
are removed. In the new definition, the ARGS are spliced into the
containing s-expression.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-add 'lisp-indent-function '0)

(autoload 'el-patch-remove "el-patch" "\
Patch directive for removing forms.
In the original definition, the ARGS are spliced into the
containing s-expression. In the new definition, the ARGS and
their containing form are removed.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-remove 'lisp-indent-function '0)

(autoload 'el-patch-swap "el-patch" "\
Patch directive for swapping forms.
In the original definition, OLD is spliced into the containing
s-expression. In the new definition, NEW is spliced instead.

\(fn OLD NEW)" nil t)

(function-put 'el-patch-swap 'lisp-indent-function '0)

(autoload 'el-patch-wrap "el-patch" "\
Patch directive for wrapping forms.
TRIML and TRIMR are optional arguments. If only one is provided,
it is assumed to be TRIML. ARGS is required, and it must be a
list.

In the original definition, the ARGS are spliced into the
containing s-expression. If TRIML is provided, the first TRIML of
the ARGS are removed first. If TRIMR is provided, the last TRIMR
are also removed. In the new definition, the ARGS and their
containing list are spliced into the containing s-expression.

\(fn &optional TRIML TRIMR ARGS)" nil t)

(function-put 'el-patch-wrap 'lisp-indent-function 'defun)

(autoload 'el-patch-splice "el-patch" "\
Patch directive for splicing forms.
TRIML and TRIMR are optional arguments. If only one is provided,
it is assumed to be TRIML. ARGS is required, and it must be a
list.

In the original definition, the ARGS and their containing list
are spliced into the containing s-expression. In the new
definition, the ARGS are spliced into the containing
s-expression. If TRIML is provided, the first TRIML of the ARGS
are removed first. If TRIMR is provided, the last TRIMR are also
removed.

\(fn &optional TRIML TRIMR ARGS)" nil t)

(function-put 'el-patch-splice 'lisp-indent-function 'defun)

(autoload 'el-patch-let "el-patch" "\
Patch directive for creating local el-patch bindings.
Creates local bindings according to VARLIST, then splices ARGS
into both the original and new definitions. You may bind symbols
that are also patch directives, but the bindings will not have
effect if the symbols are used at the beginning of a list (they
will act as patch directives).

\(fn VARLIST &rest ARGS)" nil t)

(function-put 'el-patch-let 'lisp-indent-function '1)

(autoload 'el-patch-literal "el-patch" "\
Patch directive for treating patch directives literally.
ARGS are spliced into the containing s-expression, but are not
processed further by el-patch.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-literal 'lisp-indent-function '0)

(autoload 'el-patch-concat "el-patch" "\
Patch directive for modifying string literals.
ARGS should resolve to strings; those strings are passed to
`concat' and spliced into the containing s-expression in both the
original and new definitions.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-concat 'lisp-indent-function '0)

(autoload 'el-patch--definition "el-patch" "\
Activate a PATCH-DEFINITION and update `el-patch--patches'.
PATCH-DEFINITION is an unquoted list starting with `defun',
`defmacro', etc., which may contain patch directives.

\(fn PATCH-DEFINITION)" nil t)

(autoload 'el-patch-unpatch "el-patch" "\
Remove the patch given by the PATCH-DEFINITION.
This restores the original functionality of the object being
patched. NAME and TYPE are as returned by `el-patch-get'.

\(fn NAME TYPE)" t nil)

(cl-defmacro el-patch-deftype (type &rest kwargs &key classify locate declare macro-name) "\
Allow `el-patch' to patch definitions of the given TYPE.
TYPE is a symbol like `defun', `define-minor-mode', etc. This
updates `el-patch-deftype-alist' (which see) with the provided
keyword arguments and defines a macro named like
`el-patch-defun', `el-patch-define-minor-mode', etc." (declare (indent defun)) (ignore locate) (unless classify (error "You must specify `:classify' in calls to `el-patch-deftype'")) (\` (progn (setf (alist-get (quote (\, type)) el-patch-deftype-alist) (copy-tree (quote (\, kwargs)))) (defmacro (\, (or macro-name (intern (format "el-patch-%S" type)))) (name &rest args) (\, (format "Use `el-patch' to override a `%S' form.
The ARGS are the same as for `%S'." type type)) (\,@ (when declare (\` ((declare (\,@ declare)))))) (list (function el-patch--definition) (cl-list* (quote (\, type)) name args))))))

(el-patch-deftype cl-defun :classify el-patch-classify-function :locate el-patch-locate-function :declare ((doc-string 3) (indent defun)))

(el-patch-deftype defconst :classify el-patch-classify-variable :locate el-patch-locate-variable :declare ((doc-string 3) (indent defun)))

(el-patch-deftype defcustom :classify el-patch-classify-variable :locate el-patch-locate-variable :declare ((doc-string 3) (indent defun)))

(el-patch-deftype define-minor-mode :classify el-patch-classify-define-minor-mode :locate el-patch-locate-function :declare ((doc-string 2) (indent defun)))

(el-patch-deftype defmacro :classify el-patch-classify-function :locate el-patch-locate-function :declare ((doc-string 3) (indent defun)))

(el-patch-deftype defsubst :classify el-patch-classify-function :locate el-patch-locate-function :declare ((doc-string 3) (indent defun)))

(el-patch-deftype defun :classify el-patch-classify-function :locate el-patch-locate-function :declare ((doc-string 3) (indent defun)))

(el-patch-deftype defvar :classify el-patch-classify-variable :locate el-patch-locate-variable :declare ((doc-string 3) (indent defun)))

(autoload 'el-patch-validate "el-patch" "\
Validate the patch with given NAME and TYPE.
This means el-patch will attempt to find the original definition
for the function, and verify that it is the same as the original
function assumed by the patch. A warning will be signaled if the
original definition for a patched function cannot be found, or if
there is a difference between the actual and expected original
definitions.

Interactively, use `completing-read' to select a function to
inspect the patch of.

NAME is a symbol naming the object being patched; TYPE is a
symbol `defun', `defmacro', etc.

Returns nil if the patch is not valid, and otherwise returns t.
If NOMSG is non-nil, does not signal a message when the patch is
valid.

If RUN-HOOKS is non-nil, runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'. Interactively, this happens unless
a prefix argument is provided.

See also `el-patch-validate-all'.

\(fn NAME TYPE &optional NOMSG RUN-HOOKS)" t nil)

(autoload 'el-patch-validate-all "el-patch" "\
Validate all currently defined patches.
Runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'.

See `el-patch-validate'.

\(fn)" t nil)

(autoload 'el-patch-feature "el-patch" "\
Declare that some patches are only defined after FEATURE is loaded.
This is a convenience macro that creates a function for invoking
`require' on that feature, and then adds it to
`el-patch-pre-validate-hook' so that your patches are loaded and
`el-patch' can properly validate them.

FEATURE should be an unquoted symbol. ARGS, if given, are passed
as quoted literals along with FEATURE to
`el-patch-require-function' when `el-patch-validate-all' is
called.

\(fn FEATURE &rest ARGS)" nil t)

(autoload 'el-patch-get "el-patch" "\
Return the patch for object NAME of the given TYPE.
NAME is a symbol for the name of the definition that was patched,
and TYPE is a symbol `defun', `defmacro', etc. If the patch could
not be found, return nil.

\(fn NAME TYPE)" nil nil)

(autoload 'el-patch-ediff-patch "el-patch" "\
Show the patch for an object in Ediff.
NAME and TYPE are as returned by `el-patch-get'.

\(fn NAME TYPE)" t nil)

(autoload 'el-patch-ediff-conflict "el-patch" "\
Show a patch conflict in Ediff.
This is a diff between the expected and actual values of a
patch's original definition. NAME and TYPE are as returned by
`el-patch-get'.

\(fn NAME TYPE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "el-patch" '("el-patch-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; el-patch-autoloads.el ends here
