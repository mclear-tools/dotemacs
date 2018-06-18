;;; treemacs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "treemacs" "treemacs.el" (0 0 0 0))
;;; Generated autoloads from treemacs.el

(autoload 'treemacs-version "treemacs" "\
Return `treemacs-version'.

\(fn)" t nil)

(autoload 'treemacs "treemacs" "\
Initialize or toggle treemacs.
* If the treemacs window is visible hide it.
* If a treemacs buffer exists, but is not visible show it.
* If no treemacs buffer exists for the current frame create and show it.
* If the workspace is empty additionally ask for the root path of the first
  project to add.

\(fn)" t nil)

(autoload 'treemacs-bookmark "treemacs" "\
Find a bookmark in treemacs.
Only bookmarks marking either a file or a directory are offered for selection.
Treemacs will try to find and focus the given bookmark's location, in a similar
fashion to `treemacs-find-file'.

With a prefix argument ARG treemacs will also open the bookmarked location.

\(fn &optional ARG)" t nil)

(autoload 'treemacs-find-file "treemacs" "\
Find and focus the current file in the treemacs window.
If the current buffer has visits no file or with a prefix ARG ask for the
file instead.
Will show/create a treemacs buffers if it is not visible/does not exist.
For the most part only useful when `treemacs-follow-mode' is not active.

\(fn &optional ARG)" t nil)

(autoload 'treemacs-find-tag "treemacs" "\
Find and move point to the tag at point in the treemacs view.
Most likley to be useful when `treemacs-tag-follow-mode' is not active.

Will ask to change the treemacs root if the file to find is not under the
root. If no treemacs buffer exists it will be created with the current file's
containing directory as root. Will do nothing if the current buffer is not
visiting a file or Emacs cannot find any tags for the current file.

\(fn)" t nil)

(autoload 'treemacs-select-window "treemacs" "\
Select the treemacs window if it is visible.
Bring it to the foreground if it is not visible.
Initialize a new treemacs buffer as calling `treemacs' would if there is no
treemacs buffer for this frame.

\(fn)" t nil)

(autoload 'treemacs-show-changelog "treemacs" "\
Show the changelog of treemacs.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "treemacs-async" "treemacs-async.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from treemacs-async.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-async" '("treemacs-")))

;;;***

;;;### (autoloads nil "treemacs-branch-creation" "treemacs-branch-creation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-branch-creation.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-branch-creation" '("treemacs--")))

;;;***

;;;### (autoloads nil "treemacs-compatibility" "treemacs-compatibility.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-compatibility.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-compatibility" '("treemacs--split-window-advice")))

;;;***

;;;### (autoloads nil "treemacs-customization" "treemacs-customization.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-customization.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-customization" '("treemacs-")))

;;;***

;;;### (autoloads nil "treemacs-filewatch-mode" "treemacs-filewatch-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-filewatch-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-filewatch-mode" '("treemacs-")))

;;;***

;;;### (autoloads nil "treemacs-follow-mode" "treemacs-follow-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-follow-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-follow-mode" '("treemacs-")))

;;;***

;;;### (autoloads nil "treemacs-impl" "treemacs-impl.el" (0 0 0 0))
;;; Generated autoloads from treemacs-impl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-impl" '("treemacs-")))

;;;***

;;;### (autoloads nil "treemacs-interface" "treemacs-interface.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-interface.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-interface" '("treemacs-")))

;;;***

;;;### (autoloads nil "treemacs-macros" "treemacs-macros.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from treemacs-macros.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-macros" '("only-during-treemacs-init" "-if-let-" "-unless-let" "-when-let-" "-let" "-pcase" "-defstruct" "treemacs-")))

;;;***

;;;### (autoloads nil "treemacs-mode" "treemacs-mode.el" (0 0 0 0))
;;; Generated autoloads from treemacs-mode.el

(autoload 'treemacs-mode "treemacs-mode" "\
A major mode for displaying the file system in a tree layout.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-mode" '("treemacs-")))

;;;***

;;;### (autoloads nil "treemacs-mouse-interface" "treemacs-mouse-interface.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-mouse-interface.el

(autoload 'treemacs-node-buffer-and-position "treemacs-mouse-interface" "\
Return source buffer or list of buffer and position for the current node.
This information can be used for future display. Stay in the selected window and
ignore any prefix argument.

\(fn &optional _)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-mouse-interface" '("treemacs-")))

;;;***

;;;### (autoloads nil "treemacs-persistence" "treemacs-persistence.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-persistence.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-persistence" '("treemacs--")))

;;;***

;;;### (autoloads nil "treemacs-structure" "treemacs-structure.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-structure.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-structure" '("treemacs-")))

;;;***

;;;### (autoloads nil "treemacs-tag-follow-mode" "treemacs-tag-follow-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-tag-follow-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-tag-follow-mode" '("treemacs-")))

;;;***

;;;### (autoloads nil "treemacs-tags" "treemacs-tags.el" (0 0 0 0))
;;; Generated autoloads from treemacs-tags.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-tags" '("treemacs--")))

;;;***

;;;### (autoloads nil "treemacs-visuals" "treemacs-visuals.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from treemacs-visuals.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-visuals" '("treemacs-")))

;;;***

;;;### (autoloads nil "treemacs-workspaces" "treemacs-workspaces.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from treemacs-workspaces.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "treemacs-workspaces" '("treemacs-")))

;;;***

;;;### (autoloads nil nil ("treemacs-faces.el" "treemacs-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; treemacs-autoloads.el ends here
