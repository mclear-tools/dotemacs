;;; org-roam-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-roam" "org-roam.el" (0 0 0 0))
;;; Generated autoloads from org-roam.el

(defalias 'org-roam 'org-roam-buffer-toggle-display)

(defvar org-roam-mode nil "\
Non-nil if Org-Roam mode is enabled.
See the `org-roam-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-mode'.")

(custom-autoload 'org-roam-mode "org-roam" nil)

(autoload 'org-roam-mode "org-roam" "\
Minor mode for Org-roam.

This mode sets up several hooks, to ensure that the cache is updated on file
changes, renames and deletes. It is also in charge of graceful termination of
the database connection.

When called interactively, toggle `org-roam-mode'. with prefix
ARG, enable `org-roam-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `org-roam-mode' if ARG is omitted,
nil, or positive. If ARG is `toggle', toggle `org-roam-mode'.
Otherwise, behave as if called interactively.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam" '("org-roam-")))

;;;***

;;;### (autoloads nil "org-roam-buffer" "org-roam-buffer.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-roam-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-buffer" '("org-roam-buffer")))

;;;***

;;;### (autoloads nil "org-roam-capture" "org-roam-capture.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-roam-capture.el

(autoload 'org-roam-capture "org-roam-capture" "\
Launches an `org-capture' process for a new or existing note.
This uses the templates defined at `org-roam-capture-templates'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-capture" '("org-roam-")))

;;;***

;;;### (autoloads nil "org-roam-compat" "org-roam-compat.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-roam-compat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-compat" '("org-roam-graph-node-shape")))

;;;***

;;;### (autoloads nil "org-roam-completion" "org-roam-completion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-roam-completion.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-completion" '("org-roam-completion-")))

;;;***

;;;### (autoloads nil "org-roam-dailies" "org-roam-dailies.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-roam-dailies.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-dailies" '("org-roam-dailies-")))

;;;***

;;;### (autoloads nil "org-roam-db" "org-roam-db.el" (0 0 0 0))
;;; Generated autoloads from org-roam-db.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-db" '("org-roam-db")))

;;;***

;;;### (autoloads nil "org-roam-graph" "org-roam-graph.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-roam-graph.el

(autoload 'org-roam-graph "org-roam-graph" "\
Build and possibly display a graph for FILE from NODE-QUERY.
If FILE is nil, default to current buffer's file name.
ARG may be any of the following values:
  - nil       show the graph.
  - `\\[universal-argument]'     show the graph for FILE.
  - `\\[universal-argument]' N   show the graph for FILE limiting nodes to N steps.
  - `\\[universal-argument] \\[universal-argument]' build the graph.
  - `\\[universal-argument]' -   build the graph for FILE.
  - `\\[universal-argument]' -N  build the graph for FILE limiting nodes to N steps.

\(fn &optional ARG FILE NODE-QUERY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-graph" '("org-roam-graph-")))

;;;***

;;;### (autoloads nil "org-roam-macs" "org-roam-macs.el" (0 0 0 0))
;;; Generated autoloads from org-roam-macs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-macs" '("org-roam--with-temp-buffer")))

;;;***

;;;### (autoloads nil "org-roam-protocol" "org-roam-protocol.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-roam-protocol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-roam-protocol" '("org-roam-protocol-open-")))

;;;***

;;;### (autoloads nil nil ("org-roam-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-roam-autoloads.el ends here
