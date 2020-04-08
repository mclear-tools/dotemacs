;;; dired+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired+" "dired+.el" (0 0 0 0))
;;; Generated autoloads from dired+.el

(defvar diff-switches "-c" "\
*A string or list of strings specifying switches to be passed to diff.")

(custom-autoload 'diff-switches "dired+" t)

(defvar diredp-auto-focus-frame-for-thumbnail-tooltip-flag nil "\
*Non-nil means automatically focus the frame for a thumbnail tooltip.
If nil then you will not see a thumbnail image tooltip when you
mouseover an image-file name in Dired, unless you first give the frame
the input focus (e.g., by clicking its title bar).

This option has no effect if `diredp-image-preview-in-tooltip' is nil.
It also has no effect for Emacs versions prior to Emacs 22.")

(custom-autoload 'diredp-auto-focus-frame-for-thumbnail-tooltip-flag "dired+" t)

(defvar diredp-bind-problematic-terminal-keys t "\
*Non-nil means bind some keys that might not work in a text-only terminal.
This applies to keys that use modifiers Meta and Shift together.
If you use Emacs in text-only terminal and your terminal does not
support the use of such keys then customize this option to nil.")

(custom-autoload 'diredp-bind-problematic-terminal-keys "dired+" t)

(defvar diredp-compressed-extensions '(".tar" ".taz" ".tgz" ".arj" ".lzh" ".lzma" ".xz" ".zip" ".z" ".Z" ".gz" ".bz2" ".rar" ".rev") "\
*List of compressed-file extensions, for highlighting.

Note: If you change the value of this option then you need to restart
Emacs to see the effect of the new value on font-locking.")

(custom-autoload 'diredp-compressed-extensions "dired+" t)

(defvar diredp-do-report-echo-limit 5 "\
Echo result for each file, for fewer than this many files.
If more than this many files are acted on then there is no echoing.

Used by some do-and-report commands such as `diredp-do-emacs-command'.
Results that are not echoed are anyway reported by `dired-log', so you
can show them with `?' in the Dired buffer.")

(custom-autoload 'diredp-do-report-echo-limit "dired+" t)

(defvar diredp-dwim-any-frame-flag pop-up-frames "\
*Non-nil means the target directory can be in a window in another frame.
Only visible frames are considered.
This is used by ``dired-dwim-target-directory'.
This option has no effect for Emacs versions before Emacs 22.")

(custom-autoload 'diredp-dwim-any-frame-flag "dired+" t)

(unless (fboundp 'define-minor-mode) (defcustom diredp-highlight-autofiles-mode t "*Non-nil means highlight names of files that are autofile bookmarks.\nAutofiles that have tags are highlighted using face\n`diredp-tagged-autofile-name'.  Those with no tags are highlighted\nusing face `diredp-autofile-name'.\n\nSetting this option directly does not take effect; use either\n\\[customize] or command `diredp-highlight-autofiles-mode'.\n\nNOTE: When `dired+.el' is loaded (for the first time per Emacs\nsession), the highlighting is turned ON, regardless of the option\nvalue.  To prevent this and have the highlighting OFF by default, you\nmust do one of the following:\n\n * Put (diredp-highlight-autofiles-mode -1) in your init file, AFTER\n   it loads `dired+.el'.\n\n * Customize the option to `nil', AND ensure that your `custom-file'\n   (or the `custom-saved-variables' part of your init file) is\n   evaluated before `dired+.el' is loaded.\n\nThis option has no effect unless you use libraries `Bookmark and\n`highlight.el'." :set (lambda (symbol value) (diredp-highlight-autofiles-mode (if value 1 -1))) :initialize 'custom-initialize-default :type 'boolean :group 'Dired-Plus :require 'dired+))

(defvar diredp-ignore-compressed-flag t "\
*Non-nil means to font-lock names of compressed files as ignored files.
This applies to filenames whose extensions are in
`diredp-compressed-extensions'.  If nil they are highlighted using
face `diredp-compressed-file-name'.

Note: If you change the value of this option then you need to restart
Emacs to see the effect of the new value on font-locking.")

(custom-autoload 'diredp-ignore-compressed-flag "dired+" t)

(defvar diredp-image-preview-in-tooltip (or (and (boundp 'image-dired-thumb-size) image-dired-thumb-size) 100) "\
*Whether and what kind of image preview to show in a tooltip.
The possible values are:

 `nil'       : do not show a tooltip preview
 integer N>0 : show a thumbnail preview of that size
 `full'      : show a full-size preview of the image

To enable tooltip image preview you must turn on `tooltip-mode' and
load library `image-dired.el'.  See also option
`diredp-auto-focus-frame-for-thumbnail-tooltip-flag'.

This option has no effect for Emacs versions prior to Emacs 22.")

(custom-autoload 'diredp-image-preview-in-tooltip "dired+" t)

(defvar diredp-image-show-this-file-use-frame-flag t "\
Non-nil means `diredp-image-show-this-file' uses another frame.
If nil then it uses another window.  Using another frame means you
have more control over the image size when you use a prefix arg.

If it uses another window then the prefix arg controls only the
minimum window height, not necessarily the image scale (height).

\(If the buffer displaying the image is already considered a
special-display buffer by your Emacs setup, then a nil value of this
option has no effect.)")

(custom-autoload 'diredp-image-show-this-file-use-frame-flag "dired+" t)

(defvar diredp-list-file-attributes (list 5 8) "\
Which file attributes `diredp-list-file' uses, and when.
A list of file attribute numbers means use only the values of those
attributes.
A non-list means use all attribute values.")

(custom-autoload 'diredp-list-file-attributes "dired+" t)

(defvar diredp-max-frames 200 "\
*Max number of frames, for commands that find files in separate frames.
These commands are `dired-do-find-marked-files' and
`diredp-do-find-marked-files-recursive'.  See their descriptions for
the circumstances in which they show the files in separate frames.")

(custom-autoload 'diredp-max-frames "dired+" t)

(defvar diredp-prompt-for-bookmark-prefix-flag nil "\
*Non-nil means prompt for a prefix string for bookmark names.")

(custom-autoload 'diredp-prompt-for-bookmark-prefix-flag "dired+" t)

(defvar diredp-visit-ignore-regexps nil "\
Regexps matching file names for `diredp-visit-(next|previous)' to skip.
A file or directory name matching one of these regexps is skipped,
along with those with an extension in `diredp-visit-ignore-extensions'.")

(custom-autoload 'diredp-visit-ignore-regexps "dired+" t)

(defvar diredp-visit-ignore-extensions '("elc") "\
Extensions of file names for `diredp-visit-(next|previous)' to skip.
A file name with one of these extensions is skipped, along with those
matching a regexp in `diredp-visit-ignore-regexps'.")

(custom-autoload 'diredp-visit-ignore-extensions "dired+" t)

(defvar diredp-w32-local-drives '(("C:" "Local disk")) "\
*Local MS Windows drives that you want to use for `diredp-w32-drives'.
Each entry is a list (DRIVE DESCRIPTION), where DRIVE is the drive
name and DESCRIPTION describes DRIVE.")

(custom-autoload 'diredp-w32-local-drives "dired+" t)

(defvar diredp-wrap-around-flag t "\
*Non-nil means Dired \"next\" commands wrap around to buffer beginning.")

(custom-autoload 'diredp-wrap-around-flag "dired+" t)

(autoload 'diredp-image-dired-create-thumb "dired+" "\
Create thumbnail image file for FILE (default: file on current line).
With a prefix arg, replace any existing thumbnail for FILE.
With a numeric prefix arg (not a cons), use it as the thumbnail size.
Return the name of the thumbnail image file, or nil if none.

\(fn FILE &optional ARG)" t nil)

(autoload 'image-dired-dired-insert-marked-thumbs "dired+" "\
Insert thumbnails before file names of marked files in the Dired buffer.

\(fn)" t nil)

(autoload 'image-dired-dired-toggle-marked-thumbs "dired+" "\
Toggle thumbnails in front of file names in Dired.
If no files are marked, insert or hide thumbnails on the current line.
With a numeric prefix arg N, ignore marked files and act on the next N
files (previous -N files, if N < 0).

\(fn &optional ARG)" t nil)

(autoload 'diredp-image-dired-comment-file "dired+" "\
Add comment to this image file.

\(fn)" t nil)

(autoload 'diredp-image-dired-tag-file "dired+" "\
Tag this image file with an `image-dired' tag.

\(fn)" t nil)

(autoload 'diredp-image-dired-delete-tag "dired+" "\
Remove an `image-dired' tag from  this image file.

\(fn)" t nil)

(autoload 'diredp-image-dired-display-thumb "dired+" "\
Pop to thumbnail of this image file, in `image-dired-thumbnail-buffer'.
If a thumbnail image does not yet exist for this file, create it.
With a prefix arg, append the thumbnail to the thumbnails buffer,
instead of clearing the buffer first.

\(fn &optional APPEND)" t nil)

(autoload 'diredp-image-dired-copy-with-exif-name "dired+" "\
Copy this image file to your main image directory.
Uses `image-dired-get-exif-file-name' to name the new file.

\(fn)" t nil)

(autoload 'diredp-image-dired-edit-comment-and-tags "dired+" "\
Edit comment and tags for this image file.

\(fn)" t nil)

(autoload 'diredp-do-display-images "dired+" "\
Display the marked image files.
A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any files are marked).
 More than one `C-u' means use all files in the Dired buffer, as if
 they were all marked.

\(fn &optional ARG)" t nil)

(autoload 'diredp-image-show-this-file "dired+" "\
Show the image file named on this line in another frame or window.
Option `diredp-image-show-this-file-use-frame-flag' which is used.

With a prefix arg, shrink the image to fit a frame that many lines
high or a window at least that many lines high.
Otherwise, show the image full size.
Note:
 * To show the image full size, you can also use `\\<dired-mode-map>\\[dired-find-file]'.
 * To show the image in another window, at whatever scale fits there,
   you can use `\\[image-dired-dired-display-image]'.

\(fn &optional ARG)" t nil)

(autoload 'diredp-do-emacs-command "dired+" "\
Invoke an Emacs COMMAND in each marked file.
Visit each marked file at its beginning, then invoke COMMAND.
You are prompted for the COMMAND.

The result returned for each file is logged by `dired-log'.  Use `?'
to see all such results and any error messages.  If there are fewer
marked files than `diredp-do-report-echo-limit' then each result is
also echoed momentarily.

A prefix argument behaves according to the ARG argument of
`dired-get-marked-files'.  In particular, `C-u C-u' operates on all
files in the Dired buffer.

\(fn COMMAND &optional ARG)" t nil)

(autoload 'diredp-dired-for-files "dired+" "\
Dired the file names that you enter, in a Dired buffer that you name.
You are prompted for the name of the Dired buffer to use.
You are then prompted for names of files and directories to list,
 which can be located anywhere.
Use `C-g' when you are done.

With a prefix arg you are first prompted for the `ls' switches to use.

See also `dired' (including the advice).

\(fn ARG &optional SWITCHES)" t nil)

(autoload 'diredp-dired-for-files-other-window "dired+" "\
Same as `diredp-dired-for-files', except uses another window.

\(fn ARG &optional SWITCHES)" t nil)

(autoload 'diredp-dired-recent-files "dired+" "\
Open Dired in BUFFER, showing recently visited files and directories.
You are prompted for BUFFER (default: `Recently Visited Files').

With a numeric prefix arg you can enter names of recent files to
include or exclude.

No prefix arg or a plain prefix arg (`C-u', `C-u C-u', etc.) means
list all of the recently used files.

With a prefix arg:
* If 0, `-', or plain (`C-u') then you are prompted for the `ls'
  switches to use.
* If not plain (`C-u') then:
  * If >= 0 then the files to include are read, one by one.
  * If  < 0 then the files to exclude are read, one by one.

When entering files to include or exclude, use `C-g' to end.

The file listing is always in the order of `recentf-list', which is
reverse chronological order of opening or writing files you access.

\(fn BUFFER &optional ARG)" t nil)

(autoload 'diredp-dired-recent-files-other-window "dired+" "\
Same as `diredp-dired-recent-files', but use other window.

\(fn BUFFER &optional ARG)" t nil)

(autoload 'diredp-dired-recent-dirs "dired+" "\
Open Dired in BUFFER, showing recently visited directories.
Like `diredp-dired-recent-files', but limited to recent directories.
A directory is recent if any of its files is recent.

\(fn BUFFER &optional ARG)" t nil)

(autoload 'diredp-dired-recent-dirs-other-window "dired+" "\
Same as `diredp-dired-recent-dirs', but use other window.

\(fn BUFFER &optional ARG)" t nil)

(autoload 'diredp-do-add-to-recentf "dired+" "\
Add all marked (or next ARG) files to list of recently used files.
That is, add them from variable `recentf-list'.

\(This does not refresh any Dired buffer listing the recently visited
files.  You can refresh it manually using `\\[revert-buffer]'.

\(fn &optional ARG)" t nil)

(autoload 'diredp-do-remove-from-recentf "dired+" "\
Remove all marked (or next ARG) files from list of recently used files.
That is, remove them from variable `recentf-list'.

\(This does not refresh any Dired buffer listing the recently visited
files.  You can refresh it manually using `\\[revert-buffer]'.

\(fn &optional ARG)" t nil)

(autoload 'diredp-add-this-to-recentf "dired+" "\
In Dired, add this file to the list of recently used files.

\(fn)" t nil)

(autoload 'diredp-remove-this-from-recentf "dired+" "\
In Dired, remove this file from the list of recently used files.

\(fn)" t nil)

(autoload 'diredp-add-file-to-recentf "dired+" "\
Add FILE to the front of `recentf-list'.
If FILE is already present, move it to the front of the list.
In Dired, FILE defaults to the file of the current Dired line.

\(fn &optional FILE INTERACTIVEP)" t nil)

(autoload 'diredp-remove-file-from-recentf "dired+" "\
Remove FILE from `recentf-list'.
In Dired, FILE defaults to the file of the current Dired line.  After
removing, revert any displayed buffers showing `recentf-list'.

\(fn &optional FILE INTERACTIVEP)" t nil)

(autoload 'diredp-dired-union "dired+" "\
Create a Dired buffer that is the union of some existing Dired buffers.
With a non-negative prefix arg, you are prompted for `ls' switches.
With a non-positive prefix arg, you are prompted for file and dir
names to add to the listing - see below.

You are prompted for the name of the Dired union buffer.  Completion
against names of existing Dired buffers is available, but you can
enter any other name to create a new Dired buffer of that name.

If the union buffer name you choose names an existing Dired buffer,
then what happens depends on whether that buffer is an ordinary Dired
directory listing or a list of arbitrary file names.  That is, it
depends on whether `dired-directory' is a directory name or a cons of
a Dired buffer name plus file names.

* If the buffer is an ordinary Dired listing, then it is converted to
  an explicit list of absolute file names, just as if these had been
  chosen individually.  The existing buffer and window are replaced by
  new ones that show the explicit listing.  (This replacement is
  necessary because the list of files contained in an ordinary Dired
  listing cannot be modified.)

* If the buffer lists arbitrary file names explicitly, then it is
  updated to include also the files from any Dired buffers and any
  additional files that you specify.

If the union buffer name you choose does not name an existing Dired
buffer, then its `default-directory' is the same as the
`default-directory' before invoking the command.

If you use a non-positive prefix arg, then you can next choose
additional file and directory names to add to the listing.  Use `C-g'
when done choosing them.

Any directory names you choose this way are included as single entries
in the listing - the directory contents are not included (these
directories are not unioned).  To instead include the contents of a
directory chosen this way, use a glob pattern: `/*' after the
directory name.

You are then prompted for the Dired buffers to union.  Use `C-g' when
done choosing them.  These Dired listings to union are included in the
order that you chose them, and each entry is listed only once in the
new Dired buffer.

The new Dired listing respects the markings, subdirectory insertions,
and hidden subdirectories of the selected Dired listings.  However, in
case of conflict between marked or unmarked status for the same entry,
the entry is marked.  Similarly, in case of conflict over an included
subdirectory between it being hidden or shown, it is hidden, but its
contained files are also listed.

See also command `diredp-add-to-dired-buffer'.

From Lisp:
 DIRED-NAME is the name of the resulting Dired union buffer.
 DIRBUFS is a list of the names of Dired buffers to union.
 SWITCHES is a string of `ls' switches.
 EXTRA is a list of files & directories to be included in the listing.

\(fn DIRED-NAME DIRBUFS &optional SWITCHES EXTRA)" t nil)

(autoload 'diredp-dired-union-other-window "dired+" "\
Same as `diredp-dired-union', except use other window.

\(fn DIRED-NAME DIRBUFS &optional SWITCHES EXTRA)" t nil)

(autoload 'diredp-add-to-dired-buffer "dired+" "\
Add individual file and directory names to a Dired buffer.
You are prompted for the buffer name.
With a prefix arg, you are also prompted for the `ls' switches.

The buffer must either not exist yet or must list arbitrary file and
directory names.  That is, it cannot be an ordinary Dired directory
listing - those cannot be modified.

Any directory names you choose this way are included as single entries
in the listing - the directory contents are not included (these
directories are not unioned).  To instead include the contents of a
directory chosen this way, use a glob pattern: `/*' after the
directory name.

See also command `diredp-dired-union'.

From Lisp:
 DIRED-NAME is the name of the Dired buffer to modify.
 TO-ADD is the list of files and dirs to add to it.
 SWITCHES is the string of `ls' switches.

\(fn DIRED-NAME TO-ADD &optional SWITCHES)" t nil)

(autoload 'diredp-add-to-dired-buffer-other-window "dired+" "\
Same as `diredp-add-to-dired-buffer', except use other window.

\(fn DIRED-NAME TO-ADD &optional SWITCHES)" t nil)

(autoload 'diredp-add-to-this-dired-buffer "dired+" "\
Same as `diredp-add-to-dired-buffer' for this Dired buffer.

\(fn DIRED-NAME TO-ADD &optional SWITCHES)" t nil)

(autoload 'diredp-fileset "dired+" "\
Open Dired on the files in fileset FLSET-NAME.

\(fn FLSET-NAME)" t nil)

(autoload 'diredp-fileset-other-window "dired+" "\
Open Dired in another window on the files in fileset FLSET-NAME.

\(fn FLSET-NAME)" t nil)

(autoload 'diredp-dired-this-subdir "dired+" "\
Open Dired for the subdir at or above point.
If point is not on a subdir line, but is in an inserted subdir
listing, then use that subdir.

With a prefix arg:
 If the subdir is inserted and point is in the inserted listing then
 remove that listing and move to the ordinary subdir line.  In other
 words, when in an inserted listing, a prefix arg tears off the
 inserted subdir to its own Dired buffer.

\(fn &optional TEAR-OFF-P MSGP)" t nil)

(autoload 'diredp-dired-inserted-subdirs "dired+" "\
Open Dired for each of the subdirs inserted in this Dired buffer.
A separate Dired buffer is used for each of them.
With a prefix arg, create the Dired buffers but do not display them.
Markings and current Dired switches are preserved.

\(fn &optional NO-SHOW-P MSGP)" t nil)

(autoload 'diredp-find-line-file-other-window "dired+" "\
Visit file named by current line, in another window.
The full text of the line is used as the file name.

\(fn)" t nil)

(autoload 'diredp-mouse-find-line-file-other-window "dired+" "\
Visit file named by clicked line, in another window.
The full text of the line is used as the file name.

\(fn E)" t nil)

(autoload 'diredp-list-marked "dired+" "\
List the marked files in this Dired buffer.
A prefix arg specifies files to use instead of the marked files:

 * Numeric prefix arg N: The next N files (previous -N, if < 0).
 * C-u C-u: All files, but no directories.
 * C-u C-u C-u: All files and directories, except `.' and `..'
 * C-u C-u C-u C-u: All files and directories, including `.' and `..'
 * Any other prefix arg: The current line's file only.

You can use `RET' or `mouse-2' to visit any of the files.
If `tooltip-mode' is on then moving the mouse over image-file names
shows image previews.

When called from Lisp:
 Non-nil optional arg PREDICATE is a file-name predicate.  List only
  the files for which it returns non-nil.
 Non-nil optional arg DETAILS is passed to `diredp-list-files'.

\(fn &optional ARG PREDICATE INTERACTIVEP DETAILS)" t nil)

(defalias 'diredp-paste-files 'diredp-yank-files)

(autoload 'diredp-yank-files "dired+" "\
Paste files, whose absolute names you copied, to the current directory.
With a non-negative prefix arg you are instead prompted for the target
 directory.
With a non-positive prefix arg you can see details about the files if
 you hit `l' when prompted to confirm pasting.  Otherwise you see only
 the file names.  The details you see are defined by option
 `diredp-list-file-attributes'.

You should have copied the list of file names as a string to the kill
ring using \\<dired-mode-map>`M-0 \\[dired-copy-filename-as-kill]' or \\[diredp-copy-abs-filenames-as-kill].
Those commands also set variable `diredp-last-copied-filenames' to the
same string.  `diredp-yank-files' uses the value of that variable, not
whatever is currently at the head of the kill ring.

When called from Lisp:

Optional arg NO-CONFIRM-P means do not ask for confirmation to copy.
Optional arg DETAILS is passed to `diredp-y-or-n-files-p'.

\(fn &optional DIR NO-CONFIRM-P DETAILS)" t nil)

(autoload 'diredp-move-files-named-in-kill-ring "dired+" "\
Move files, whose absolute names you copied, to the current directory.
With a non-negative prefix arg you are instead prompted for the target
 directory.
With a non-positive prefix arg you can see details about the files if
 you hit `l' when prompted to confirm pasting.  Otherwise you see only
 the file names.  The details you see are defined by option
 `diredp-list-file-attributes'.

You should have copied the list of file names as a string to the kill
ring using \\<dired-mode-map>`M-0 \\[dired-copy-filename-as-kill]' or \\[diredp-copy-abs-filenames-as-kill].
Those commands also set variable `diredp-last-copied-filenames' to the
same string.  `diredp-move-files-named-in-kill-ring' uses the value of
that variable, not whatever is currently at the head of the kill ring.

When called from Lisp:

Optional arg NO-CONFIRM-P means do not ask for confirmation to move.
Optional arg DETAILS is passed to `diredp-y-or-n-files-p'.

\(fn &optional DIR NO-CONFIRM-P DETAILS)" t nil)

(when (> emacs-major-version 21) (defun diredp-kill-this-tree nil "Remove this subdir listing and lower listings." (interactive) (dired-kill-tree (dired-current-directory) nil 'KILL-ROOT)))

(autoload 'diredp-insert-subdirs "dired+" "\
Insert the marked subdirectories.
Like using \\<dired-mode-map>`\\[dired-maybe-insert-subdir]' at each marked directory line.

\(fn &optional SWITCHES INTERACTIVEP)" t nil)

(autoload 'diredp-insert-subdirs-recursive "dired+" "\
Insert the marked subdirs, including those in marked subdirs.
Like `diredp-insert-subdirs', but act recursively on subdirs.
The subdirs inserted are those that are marked in the current Dired
buffer, or ALL subdirs in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way (their marked
subdirs are inserted...).

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-do-shell-command-recursive "dired+" "\
Run shell COMMAND on the marked files, including those in marked subdirs.
Like `dired-do-shell-command', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn COMMAND &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-do-symlink-recursive "dired+" "\
Make symbolic links to marked files, including those in marked subdirs.
Like `dired-do-symlink', but act recursively on subdirs to pick up the
files to link.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-create-files-recursive'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-do-hardlink-recursive "dired+" "\
Add hard links for marked files, including those in marked subdirs.
Like `dired-do-hardlink', but act recursively on subdirs to pick up the
files to link.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-create-files-recursive'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-do-print-recursive "dired+" "\
Print the marked files, including those in marked subdirs.
Like `dired-do-print', but act recursively on subdirs to pick up the
files to print.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-image-dired-display-thumbs-recursive "dired+" "\
Display thumbnails of marked files, including those in marked subdirs.
Like `image-dired-display-thumbs', but act recursively on subdirs.
Optional arguments APPEND and DO-NOT-POP are as for
`image-dired-display-thumbs'.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn &optional IGNORE-MARKS-P APPEND DO-NOT-POP DETAILS)" t nil)

(autoload 'diredp-image-dired-tag-files-recursive "dired+" "\
Tag marked files with an `image-dired' tag, including in marked subdirs.
Like `image-dired-tag-files', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-image-dired-delete-tag-recursive "dired+" "\
Remove `image-dired' tag for marked files, including in marked subdirs.
Like `image-dired-delete-tag', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-image-dired-comment-files-recursive "dired+" "\
Add comment to marked files in dired, including those in marked subdirs.
Like `image-dired-dired-comment-files' but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-do-bookmark-recursive "dired+" "\
Bookmark the marked files, including those in marked subdirs.
Like `diredp-do-bookmark', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn &optional IGNORE-MARKS-P PREFIX DETAILS)" t nil)

(autoload 'diredp-do-bookmark-dirs-recursive "dired+" "\
Bookmark this Dired buffer and marked subdirectory Dired buffers, recursively.
Create a Dired bookmark for this directory and for each of its marked
subdirectories.  Handle each of the marked subdirectory similarly:
bookmark it and its marked subdirectories, and so on, recursively.
Name each of these Dired bookmarks with the Dired buffer name.

After creating the Dired bookmarks, create a sequence bookmark, named
`DIRBUF and subdirs', where DIRBUF is the name of the original buffer.
This bookmark represents the whole Dired tree rooted in the directory
where you invoked the command.  Jumping to this sequence bookmark
restores all of the Dired buffers making up the tree, by jumping to
each of their bookmarks.

With a prefix arg, bookmark the marked and unmarked subdirectory Dired
buffers, recursively, that is, ignore markings.

Note:

* If there is more than one Dired buffer for a given subdirectory then
  only the first such is used.

* This command creates new bookmarks.  It never updates or overwrites
  an existing bookmark.

You need library `Bookmark+' for this command.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-subdirs'.

\(fn IGNORE-MARKS-P &optional DETAILS MSGP)" t nil)

(autoload 'diredp-do-bookmark-in-bookmark-file-recursive "dired+" "\
Bookmark files here and below in BOOKMARK-FILE and save BOOKMARK-FILE.
Like `diredp-do-bookmark-in-bookmark-file', but act recursively on
subdirs.  The files included are those that are marked in the current
Dired buffer, or all files in the directory if none are marked.
Marked subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp:
 * Optional arg BFILE-BOOKMARKP non-nil means create a bookmark-file
   bookmark for BOOKMARK-FILE.
 * Optional arg DETAILS is passed to `diredp-get-files'.

\(fn BOOKMARK-FILE &optional PREFIX IGNORE-MARKS-P BFILE-BOOKMARKP DETAILS)" t nil)

(autoload 'diredp-set-bookmark-file-bookmark-for-marked-recursive "dired+" "\
Bookmark the marked files and create a bookmark-file bookmark for them.
Like `diredp-set-bookmark-file-bookmark-for-marked', but act
recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-bookmark-in-bookmark-file-recursive'.

\(fn BOOKMARK-FILE &optional PREFIX IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-do-find-marked-files-recursive "dired+" "\
Find marked files simultaneously, including those in marked subdirs.
Like `dired-do-find-marked-files', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With (explicit) numeric prefix ARG >= 0, find the files but do not
display them.

With numeric prefix ARG <= 0, ignore all marks - include all files in
this Dired buffer and all subdirs, recursively.

Note that prefix-argument behavior is different for this command than
for `dired-do-find-marked-files'.  In particular, a negative numeric
prefix arg does not cause the files to be shown in separate frames.
Only non-nil `pop-up-frames' (or equivalent configuration) causes
the files to be shown in separate frames.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn &optional ARG DETAILS)" t nil)

(autoload 'diredp-do-query-replace-regexp-recursive "dired+" "\
Do `query-replace-regexp' on marked files, including in marked subdirs.
Query-replace FROM with TO.

Like `dired-do-query-replace-regexp', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With an (explicit) numeric prefix argument:

* >= 0 means ignore all marks - include ALL files in this Dired buffer
  and all subdirs, recursively.

* <= 0 means replace only word-delimited matches.

If you exit (`\\[keyboard-quit]', `RET' or `q'), you can resume the query replacement
using `\\[tags-loop-continue]'.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn FROM TO &optional ARG DETAILS)" t nil)

(autoload 'diredp-do-grep-recursive "dired+" "\
Run `grep' on marked files, including those in marked subdirs.
Like `diredp-do-grep', but act recursively on subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn COMMAND-ARGS &optional DETAILS)" t nil)

(autoload 'diredp-marked-recursive "dired+" "\
Open Dired on marked files, including those in marked subdirs.
Like `diredp-marked', but act recursively on subdirs.

See `diredp-do-find-marked-files-recursive' for a description of the
files included.  In particular, if no files are marked here or in a
marked subdir, then all files in the directory are included.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, DIRNAME here must be a string, not a cons.  It
is used as the name of the new Dired buffer.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn DIRNAME &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-marked-recursive-other-window "dired+" "\
Same as `diredp-marked-recursive', but uses a different window.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn DIRNAME &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-list-marked-recursive "dired+" "\
List the files marked here and in marked subdirs, recursively.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, all marks are ignored: all files in this Dired
buffer and all descendant directories are included.

You can use `RET' or `mouse-2' to visit any of the files.
If `tooltip-mode' is on then moving the mouse over image-file names
shows image previews.

When called from Lisp:
 Non-nil optional arg IGNORE-MARKS-P means ignore marks.
 Non-nil optional arg PREDICATE is a file-name predicate.  List only
  the files for which it returns non-nil.
 Non-nil optional arg DETAILS is passed to `diredp-list-files'.

\(fn &optional IGNORE-MARKS-P PREDICATE DETAILS)" t nil)

(autoload 'diredp-flag-auto-save-files-recursive "dired+" "\
Flag all auto-save files for deletion, including in marked subdirs.
A non-negative prefix arg means to unmark (unflag) them instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, flag all in this directory and all
descendant directories.

When called from Lisp, optional arg DETAILS is passed to
`diredp-mark-recursive-1'.

\(fn &optional ARG DETAILS)" t nil)

(autoload 'diredp-copy-filename-as-kill-recursive "dired+" "\
Copy names of marked files here and in marked subdirs, to `kill-ring'.
The names are separated by a space.

Like `dired-copy-filename-as-kill', but act recursively on subdirs.
\(Do not copy subdir names themselves.)

With no prefix arg, use relative file names.
With a zero prefix arg, use absolute file names.
With a plain prefix arg (`C-u'), use names relative to the current
Dired directory.  (This might contain slashes if in a subdirectory.)

If on a subdir headerline, use absolute subdir name instead - prefix
arg and marked files are ignored in this case.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

The names are copied to the kill ring and to variable
`diredp-last-copied-filenames'.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn &optional ARG DETAILS)" t nil)

(autoload 'diredp-copy-abs-filenames-as-kill-recursive "dired+" "\
Copy absolute names of files marked here and in marked subdirs, recursively.
The names are copied to the kill ring and to variable
`dired-copy-filename-as-kill'.

The files whose names are copied are those that are marked in the
current Dired buffer, or all files in the directory if none are
marked.  Marked subdirectories are handled recursively in the same
way.

When called from Lisp, optional arg DETAILS is passed to
`diredp-copy-filename-as-kill-recursive'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-mark-files-regexp-recursive "dired+" "\
Mark all files matching REGEXP, including those in marked subdirs.
Like `dired-mark-files-regexp' but act recursively on marked subdirs.

The file names to be matched by this command are always absolute -
they include the full directory.  Note that this does NOT correspond
to the default behavior for `dired-mark-files-regexp'.  The other
matching possibilities offered by `dired-mark-files-regexp' are not
available for this command.

Directories `.' and `..' are never marked.

A non-negative prefix arg means to UNmark the files instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all matching files in this
directory and all descendant directories.

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\\.o$' for
object files--just `.o' will mark more than you might think.

REGEXP is added to `regexp-search-ring', for regexp search.

Note: If there is more than one Dired buffer for a given subdirectory
then only the first such is used.

When called from Lisp, DETAILS is passed to `diredp-get-subdirs'.

\(fn REGEXP &optional MARKER-CHAR IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-mark-files-containing-regexp-recursive "dired+" "\
Mark files with contents containing a REGEXP match, including in marked subdirs.
Like `dired-mark-files-containing-regexp' but act recursively on
marked subdirs.

A non-negative prefix arg means to UNmark the files instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all matching files in this
directory and all descendant directories.

REGEXP is added to `regexp-search-ring', for regexp search.

Note: If there is more than one Dired buffer for a given subdirectory
then only the first such is used.

If a file is visited in a buffer and `dired-always-read-filesystem' is
nil, this looks in the buffer without revisiting the file, so the
results might be inconsistent with the file on disk if its contents
have changed since it was last visited.

When called from Lisp, DETAILS is passed to `diredp-get-subdirs'.

\(fn REGEXP &optional MARKER-CHAR IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-mark-autofiles-recursive "dired+" "\
Mark all autofiles, including in marked subdirs.
Autofiles are files that have an autofile bookmark.
A non-negative prefix arg means to unmark them instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all in this directory and all
descendant directories.

When called from Lisp, optional arg DETAILS is passed to
`diredp-mark-recursive-1'.

\(fn &optional ARG DETAILS)" t nil)

(autoload 'diredp-mark-executables-recursive "dired+" "\
Mark all executable files, including in marked subdirs.
The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

A non-negative prefix arg means to unmark them instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all in this directory and all
descendant directories.

When called from Lisp, optional arg DETAILS is passed to
`diredp-mark-recursive-1'.

\(fn &optional ARG DETAILS)" t nil)

(autoload 'diredp-mark-directories-recursive "dired+" "\
Mark all directories except `.' and `..', including in marked subdirs.
The directories included are those that are marked in the current
Dired buffer, or all subdirs in the directory if none are marked.
Marked subdirectories are handled recursively in the same way.

A non-negative prefix arg means to unmark them instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all in this directory and all
descendant directories.

When called from Lisp, optional arg DETAILS is passed to
`diredp-mark-recursive-1'.

\(fn &optional ARG DETAILS)" t nil)

(autoload 'diredp-mark-symlinks-recursive "dired+" "\
Mark all symbolic links, including in marked subdirs.
The symlinks included are those that are marked in the current Dired
buffer, or all symlinks in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

A non-negative prefix arg means to unmark them instead.

A non-positive prefix arg means to ignore subdir markings and act
instead on ALL subdirs.  That is, mark all in this directory and all
descendant directories.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-subdirs'.

\(fn &optional ARG DETAILS)" t nil)

(autoload 'diredp-capitalize-recursive "dired+" "\
Rename marked files, including in marked subdirs, by capitalizing them.
Like `diredp-capitalize', but act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-create-files-non-directory-recursive'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-upcase-recursive "dired+" "\
Rename marked files, including in marked subdirs, making them uppercase.
Like `dired-upcase', but act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-create-files-non-directory-recursive'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-downcase-recursive "dired+" "\
Rename marked files, including in marked subdirs, making them lowercase.
Like `dired-downcase', but act recursively on subdirs.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-create-files-non-directory-recursive'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-do-apply/eval-recursive "dired+" "\
Apply a function to marked files, or evaluate a sexp in them.
Like `diredp-do-apply/eval' but act recursively on subdirs, and do no
result- or error-logging or echoing.

The files acted on are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

With a plain prefix ARG (`C-u'), visit each file and eval the sexp or
 invoke the function there.  (The function is passed no arguments.)
Otherwise, apply the function to each file name.

Any prefix arg other than single `C-u' behaves according to the ARG
argument of `dired-get-marked-files'.  In particular, `C-u C-u'
operates on all files in the Dired buffer.

When called from Lisp:
 * If ARG is `(4)' then invoke the function, or eval the sexp, while
   visiting each file.
 * Optional arg DETAILS is passed to `diredp-get-files'.

\(fn FUN/SEXP &optional ARG DETAILS)" t nil)

(autoload 'diredp-do-delete-recursive "dired+" "\
Delete marked (not flagged) files, including in marked subdirs.
Like `dired-do-delete' but act recursively on subdirs.

The files to be deleted are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files' and `diredp-get-subdirs'.

\(fn ARG &optional DETAILS)" t nil)

(autoload 'diredp-do-move-recursive "dired+" "\
Move marked files, including in marked subdirs, to a given directory.
Like `dired-do-rename', but act recursively on subdirs to pick up the
files to move.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

This means move the marked files of marked subdirs and their marked
subdirs, etc.  It does not mean move or rename the subdirs themselves
recursively.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

Renames any buffers that are visiting the files.

The default suggested for the target directory depends on the value of
`dired-dwim-target', which see.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-do-copy-recursive "dired+" "\
Copy marked files, including in marked subdirs, to a given directory.
Like `dired-do-copy', but act recursively on subdirs to pick up the
files to copy.

The files included are those that are marked in the current Dired
buffer, or all files in the directory if none are marked.  Marked
subdirectories are handled recursively in the same way.

This means copy the marked files of marked subdirs and their marked
subdirs, etc.  It does not mean copy the subdirs themselves
recursively.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

Preserves the last-modified date when copying, unless
`dired-copy-preserve-time' is nil.

The default suggested for the target directory depends on the value of
`dired-dwim-target', which see.

This command copies symbolic links by creating new ones, like UNIX
command `cp -d'.

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-create-files-recursive'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-do-chmod-recursive "dired+" "\
Change the mode of the marked files, including those in marked subdirs.
Symbolic modes like `g+w' are allowed.

Note that marked subdirs are not changed.  Their markings are used only
to indicate that some of their files are to be changed.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files' and `diredp-do-redisplay-recursive'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-do-touch-recursive "dired+" "\
Change the timestamp of marked files, including those in marked subdirs.
This calls `touch'.  Like `dired-do-touch', but act recursively on
subdirs.  The subdirs inserted are those that are marked in the
current Dired buffer, or all subdirs in the directory if none are
marked.  Marked subdirectories are handled recursively in the same
way.

With a prefix argument, ignore all marks - include all files in this
Dired buffer and all subdirs, recursively.

When called from Lisp, optional arg DETAILS is passed to
`diredp-do-chxxx-recursive'.

\(fn &optional IGNORE-MARKS-P DETAILS)" t nil)

(autoload 'diredp-do-redisplay-recursive "dired+" "\
Redisplay marked file lines, including those in marked subdirs.
Non-nil MSGP means show status messages.
Like `dired-do-redisplay' with no args, but act recursively on
subdirs.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn &optional DETAILS MSGP)" t nil)

(autoload 'diredp-marked "dired+" "\
Open Dired on only the marked files or the next N files.
With a non-zero numeric prefix arg N, use the next abs(N) files.
A plain (`C-u'), zero, or negative prefix arg prompts for listing
switches as in command `dired'.

Note that the marked files can include files in inserted
subdirectories, so the Dired buffer that is opened can contain files
from multiple directories in the same tree.

\(fn DIRNAME &optional N SWITCHES)" t nil)

(autoload 'diredp-marked-other-window "dired+" "\
Same as `diredp-marked', but uses a different window.

\(fn DIRNAME &optional N SWITCHES)" t nil)

(autoload 'diredp-mark/unmark-extension "dired+" "\
Mark all files with a certain EXTENSION for use in later commands.
A `.' is not automatically prepended to the string entered.
Non-nil prefix argument UNMARK-P means unmark instead of mark.

Non-interactively, EXTENSION is the extension (a string).  It can also
  be a list of extension strings.
Optional argument UNMARK-P is the prefix arg.

\(fn EXTENSION &optional UNMARK-P)" t nil)

(autoload 'diredp-mark-files-tagged-all "dired+" "\
Mark all files that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then mark the files that have
 any tags at all (i.e., at least one tag).
With a prefix arg, mark all that are *not* tagged with *any* TAGS.
You need library `bookmark+.el' to use this command.

\(fn TAGS &optional NONE-P PREFIX)" t nil)

(autoload 'diredp-mark-files-tagged-none "dired+" "\
Mark all files that are not tagged with *any* tag in TAGS.
As a special case, if TAGS is empty, then mark the files that have
 no tags at all.
With a prefix arg, mark all that are tagged with *each* tag in TAGS.
You need library `bookmark+.el' to use this command.

\(fn TAGS &optional ALLP PREFIX)" t nil)

(autoload 'diredp-mark-files-tagged-some "dired+" "\
Mark all files that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then mark the files that have
 any tags at all (i.e., at least one tag).
With a prefix arg, mark all that are *not* tagged with *all* TAGS.
You need library `bookmark+.el' to use this command.

\(fn TAGS &optional SOMENOTP PREFIX)" t nil)

(autoload 'diredp-mark-files-tagged-not-all "dired+" "\
Mark all files that are not tagged with *all* TAGS.
As a special case, if TAGS is empty, then mark the files that have
 no tags at all.
With a prefix arg, mark all that are tagged with *some* TAGS.
You need library `bookmark+.el' to use this command.

\(fn TAGS &optional SOMEP PREFIX)" t nil)

(autoload 'diredp-mark-files-tagged-regexp "dired+" "\
Mark files that have at least one tag that matches REGEXP.
With a prefix arg, mark all that are tagged but have no matching tags.
You need library `bookmark+.el' to use this command.

\(fn REGEXP &optional NOTP PREFIX)" t nil)

(autoload 'diredp-unmark-files-tagged-regexp "dired+" "\
Unmark files that have at least one tag that matches REGEXP.
With a prefix arg, unmark all that are tagged but have no matching tags.
You need library `bookmark+.el' to use this command.

\(fn REGEXP &optional NOTP PREFIX)" t nil)

(autoload 'diredp-unmark-files-tagged-all "dired+" "\
Unmark all files that are tagged with *each* tag in TAGS.
As a special case, if TAGS is empty, then unmark the files that have
 any tags at all (i.e., at least one tag).
With a prefix arg, unmark all that are *not* tagged with *any* TAGS.
You need library `bookmark+.el' to use this command.

\(fn TAGS &optional NONE-P PREFIX)" t nil)

(autoload 'diredp-unmark-files-tagged-none "dired+" "\
Unmark all files that are *not* tagged with *any* tag in TAGS.
As a special case, if TAGS is empty, then unmark the files that have
 no tags at all.
With a prefix arg, unmark all that are tagged with *each* tag in TAGS.
You need library `bookmark+.el' to use this command.

\(fn TAGS &optional ALLP PREFIX)" t nil)

(autoload 'diredp-unmark-files-tagged-some "dired+" "\
Unmark all files that are tagged with *some* tag in TAGS.
As a special case, if TAGS is empty, then unmark the files that have
 any tags at all.
With a prefix arg, unmark all that are *not* tagged with *all* TAGS.
You need library `bookmark+.el' to use this command.

\(fn TAGS &optional SOMENOTP PREFIX)" t nil)

(autoload 'diredp-unmark-files-tagged-not-all "dired+" "\
Unmark all files that are *not* tagged with *all* TAGS.
As a special case, if TAGS is empty, then unmark the files that have
 no tags at all.
With a prefix arg, unmark all that are tagged with *some* TAGS.
You need library `bookmark+.el' to use this command.

\(fn TAGS &optional SOMEP PREFIX)" t nil)

(autoload 'diredp-do-tag "dired+" "\
Tag the marked (or the next prefix argument) files.
You need library `bookmark+.el' to use this command.

Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag.  Completion is lax: you are
not limited to existing tags.

TAGS is a list of strings.  PREFIX is as for `diredp-do-bookmark'.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

\(fn TAGS &optional PREFIX ARG)" t nil)

(autoload 'diredp-mouse-do-tag "dired+" "\
In Dired, add some tags to this file.
You need library `bookmark+.el' to use this command.

\(fn EVENT)" t nil)

(autoload 'diredp-do-untag "dired+" "\
Remove some tags from the marked (or the next prefix arg) files.
You need library `bookmark+.el' to use this command.

Hit `RET' to enter each tag, then hit `RET' again after the last tag.
You can use completion to enter each tag.  Completion is lax: you are
not limited to existing tags.

TAGS is a list of strings.  PREFIX is as for `diredp-do-bookmark'.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

\(fn TAGS &optional PREFIX ARG)" t nil)

(autoload 'diredp-mouse-do-untag "dired+" "\
In Dired, remove some tags from this file.
You need library `bookmark+.el' to use this command.

\(fn EVENT)" t nil)

(autoload 'diredp-do-remove-all-tags "dired+" "\
Remove all tags from the marked (or the next prefix arg) files.
You need library `bookmark+.el' to use this command.

PREFIX is as for `diredp-do-bookmark'.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

\(fn &optional PREFIX ARG)" t nil)

(autoload 'diredp-mouse-do-remove-all-tags "dired+" "\
In Dired, remove all tags from the marked (or next prefix arg) files.
You need library `bookmark+.el' to use this command.

\(fn EVENT)" t nil)

(autoload 'diredp-do-paste-add-tags "dired+" "\
Add previously copied tags to the marked (or next prefix arg) files.
The tags were previously copied from a file to `bmkp-copied-tags'.
You need library `bookmark+.el' to use this command.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

\(fn &optional PREFIX ARG)" t nil)

(autoload 'diredp-mouse-do-paste-add-tags "dired+" "\
In Dired, add previously copied tags to this file.
The tags were previously copied from a file to `bmkp-copied-tags'.
You need library `bookmark+.el' to use this command.

\(fn EVENT)" t nil)

(autoload 'diredp-do-paste-replace-tags "dired+" "\
Replace tags for marked (or next prefix arg) files with copied tags.
The tags were previously copied from a file to `bmkp-copied-tags'.
You need library `bookmark+.el' to use this command.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

\(fn &optional PREFIX ARG)" t nil)

(autoload 'diredp-mouse-do-paste-replace-tags "dired+" "\
In Dired, replace tags for this file with tags copied previously.
The tags were previously copied from a file to `bmkp-copied-tags'.
You need library `bookmark+.el' to use this command.

\(fn EVENT)" t nil)

(autoload 'diredp-do-set-tag-value "dired+" "\
Set TAG value to VALUE, for the marked (or next prefix arg) files.
This does not change the TAG name.
You need library `bookmark+.el' to use this command.

PREFIX is as for `diredp-do-bookmark'.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

\(fn TAG VALUE &optional PREFIX ARG)" t nil)

(autoload 'diredp-mouse-do-set-tag-value "dired+" "\
In Dired, set the value of a tag for this file.
This does not change the tag name.
You need library `bookmark+.el' to use this command.

\(fn EVENT)" t nil)

(autoload 'diredp-mark-autofiles "dired+" "\
Mark all autofiles, that is, files that have an autofile bookmark.

\(fn)" t nil)

(autoload 'diredp-unmark-autofiles "dired+" "\
Unmark all autofiles, that is, files that have an autofile bookmark.

\(fn)" t nil)

(autoload 'diredp-mark/unmark-autofiles "dired+" "\
Mark all autofiles, or unmark if UNMARKP is non-nil.

\(fn &optional UNMARKP)" nil nil)

(autoload 'diredp-do-bookmark "dired+" "\
Bookmark the marked (or the next prefix argument) files.
Each bookmark name is the non-directory portion of the file name,
 prefixed by PREFIX if it is non-nil.
Interactively, you are prompted for the PREFIX if
 `diredp-prompt-for-bookmark-prefix-flag' is non-nil.
The bookmarked position is the beginning of the file.
If you use library `bookmark+.el' then the bookmark is an autofile.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

\(fn &optional PREFIX ARG)" t nil)

(autoload 'diredp-mouse-do-bookmark "dired+" "\
In Dired, bookmark this file.  See `diredp-do-bookmark'.

\(fn EVENT)" t nil)

(autoload 'diredp-set-bookmark-file-bookmark-for-marked "dired+" "\
Bookmark the marked files and create a bookmark-file bookmark for them.
The bookmarked position is the beginning of the file.
Jumping to the bookmark-file bookmark loads the set of file bookmarks.
You need library `bookmark+.el' to use this command.

Each bookmark name is the non-directory portion of the file name,
 prefixed by PREFIX if it is non-nil.
Interactively, you are prompted for PREFIX if
 `diredp-prompt-for-bookmark-prefix-flag' is non-nil.

A prefix argument ARG specifies files to use instead of those marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

You are also prompted for the bookmark file, BOOKMARK-FILE.  The
default is `.emacs.bmk' in the current directory, but you can enter
any file name, anywhere.

The marked-file bookmarks are added to file BOOKMARK-FILE, but this
command does not make BOOKMARK-FILE the current bookmark file.  To
make it current, just jump to the bookmark-file bookmark created by
this command.  That bookmark (which bookmarks BOOKMARK-FILE) is
defined in that current bookmark file.

Example:

 Bookmark file `~/.emacs.bmk' is current before invoking this command.
 The current (Dired) directory is `/foo/bar'.
 The marked files are bookmarked in the (possibly new) bookmark file
   `/foo/bar/.emacs.bmk'.
 The bookmarks for the marked files have names prefixed by `FOOBAR '.
 The name of the bookmark-file bookmark is `Foobar Files'.
 Bookmark `Foobar Files' is itself in bookmark file `~/.emacs.bmk'.
 Bookmark file `~/.emacs.bmk' is current after invoking this command.

You are prompted for the name of the bookmark-file bookmark, the
BOOKMARK-FILE for the marked-file bookmarks, and a PREFIX string for
each of the marked-file bookmarks.

See also command `diredp-do-bookmark-in-bookmark-file'.

\(fn BOOKMARK-FILE &optional PREFIX ARG)" t nil)

(autoload 'diredp-do-bookmark-in-bookmark-file "dired+" "\
Bookmark marked files in BOOKMARK-FILE and save BOOKMARK-FILE.
The files bookmarked are the marked files, by default.
The bookmarked position is the beginning of the file.
You are prompted for BOOKMARK-FILE.  The default is `.emacs.bmk' in
the current directory, but you can enter any file name, anywhere.
You need library `bookmark+.el' to use this command.

The marked files are bookmarked in file BOOKMARK-FILE, but this
command does not make BOOKMARK-FILE the current bookmark file.  To
make it current, use `\\[bmkp-switch-bookmark-file]' (`bmkp-switch-bookmark-file').

Each bookmark name is the non-directory portion of the file name,
 prefixed by PREFIX if it is non-nil.
Interactively, you are prompted for PREFIX if
 `diredp-prompt-for-bookmark-prefix-flag' is non-nil.

Interactively, a prefix argument ARG specifies the files to use
instead of those marked.

 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

See also command `diredp-set-bookmark-file-bookmark-for-marked'.

Non-interactively:

 * Non-nil BFILE-BOOKMARKP means create a bookmark-file bookmark for
   BOOKMARK-FILE.
 * Non-nil FILES is the list of files to bookmark.

\(fn BOOKMARK-FILE &optional PREFIX ARG BFILE-BOOKMARKP FILES)" t nil)

(autoload 'diredp-find-a-file "dired+" "\
`find-file', but use file on current line as default (`M-n').

\(fn FILENAME &optional WILDCARDS)" t nil)

(autoload 'diredp-find-a-file-other-frame "dired+" "\
`find-file-other-frame', but use file under cursor as default (`M-n').

\(fn FILENAME &optional WILDCARDS)" t nil)

(autoload 'diredp-find-a-file-other-window "dired+" "\
`find-file-other-window', but use file under cursor as default (`M-n').

\(fn FILENAME &optional WILDCARDS)" t nil)

(autoload 'diredp-find-a-file-read-args "dired+" "\


\(fn PROMPT MUSTMATCH)" nil nil)

(autoload 'diredp-find-file-reuse-dir-buffer "dired+" "\
Like `dired-find-file', but reuse Dired buffers.
Unlike `dired-find-alternate-file' this does not use
`find-alternate-file' unless (1) the target is a directory that is not
yet visited as a Dired buffer, and (2) the current (Dired) buffer is
not visited also in some other window (possibly in an iconified
frame).

\(fn)" t nil)

(autoload 'diredp-mouse-find-file-reuse-dir-buffer "dired+" "\
Like `dired-mouse-find-file', but reuse Dired buffers.
Unlike `dired-find-alternate-file' this does not use
`find-alternate-file' unless (1) the target is a directory that is not
yet visited as a Dired buffer, and (2) the current (Dired) buffer is
not visited also in some other window (possibly in an iconified
frame).

Non-nil optional args FIND-FILE-FUNC and FIND-DIR-FUNC specify
functions to visit the file and directory, respectively.
Defaults: `find-file' and `dired', respectively.

\(fn EVENT &optional FIND-FILE-FUNC FIND-DIR-FUNC)" t nil)

(defalias 'toggle-diredp-find-file-reuse-dir 'diredp-toggle-find-file-reuse-dir)

(autoload 'diredp-toggle-find-file-reuse-dir "dired+" "\
Toggle whether Dired `find-file' commands reuse directories.
This applies also to `dired-w32-browser' commands and
`diredp-up-directory'.

A prefix arg specifies directly whether or not to reuse.
 If its numeric value is non-negative then reuse; else do not reuse.

To set the behavior as a preference (default behavior), put this in
your ~/.emacs, where VALUE is 1 to reuse or -1 to not reuse:

 (diredp-toggle-find-file-reuse-dir VALUE)

Note: This affects only these commands:

  `dired-find-file'
  `dired-mouse-find-file'

It does not affect the corresponding `-other-window' commands.  Note
too that, by default, mouse clicks to open files or directories open
in another window: command `diredp-mouse-find-file-other-window', not
`dired-mouse-find-file'.  If you want a mouse click to reuse a
directory then bind `mouse-2' to `dired-mouse-find-file' instead.

\(fn FORCE-P)" t nil)

(autoload 'diredp-omit-marked "dired+" "\
Omit lines of marked files.  Return the number of lines omitted.

\(fn)" t nil)

(autoload 'diredp-omit-unmarked "dired+" "\
Omit lines of unmarked files.  Return the number of lines omitted.

\(fn)" t nil)

(autoload 'diredp-ediff "dired+" "\
Compare file at cursor with file FILE2 using `ediff'.
FILE2 defaults to the file at the cursor as well.  If you enter just a
directory name for FILE2, then the file at the cursor is compared with
a file of the same name in that directory.  FILE2 is the second file
given to `ediff'; the file at the cursor is the first.

Try to guess a useful default value for FILE2, as follows:

* If the mark is active, use the file at mark.
* Else if the file at cursor is a autosave file or a backup file, use
  the corresponding base file.
* Else if there is any backup file for the file at point, use the
  newest backup file for it.
* Else use the file at point.

\(fn FILE2)" t nil)

(autoload 'diredp-do-apply/eval "dired+" "\
Apply a function to the marked files, or evaluate a sexp in them.
You are prompted for the function or sexp.

With a plain prefix ARG (`C-u'), visit each file, and eval the sexp or
 invoke the function there.  (The function is passed no arguments.)
Otherwise, apply the function to each file name.

Any prefix arg other than single `C-u' behaves according to the ARG
argument of `dired-get-marked-files'.  In particular, `C-u C-u'
operates on all files in the Dired buffer.

The result returned for each file is logged by `dired-log'.  Use `?'
to see all such results and any error messages.  If there are fewer
marked files than `diredp-do-report-echo-limit' then each result is
also echoed momentarily.

When called from Lisp, if ARG is `(4)' then eval the sexp or invoke
the function while visiting each file.

\(fn FUN/SEXP &optional ARG)" t nil)

(autoload 'dired-do-compress "dired+" "\
Compress or uncompress marked (or next prefix argument) files.
A prefix argument ARG specifies files to use instead of marked.
 An integer means use the next ARG files (previous -ARG, if < 0).
 `C-u': Use the current file (whether or not any are marked).
 `C-u C-u': Use all files in Dired, except directories.
 `C-u C-u C-u': Use all files and directories, except `.' and `..'.
 `C-u C-u C-u C-u': Use all files and all directories.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-byte-compile "dired+" "\
Byte compile marked Emacs Lisp files.
A prefix argument ARG specifies files to use instead of those marked.
 * An integer means use the next ARG files (previous -ARG, if < 0).
 * Two or more `C-u' (e.g. `C-u C-u') means ignore any marks and use
   all files in the Dired buffer.
 * Any other prefix arg means use the current file.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-load "dired+" "\
Load the marked Emacs Lisp files.
A prefix argument ARG specifies files to use instead of those marked.
 * An integer means use the next ARG files (previous -ARG, if < 0).
 * Two or more `C-u' (e.g. `C-u C-u') means ignore any marks and use
   all files in the Dired buffer.
 * Any other prefix arg means use the current file.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-search "dired+" "\
Search through all marked files for a match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

A prefix arg behaves as follows:
 * An integer means use the next ARG files (previous -ARG, if < 0).
 * Two or more `C-u' (e.g. `C-u C-u') means ignore any marks and use
   all files in the Dired buffer.
 * Any other prefix arg means use the current file.

When invoked interactively, raise an error if no files are marked.

\(fn REGEXP &optional ARG INTERACTIVEP)" t nil)

(autoload 'dired-do-query-replace-regexp "dired+" "\
Do `query-replace-regexp' of FROM with TO, on all marked files.
NOTE: A prefix arg for this command acts differently than for other
commands, so that you can use it to request word-delimited matches.

With a prefix argument:
 * An odd number of plain `C-u': act on the marked files, but replace
   only word-delimited matches.
 * More than one plain `C-u': act on all files, ignoring whether any
   are marked.
 * Any other prefix arg: Act on the next numeric-prefix files.

So for example:
 * `C-u C-u C-u': act on all files, replacing word-delimited matches.
 * `C-u 4': act on the next 4 files.  `C-4' means the same thing.
 * `C-u': act on the marked files, replacing word-delimited matches.

When invoked interactively, raise an error if no files are marked.

If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue].

\(fn FROM TO &optional ARG INTERACTIVEP)" t nil)

(autoload 'diredp-do-grep "dired+" "\
Run `grep' on marked (or next prefix arg) files.
A prefix argument behaves according to the ARG argument of
`dired-get-marked-files'.  In particular, `C-u C-u' operates on all
files in the Dired buffer.

\(fn COMMAND-ARGS)" t nil)

(autoload 'dired-maybe-insert-subdir "dired+" "\
Move to Dired subdirectory line or subdirectory listing.
This bounces you back and forth between a subdirectory line and its
inserted listing header line.  Using it on a non-directory line in a
subdirectory listing acts the same as using it on the subdirectory
header line.

* If on a subdirectory line, then go to the subdirectory's listing,
  creating it if not yet present.

* If on a subdirectory listing header line or a non-directory file in
  a subdirectory listing, then go to the line for the subdirectory in
  the parent directory listing.

* If on a non-directory file in the top Dired directory listing, do
  nothing.

Subdirectories are listed in the same position as for `ls -lR' output.

With a prefix arg, you can edit the `ls' switches used for this
listing.  Add `R' to the switches to expand the directory tree under a
subdirectory.

Dired remembers the switches you specify with a prefix arg, so
reverting the buffer does not reset them.  However, you might
sometimes need to reset some subdirectory switches after a
`dired-undo'.  You can reset all subdirectory switches to the
default value using \\<dired-mode-map>\\[dired-reset-subdir-switches].  See Info node
`(emacs)Subdir switches' for more details.

\(fn DIRNAME &optional SWITCHES NO-ERROR-IF-NOT-DIR-P)" t nil)

(autoload 'dired-do-find-marked-files "dired+" "\
Find marked files, displaying all of them simultaneously.
With no prefix argument:

* If `pop-up-frames' is nil then split the current window across all
  marked files, as evenly as possible.  Remaining lines go to the
  bottom-most window.  The number of files that can be displayed this
  way is restricted by the height of the current window and
  `window-min-height'.

* If `pop-up-frames' is non-nil then show each marked file in a
  separate frame (not window).

With a prefix argument:

* One or more plain `C-u' behaves as for `dired-get-marked-files'.
  In particular, `C-u C-u' means ignore any markings and operate on
  ALL files and directories (except `.' and `..') in the Dired buffer.

* A numeric prefix arg >= 0 means just find (visit) the marked files -
  do not show them.

* A numeric prefix arg < 0 means show each marked file in a separate
  frame (not window).  (This is the same behavior as no prefix arg
  with non-nil `pop-up-frames'.)

Note that a numeric prefix argument acts differently with this command
than it does with other `dired-do-*' commands: it does NOT act on the
next or previous (abs ARG) files, ignoring markings.

To keep the Dired buffer displayed, split the window (e.g., `C-x 2')
first.  To show only the marked files, type `\\[delete-other-windows]' first.

When invoked interactively, raise an error if no files are marked.

\(fn &optional ARG INTERACTIVEP)" t nil)

(autoload 'diredp-up-directory "dired+" "\
Run Dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary.

With a prefix arg, Dired the parent directory in another window.

On MS Windows, if you are already at the root directory, invoke
`diredp-w32-drives' to visit a navigable list of Windows drives.

\(fn &optional OTHER-WINDOW)" t nil)

(autoload 'diredp-up-directory-reuse-dir-buffer "dired+" "\
Like `diredp-up-directory', but reuse Dired buffers.
With a prefix arg, Dired the parent directory in another window.

On MS Windows, moving up from a root Dired buffer does not kill that
buffer (the Windows drives buffer is not really a Dired buffer).

\(fn &optional OTHER-WINDOW)" t nil)

(autoload 'diredp-next-line "dired+" "\
Move down lines then position cursor at filename.
If `goal-column' is non-nil then put the cursor at that column.
Optional prefix ARG says how many lines to move; default is one line.

If `diredp-wrap-around-flag' is non-nil then wrap around if none is
found before the buffer end (buffer beginning, if ARG is negative).
Otherwise, just move to the buffer limit.

\(fn ARG)" t nil)

(autoload 'diredp-previous-line "dired+" "\
Move up lines then position cursor at filename.
If `goal-column' is non-nil then put the cursor at that column.
Optional prefix ARG says how many lines to move; default is one line.

If `diredp-wrap-around-flag' is non-nil then wrap around if none is
found before the buffer beginning (buffer end, if ARG is negative).
Otherwise, just move to the buffer limit.

\(fn ARG)" t nil)

(autoload 'diredp-next-dirline "dired+" "\
Goto ARGth next directory file line.
If `diredp-wrap-around-flag' is non-nil then wrap around if none is
found before the buffer beginning (buffer end, if ARG is negative).
Otherwise, raise an error or, if NO-ERROR-IF-NOT-FOUND is nil, return
nil.

\(fn ARG &optional OPOINT)" t nil)

(autoload 'diredp-prev-dirline "dired+" "\
Goto ARGth previous directory file line.

\(fn ARG)" t nil)

(autoload 'diredp-next-subdir "dired+" "\
Go to the next subdirectory, regardless of level.
If ARG = 0 then go to this directory's header line.

If `diredp-wrap-around-flag' is non-nil then wrap around if none is
found before the buffer end (buffer beginning, if ARG is negative).
Otherwise, raise an error or, if NO-ERROR-IF-NOT-FOUND is nil, return
nil.

Non-nil NO-SKIP means do not move to end of header line, and return
the position moved to so far.

\(fn ARG &optional NO-ERROR-IF-NOT-FOUND NO-SKIP)" t nil)

(autoload 'diredp-prev-subdir "dired+" "\
Go to the previous subdirectory, regardless of level.
When called interactively and not on a subdir line, go to this subdir's line.
Otherwise, this is a mirror image of `diredp-next-subdir'.

\(fn ARG &optional NO-ERROR-IF-NOT-FOUND NO-SKIP)" t nil)

(autoload 'dired-do-flagged-delete "dired+" "\
In Dired, delete the files flagged for deletion.
NOTE: This deletes flagged, not marked, files.
If arg NO-MSG is non-nil, no message is displayed.

User option `dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed.

\(fn &optional NO-MSG)" t nil)

(autoload 'dired-do-delete "dired+" "\
Delete all marked (or next ARG) files.
NOTE: This deletes marked, not flagged, files.
`dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed.

\(fn &optional ARG)" t nil)

(autoload 'dired-mark-files-regexp "dired+" "\
Mark all file names matching REGEXP for use in later commands.
`.' and `..' are never marked or unmarked by this command.

Whether to mark or unmark, and what form of file name to match, are
governed by the prefix argument.  For this, a plain (`C-u') or a
double-plain (`C-u C-u') prefix arg is considered only as such - it is
not considered numerically.

Whether to mark or unmark:

 - No prefix arg, a positive arg, or a negative arg means mark.

 - Plain (`C-u'), double-plain (`C-u C-u'), or zero (e.g. `M-0' means
   unmark.

The form of a file name used for matching:

 - No prefix arg (to mark) or a plain prefix arg (`C-u', to unmark)
   means use the relative file name (no directory part).

 - A negative arg (e.g. `M--', to mark) or a zero arg (e.g. `M-0'), to
   unmark) means use the absolute file name, that is, including all
   directory components.

 - A positive arg (e.g. `M-9', to mark) or a double plain arg (`C-u
   C-u', to unmark) means construct the name relative to
   `default-directory'.  For an entry in an inserted subdir listing,
   this means prefix the relative file name (no directory part) with
   the subdir name relative to `default-directory'.

Note that the default matching behavior of this command is different
for Dired+ than it is for vanilla Emacs.  Using a positive prefix arg
or a double plain prefix arg (`C-u C-u') gives you the same behavior
as vanilla Emacs (marking or unmarking, respectively): matching
against names that are relative to the `default-directory'.

What Dired+ offers in addition is the possibility to match against
names that are relative (have no directory part - no prefix arg or
`C-u' to mark and unmark, respectively) or absolute (`M--' or `M-0',
respectively).

The default behavior uses relative names because this
is likely to be the more common use case.  But matching against
absolute names gives you more flexibility.

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\\.o$'
for object files--just `.o' might mark more than you might expect.

REGEXP is added to `regexp-search-ring', for regexp search.

Non-interactively:
 MARKER-CHAR is the marker character - used for `dired-marker-char'.
 LOCALP is passed to `dired-get-filename'.  It determines the form of
   filename that is matched against REGEXP.

\(fn REGEXP &optional MARKER-CHAR LOCALP)" t nil)

(autoload 'dired-mark-files-containing-regexp "dired+" "\
Mark files with contents containing a REGEXP match.
A prefix argument means unmark them instead.
`.' and `..' are never marked.

If a file is visited in a buffer and `dired-always-read-filesystem' is
nil, this looks in the buffer without revisiting the file, so the
results might be inconsistent with the file on disk if its contents
have changed since it was last visited.

\(fn REGEXP &optional MARKER-CHAR)" t nil)

(autoload 'dired-mark-symlinks "dired+" "\
Mark all symbolic links.
With prefix argument, unmark or unflag all those files.

\(fn UNFLAG-P)" t nil)

(autoload 'dired-mark-directories "dired+" "\
Mark all directory file lines except `.' and `..'.
With prefix argument, unmark or unflag the files instead.

\(fn UNFLAG-P)" t nil)

(autoload 'dired-mark-executables "dired+" "\
Mark all executable files.
With prefix argument, unmark or unflag the files instead.

\(fn UNFLAG-P)" t nil)

(autoload 'dired-flag-auto-save-files "dired+" "\
Flag for deletion files whose names suggest they are auto save files.
A prefix argument says to unmark or unflag the files instead.

\(fn &optional UNFLAG-P)" t nil)

(autoload 'diredp-capitalize "dired+" "\
Rename all marked (or next ARG) files by capitalizing them.
Makes the first char of the name uppercase and the others lowercase.

\(fn &optional ARG)" t nil)

(autoload 'diredp-delete-this-file "dired+" "\
In Dired, delete this file, upon confirmation.
This uses `delete-file'.
If the file is a symlink, remove the symlink.  If the file has
multiple names, it continues to exist with the other names.

For Emacs 24 and later, a prefix arg means that if
`delete-by-moving-to-trash' is non-nil then trash the file instead of
deleting it.

\(fn &optional USE-TRASH-CAN)" t nil)

(autoload 'diredp-capitalize-this-file "dired+" "\
In Dired, rename this file by capitalizing it.
Makes the first char of the name uppercase and the others lowercase.

\(fn)" t nil)

(autoload 'diredp-downcase-this-file "dired+" "\
In Dired, rename this file to lower case.

\(fn)" t nil)

(autoload 'diredp-upcase-this-file "dired+" "\
In Dired, rename this file to upper case.

\(fn)" t nil)

(autoload 'diredp-rename-this-file "dired+" "\
In Dired, rename this file.

\(fn)" t nil)

(autoload 'diredp-copy-this-file "dired+" "\
In Dired, copy this file.

\(fn)" t nil)

(autoload 'diredp-relsymlink-this-file "dired+" "\
In Dired, make a relative symbolic link to file on cursor line.

\(fn)" t nil)

(autoload 'diredp-symlink-this-file "dired+" "\
In Dired, make a symbolic link to this file.

\(fn)" t nil)

(autoload 'diredp-hardlink-this-file "dired+" "\
In Dired, add a name (hard link) to this file.

\(fn)" t nil)

(autoload 'diredp-print-this-file "dired+" "\
In Dired, print this file.

\(fn)" t nil)

(autoload 'diredp-grep-this-file "dired+" "\
In Dired, grep this file.

\(fn)" t nil)

(autoload 'diredp-compress-this-file "dired+" "\
In Dired, compress or uncompress this file.

\(fn)" t nil)

(autoload 'diredp-async-shell-command-this-file "dired+" "\
Run a shell COMMAND asynchronously on the file on the Dired cursor line.
Like `diredp-shell-command-this-file', but adds `&' at the end of
COMMAND to execute it asynchronously.  The command output appears in
buffer `*Async Shell Command*'.

\(fn COMMAND FILELIST)" t nil)

(autoload 'diredp-shell-command-this-file "dired+" "\
In Dired, run a shell COMMAND on this file.

\(fn COMMAND FILELIST)" t nil)

(autoload 'diredp-bookmark-this-file "dired+" "\
In Dired, bookmark this file.
See `diredp-do-bookmark'.

\(fn &optional PREFIX)" t nil)

(autoload 'diredp-tag-this-file "dired+" "\
In Dired, add some tags to this file.
You need library `bookmark+.el' to use this command.

\(fn TAGS &optional PREFIX)" t nil)

(autoload 'diredp-untag-this-file "dired+" "\
In Dired, remove some tags from this file.
With a prefix arg, remove all tags from the file.
You need library `bookmark+.el' to use this command.

\(fn TAGS &optional PREFIX ARG)" t nil)

(autoload 'diredp-remove-all-tags-this-file "dired+" "\
In Dired, remove all tags from this file.
You need library `bookmark+.el' to use this command.

\(fn &optional PREFIX MSGP)" t nil)

(autoload 'diredp-paste-add-tags-this-file "dired+" "\
In Dired, add previously copied tags to this file.
See `diredp-paste-add-tags'.
You need library `bookmark+.el' to use this command.

\(fn &optional PREFIX MSGP)" t nil)

(autoload 'diredp-paste-replace-tags-this-file "dired+" "\
In Dired, replace tags for this file with previously copied tags.
See `diredp-paste-replace-tags'.
You need library `bookmark+.el' to use this command.

\(fn &optional PREFIX MSGP)" t nil)

(autoload 'diredp-set-tag-value-this-file "dired+" "\
In Dired, Set value of TAG to VALUE for this file.
See `diredp-set-tag-value'.
You need library `bookmark+.el' to use this command.

\(fn TAG VALUE &optional PREFIX MSGP)" t nil)

(autoload 'diredp-copy-tags-this-file "dired+" "\
In Dired, copy the tags from this file, so you can paste them to another.
See `diredp-copy-tags'.
You need library `bookmark+.el' to use this command.

\(fn &optional PREFIX MSGP)" t nil)

(autoload 'diredp-mouse-copy-tags "dired+" "\
In Dired, copy the tags from this file, so you can paste them to another.
You need library `bookmark+.el' to use this command.

\(fn EVENT)" t nil)

(defalias 'diredp-show-metadata 'diredp-describe-autofile)

(autoload 'diredp-describe-autofile "dired+" "\
Show the metadata for the file of the current line.
The file must name an autofile bookmark.  The metadata is the bookmark
information.

With a prefix argument, show the internal definition of the bookmark.

You need library `bookmark+.el' for this command.

\(fn &optional INTERNAL-FORM-P)" t nil)

(defalias 'diredp-show-metadata-for-marked 'diredp-describe-marked-autofiles)

(autoload 'diredp-describe-marked-autofiles "dired+" "\
Show metadata for the marked files.
If no file is marked, describe ALL autofiles in this directory.
With a prefix argument, show the internal (Lisp) form of the metadata.
When invoked interactively, raise an error if no files are marked.
You need library `bookmark+.el' for this command.

When called from Lisp, optional arg DETAILS is passed to
`diredp-get-files'.

\(fn &optional INTERNAL-FORM-P INTERACTIVEP DETAILS)" t nil)

(autoload 'diredp-byte-compile-this-file "dired+" "\
In Dired, byte compile the (Lisp source) file on the cursor line.

\(fn)" t nil)

(autoload 'diredp-load-this-file "dired+" "\
In Dired, load this file.

\(fn)" t nil)

(autoload 'diredp-chmod-this-file "dired+" "\
In Dired, change the mode of this file.

\(fn)" t nil)

(autoload 'dired-mark-sexp "dired+" "\
Mark files for which PREDICATE returns non-nil.
With a prefix arg, unmark or unflag those files instead.

PREDICATE is a lisp sexp that can refer to the following symbols as
variables:

    `mode'   [string]  file permission bits, e.g. \"-rw-r--r--\"
    `nlink'  [integer] number of links to file
    `size'   [integer] file size in bytes
    `uid'    [string]  owner
    `gid'    [string]  group (If the gid is not displayed by `ls',
                       this will still be set (to the same as uid))
    `time'   [string]  the time that `ls' displays, e.g. \"Feb 12 14:17\"
    `name'   [string]  the name of the file
    `sym'    [string]  if file is a symbolic link, the linked-to name,
                       else \"\"
    `inode'  [integer] the inode of the file (only for `ls -i' output)
    `blks'   [integer] the size of the file for `ls -s' output
                       (ususally in blocks or, with `-k', in Kbytes)
Examples:
  Mark zero-length files: `(equal 0 size)'
  Mark files last modified on Feb 2: `(string-match \"Feb  2\" time)'
  Mark uncompiled Emacs Lisp files (`.el' file without a `.elc' file):
     First, Dired just the source files: `dired *.el'.
     Then, use \\[dired-mark-sexp] with this sexp:
          (not (file-exists-p (concat name \"c\")))

There's an ambiguity when a single integer not followed by a unit
prefix precedes the file mode: It is then parsed as inode number
and not as block size (this always works for GNU coreutils ls).

Another limitation is that the uid field is needed for the
function to work correctly.  In particular, the field is not
present for some values of `ls-lisp-emulation'.

This function operates only on the Dired buffer content.  It does not
refer at all to the underlying file system.  Contrast this with
`find-dired', which might be preferable for the task at hand.

\(fn PREDICATE &optional UNMARK-P)" t nil)

(autoload 'diredp-mark-region-files "dired+" "\
Mark all of the files in the current region (if it is active).
With non-nil prefix arg, unmark them instead.

\(fn &optional UNMARK-P)" t nil)

(autoload 'diredp-unmark-region-files "dired+" "\
Unmark all of the files in the current region (if it is active).
With non-nil prefix arg, mark them instead.

\(fn &optional MARK-P)" t nil)

(autoload 'diredp-flag-region-files-for-deletion "dired+" "\
Flag all of the files in the current region (if it is active) for deletion.

\(fn)" t nil)

(autoload 'diredp-toggle-marks-in-region "dired+" "\
Toggle marks in the region.

\(fn START END)" t nil)

(autoload 'diredp-mouse-3-menu "dired+" "\
Dired pop-up `mouse-3' menu, for files in selection or current line.

\(fn EVENT)" t nil)

(autoload 'diredp-find-file-other-frame "dired+" "\
In Dired, visit this file or directory in another frame.

\(fn)" t nil)

(autoload 'diredp-mouse-find-file-other-frame "dired+" "\
In Dired, visit file or directory clicked on in another frame.

\(fn EVENT)" t nil)

(autoload 'dired-mouse-find-file-other-window "dired+" "\
In Dired, visit the file or directory name you click on.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-view-file "dired+" "\
Examine this file in view mode, returning to Dired when done.
When file is a directory, show it in this buffer if it is inserted;
otherwise, display it in another buffer.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-ediff "dired+" "\
Compare this file (pointed by mouse) with file FILE2 using `ediff'.
FILE2 defaults to this file as well.  If you enter just a directory
name for FILE2, then this file is compared with a file of the same
name in that directory.  FILE2 is the second file given to `ediff';
this file is the first given to it.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-diff "dired+" "\
Compare this file (pointed by mouse) with file FILE2 using `diff'.
FILE2 defaults to the file at the mark.  This file is the first file
given to `diff'.  With prefix arg, prompt for second arg SWITCHES,
which are options for `diff'.

\(fn EVENT &optional SWITCHES)" t nil)

(autoload 'diredp-mouse-backup-diff "dired+" "\
Diff this file with its backup file or vice versa.
Use the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
With prefix arg, prompt for SWITCHES which are the options for `diff'.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-mark "dired+" "\
In Dired, mark this file.
If on a subdir headerline, mark all its files except `.' and `..'.

Use \\[dired-unmark-all-files] to remove all marks,
and \\[dired-unmark] on a subdir to remove the marks in this subdir.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-unmark "dired+" "\
In Dired, unmark this file.
If looking at a subdir, unmark all its files except `.' and `..'.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-mark/unmark "dired+" "\
Mark/unmark file or directory at mouse EVENT.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-mark-region-files "dired+" "\
Mark files between point and the mouse.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-mark/unmark-mark-region-files "dired+" "\
Mark/unmark file or mark files in region.
If the file the cursor is on is marked, then mark all files between it
 and the line clicked (included).
Otherwise (cursor's file is unmarked):
 If the file clicked is marked, then unmark it.
 If it is unmarked, then mark it.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-flag-file-deletion "dired+" "\
In Dired, flag this file for deletion.
If on a subdir headerline, mark all its files except `.' and `..'.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-copy "dired+" "\
In Dired, copy this file.
This normally preserves the last-modified date when copying.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-rename "dired+" "\
In Dired, rename this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-upcase "dired+" "\
In Dired, rename this file to upper case.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-downcase "dired+" "\
In Dired, rename this file to lower case.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-delete "dired+" "\
In Dired, delete this file, upon confirmation.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-shell-command "dired+" "\
Run a shell COMMAND on this file.
If there is output, it goes to a separate buffer.

No automatic redisplay of Dired buffers is attempted, as there's no
telling what files the command may have changed.  Type
\\[dired-do-redisplay] to redisplay.

The shell command has the top level directory as working directory, so
output files usually are created there instead of in a subdir.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-symlink "dired+" "\
Make symbolic link to this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-hardlink "dired+" "\
Make hard link (alias) to this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-print "dired+" "\
Print this file.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-grep "dired+" "\
Run grep against this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-compress "dired+" "\
Compress or uncompress this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-byte-compile "dired+" "\
Byte compile this file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-load "dired+" "\
Load this Emacs Lisp file.

\(fn EVENT)" t nil)

(autoload 'diredp-mouse-do-chmod "dired+" "\
Change the mode of this file.
This calls chmod, so symbolic modes like `g+w' are allowed.

\(fn EVENT)" t nil)

(autoload 'diredp-describe-mode "dired+" "\
Describe Dired mode, including Dired+ features.
This is `describe-mode' plus a description of Dired+ features.
For just the latter, use \\<dired-mode-map>`\\[diredp-dired-plus-help]'.

\(fn &optional BUFFER)" t nil)

(autoload 'diredp-dired-plus-help "dired+" "\
Describe Dired+.

\(fn)" t nil)

(autoload 'diredp-send-bug-report "dired+" "\
Send a bug report about a Dired+ problem.

\(fn)" t nil)

(autoload 'diredp-visit-next-file "dired+" "\
Move down a line and visit its file in another window.
With numeric prefix arg N, move down N-1 lines first.

After moving N lines, skip any lines with file names that match either
`diredp-visit-ignore-extensions' or `diredp-visit-ignore-regexps'. 

Kill the last buffer visited by a `dired-visit-*' command.

\(fn &optional ARG)" t nil)

(autoload 'diredp-visit-previous-file "dired+" "\
Move up a line and visit its file in another window.
With numeric prefix arg N, move up N-1 lines first.

After moving N lines, skip any lines with file names that match either
`diredp-visit-ignore-extensions' or `diredp-visit-ignore-regexps'. 

Kill the last buffer visited by a `dired-visit-*' command.

\(fn &optional ARG)" t nil)

(autoload 'diredp-visit-this-file "dired+" "\
View the file on this line in another window in the same frame.
If it was not already shown there then kill the previous buffer
visited by a `dired-visit-*' command.

If it was already shown there, and if it and Dired are the only
windows there, then delete its window (toggle : show/hide the file).

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired+" '("dired")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired+-autoloads.el ends here
