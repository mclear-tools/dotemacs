;;; posframe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "posframe" "posframe.el" (0 0 0 0))
;;; Generated autoloads from posframe.el

(autoload 'posframe-workable-p "posframe" "\
Test posframe workable status.

\(fn)" nil nil)

(autoload 'posframe-show "posframe" "\
Pop up a posframe and show STRING at POSITION.

POSITION can be:
1. An integer, meaning point position.
2. A cons of two integers, meaning absolute X and Y coordinates.
3. Other type, in which case the corresponding POSHANDLER should be
   provided.

POSHANDLER is a function of one argument returning an actual
position.  Its argument is a plist of the following form:

  (:position xxx
   :position-info xxx
   :poshandler xxx
   :font-height xxx
   :font-width xxx
   :posframe xxx
   :posframe-width xxx
   :posframe-height xxx
   :posframe-buffer xxx
   :parent-frame xxx
   :parent-window-left xxx
   :parent-window-top xxx
   :parent-frame-width xxx
   :parent-frame-height xxx
   :parent-window xxx
   :parent-window-width  xxx
   :parent-window-height xxx
   :minibuffer-height
   :mode-line-height
   :header-line-height
   :tab-line-height
   :x-pixel-offset xxx
   :y-pixel-offset xxx)

By default, poshandler is auto-selected based on the type of POSITION,
but the selection can be overridden using the POSHANDLER argument.
The builtin poshandler functions are listed below:

1.  `posframe-poshandler-frame-center'
2.  `posframe-poshandler-frame-top-center'
3.  `posframe-poshandler-frame-top-left-corner'
4.  `posframe-poshandler-frame-top-right-corner'
5.  `posframe-poshandler-frame-bottom-center'
6.  `posframe-poshandler-frame-bottom-left-corner'
7.  `posframe-poshandler-frame-bottom-right-corner'
8.  `posframe-poshandler-window-center'
9.  `posframe-poshandler-window-top-center'
10. `posframe-poshandler-window-top-left-corner'
11. `posframe-poshandler-window-top-right-corner'
12. `posframe-poshandler-window-bottom-center'
13. `posframe-poshandler-window-bottom-left-corner'
14. `posframe-poshandler-window-bottom-right-corner'
15. `posframe-poshandler-point-top-left-corner'
16. `posframe-poshandler-point-bottom-left-corner'

This posframe's buffer is BUFFER-OR-NAME, which can be a buffer
or a name of a (possibly nonexistent) buffer.

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before being shown in posframe.

WIDTH, MIN-WIDTH, HEIGHT and MIN-HEIGHT, specify bounds on the
new total size of posframe.  MIN-HEIGHT and MIN-WIDTH default to
the values of ‘window-min-height’ and ‘window-min-width’
respectively.  These arguments are specified in the canonical
character width and height of posframe.

If LEFT-FRINGE or RIGHT-FRINGE is a number, left fringe or
right fringe with be shown with the specified width.

By default, posframe shows no borders, but users can specify
borders by setting INTERNAL-BORDER-WIDTH to a positive number.
Border color can be specified by INTERNAL-BORDER-COLOR
or via the ‘internal-border’ face.

Posframe's font as well as foreground and background colors are
derived from the current frame by default, but can be overridden
using the FONT, FOREGROUND-COLOR and BACKGROUND-COLOR arguments,
respectively.

By default, posframe will display no header-line, mode-line and
tab-line.  In case a header-line, mode-line or tab-line is
desired, users can set RESPECT-HEADER-LINE, RESPECT-MODE-LINE or
RESPECT-TAB-LINE to t.

INITIALIZE is a function with no argument.  It will run when
posframe buffer is first selected with `with-current-buffer'
in `posframe-show', and only run once (for performance reasons).
If INITIALIZE is nil, `posframe-default-initialize-function' will
be used as fallback; this variable can be used to set posframe
buffer gobally.

OVERRIDE-PARAMETERS is very powful, *all* the frame parameters
used by posframe's frame can be overridden by it.

TIMEOUT can specify the number of seconds after which the posframe
will auto-hide.

If REFRESH is a number, posframe's frame-size will be re-adjusted
every REFRESH seconds.

You can use `posframe-delete-all' to delete all posframes.

\(fn BUFFER-OR-NAME &key STRING POSITION POSHANDLER WIDTH HEIGHT MIN-WIDTH MIN-HEIGHT X-PIXEL-OFFSET Y-PIXEL-OFFSET LEFT-FRINGE RIGHT-FRINGE INTERNAL-BORDER-WIDTH INTERNAL-BORDER-COLOR FONT FOREGROUND-COLOR BACKGROUND-COLOR RESPECT-HEADER-LINE RESPECT-MODE-LINE RESPECT-TAB-LINE INITIALIZE NO-PROPERTIES KEEP-RATIO OVERRIDE-PARAMETERS TIMEOUT REFRESH &allow-other-keys)" nil nil)

(autoload 'posframe-hide-all "posframe" "\
Hide all posframe frames.

\(fn)" t nil)

(autoload 'posframe-delete-all "posframe" "\
Delete all posframe frames and buffers.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "posframe" '("posframe-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; posframe-autoloads.el ends here
