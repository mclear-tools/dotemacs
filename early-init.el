;; early-init.el  -*- lexical-binding: t; no-byte-compile: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-
;; Only for use with emacs 27 or higher
;; see https://www.reddit.com/r/emacs/comments/7yns85/emacs_adds_support_for_a_second_read_earlier_init/
;; and https://lists.gnu.org/archive/html/emacs-devel/2017-10/msg00372.html
;; for more information

;;; Early Startup
;;;; Native Comp

;; see https://github.com/jimeh/build-emacs-for-macos#native-comp
;; https://akrl.sdf.org/gccemacs.html#org335c0de
;; https://github.com/emacscollective/no-littering/wiki/Setting-gccemacs'-eln-cache

;; See if native-comp is available
(when (not (and (fboundp 'native-comp-available-p)
                (native-comp-available-p)))
  (message "Native complation is *not* available"))

;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name ".local/temp/cache/eln-cache/" user-emacs-directory)))

;; Silence nativecomp warnings popping up
(setq native-comp-async-report-warnings-errors nil)

;; Settings
(setq native-comp-speed 2
      native-comp-deferred-compilation t)

;;;; Speed up startup
;; Help speed up emacs initialization
;; See https://blog.d46.us/advanced-emacs-startup/
;; and http://tvraman.github.io/emacspeak/blog/emacs-start-speed-up.html
;; and https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/

(defvar cpm--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)


;;;; Garbage collection

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Adjust garbage collection thresholds during startup, and thereafter
;; see http://akrl.sdf.org
;; https://gitlab.com/koral/gcmh

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; When idle for 15sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (let ((inhibit-message t))
                           (message "Garbage Collector has run for %.06fsec"
                                    (k-time (garbage-collect)))))))

;;;; Set C Directory
(setq find-function-C-source-directory "/Applications/Emacs.app/Contents/Resources/src")

;;;; Prefer Newer files
;; prefer newer versions
(setq load-prefer-newer t)

;;;; Byte Compile Warnings
;; Disable certain byte compiler warnings to cut down on the noise. This is a
;; personal choice and can be removed if you would like to see any and all byte
;; compiler warnings.
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local obsolete))
;;;; Check Errors
;; Produce backtraces when errors occur
(setq debug-on-error nil)

;;;; When-let errors
;; https://github.com/alphapapa/frame-purpose.el/issues/3
;; https://github.com/alphapapa/frame-purpose.el/issues/3
(eval-and-compile
  (when (version< emacs-version "26")
    (with-no-warnings
      (defalias 'when-let* #'when-let)
      (function-put #'when-let* 'lisp-indent-function 1)
      (defalias 'if-let* #'if-let)
      (function-put #'if-let* 'lisp-indent-function 2))))

;;;; Variable Binding Depth
;; https://www.reddit.com/r/emacs/comments/9jp9zt/anyone_know_what_variable_binding_depth_exceeds/
(setq max-specpdl-size 13000)

;;; Package settings
(setq package-enable-at-startup nil) ;; use straight not package.el
(advice-add #'package--ensure-init-file :override #'ignore)

;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;;; Clean View
;; UI - Disable visual cruft

;; Resizing the Emacs frame can be an expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq-default frame-title-format nil)
(setq-default frame-inhibit-implied-resize t)
(setq-default inhibit-startup-screen t)
;; Disable start-up screen
(setq-default inhibit-splash-screen t)
(setq-default inhibit-startup-message t)
(setq-default initial-scratch-message nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
;; Disable tool, menu, and scrollbars. these are just clutter (the scrollbar
;; also impacts performance). I am intentionally not calling `menu-bar-mode',
;; `tool-bar-mode', and `scroll-bar-mode' (see doom for inspiration) because
;; they do extra and unnecessary work that can be more concisely and efficiently
;; expressed with these six lines:
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Fundamental mode at startup
(setq initial-major-mode 'fundamental-mode)

;; ;; echo buffer
;; ;; Don't display any message
;; ;; https://emacs.stackexchange.com/a/437/11934
(defun display-startup-echo-area-message ()
  (message ""))

;; And bury the scratch buffer, don't kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))


