;; early-init.el  -*- lexical-binding: t; no-byte-compile: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-
;; Only for use with emacs 27 or higher
;; see https://www.reddit.com/r/emacs/comments/7yns85/emacs_adds_support_for_a_second_read_earlier_init/
;; and https://lists.gnu.org/archive/html/emacs-devel/2017-10/msg00372.html
;; for more information

;;; Early Startup
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
;; NOTE: The system linked above generates too many GC pauses so I'm using my own mixed setup
;; https://github.com/purcell/emacs.d/blob/3b1302f2ce3ef2f69641176358a38fd88e89e664/init.el#L24

;; (let ((normal-gc-cons-threshold (* 20 1024 1024))
;;       (init-gc-cons-threshold (* 128 1024 1024)))
;;   (setq gc-cons-threshold init-gc-cons-threshold)
;;   (add-hook 'emacs-startup-hook
;;             (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

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

;;;; Native Comp
;; see https://github.com/jimeh/build-emacs-for-macos#native-comp
;; https://akrl.sdf.org/gccemacs.html#org335c0de
(setq comp-speed 2
      comp-deferred-compilation t)
(when (boundp 'comp-eln-load-path)
  (setcar comp-eln-load-path
          (expand-file-name ".local/temp/cache/eln-cache/" user-emacs-directory)))
;; Silence nativecomp warnings popping up on 28.0.50
(setq comp-async-report-warnings-errors nil)

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
(setq package-enable-at-startup nil) ;; use straight
(advice-add #'package--ensure-init-file :override #'ignore)

;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;;; Clean View
;; Disable start-up screen
;; Resizing the Emacs frame can be an expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.

(setq-default frame-title-format nil)
(setq-default frame-inhibit-implied-resize t)
(setq-default inhibit-startup-screen t)
(setq-default inhibit-splash-screen t)
(setq-default inhibit-startup-message t)
(setq-default initial-scratch-message nil)

;; UI - Disable visual cruft
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
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


