;;; lem-setup-splash.el --- Splash screen for Lambda-Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Colin McLear
;;
;; Author: Colin McLear
;; Package-Requires: ((emacs "24.3") (dash "2.19.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; 
;; This is a simple library that creates a splash buffer centered vertically and
;; horizontally.
;;
;;; Code:

(require 'dash)

;; Calculate dimensions of largest monitor
;; hack -- for the moment just specify some large value
(defvar lem-splash--max-rows 300
  "Maximum number of rows a window can have")

(defvar lem-splash--max-columns 500
  "Maximum number of columns a window can have")


(defvar lem-splash-banner (lem-get-string-from-file (concat lem-library-dir "lambda-splash.txt"))
  "Banner to display on startup.")

(defvar lem-splash--box-dimensions nil
  "Variable used to store dimensions (rows columns) of banner text.")

;; calculate width of input text
(defun lem-splash--banner-box-dimenions ()
  "Returns list (row col) giving dimensions of bounding box of
  lem-splash-banner."
  (let* ((strings (split-string lem-splash-banner "\n"))
	     (string-lengths (-map 'length strings))
	     (ncol (apply 'max string-lengths))
	     (nrow (length strings)))
    (setq lem-splash--box-dimensions (list nrow ncol))))

(defvar lem-splash--insertion-point nil
  "Variable used to store insertion point of upper left point of banner.")

(defun lem-splash--set-local-vars ()
  "Internal function used to set all the local variables for the mode."
  (display-line-numbers-mode 0)
  (if truncate-lines
      (toggle-truncate-lines 1))
  (visual-line-mode -1)
  (setq-local hl-line-mode -1)
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)

  (setq-local auto-hscroll-mode nil)
  (setq-local hscroll-margin 0)
  (setq left-fringe-width 0)
  (setq right-fringe-width 0)
  (set-display-table-slot standard-display-table 'truncation 32)
  (set-window-buffer (selected-window) (get-buffer "*splash*"))
  (setq cursor-type nil)
  (face-remap-add-relative 'region '(:inherit default))
  (if (fboundp 'evil-mode)
      (setq-local evil-normal-state-cursor nil)
    (setq-local evil-emacs-state-cursor nil)
    (setq-local cursor-type nil)))

;; (define-derived-mode lem-splash-mode
;;   fundamental-mode "𝛌-SPLASH"
;;   "Major mode for showing custom splash screen."
;;   ;; bit of setup to make display nice
;;   (lem-splash--set-local-vars))

;; make buffer of size equal to largest monitor store center of text coordinates 
(defun lem-splash--make-splash-buffer ()
  "Creates buffer of dimension `lem-splash--max-columns' and `lem-splash--max-rows' and places the
banner at the center. Also checks to see if buffer named *splash* already exists and if so overwrites it"
  (unless lem-splash--box-dimensions
    (lem-splash--banner-box-dimenions))
  (let* ((splash-buffer (get-buffer-create "*splash*"))
	     (height (/ lem-splash--max-rows 2))
	     (width (/ lem-splash--max-columns 2))
	     (box-top (/ (car lem-splash--box-dimensions) .25))
	     (box-left (/ (nth 1 lem-splash--box-dimensions) 2))
	     (padding-top (- height box-top))
	     (padding-left (- width box-left))
	     (top-pad-string (concat (make-string lem-splash--max-columns ?\s) "\n")))
    (switch-to-buffer splash-buffer)
    (read-only-mode -1)
    (lem-splash--set-local-vars)
    (if (string= major-mode "lem-splash-mode")
	    (erase-buffer)
      nil)
    (dotimes (_ padding-top) (insert top-pad-string))
    (let ((tmp-point (point))
	      (indent-tabs-mode nil))
      (insert lem-splash-banner)
      (mark-paragraph)
      (indent-region 1)
      (goto-char (point))
      (re-search-forward "[^\s\n]")
      (backward-char)
      (setq lem-splash--insertion-point (point))
      (deactivate-mark)
      (read-only-mode 1)
      (lem-splash-mode)
      (get-buffer "*splash*"))))


(defun lem-splash--set-window-start (window)
  "Set window start to center banner in `window'."
  ;; look at set-window-start function
  (let* ((height (window-body-height nil))
	 (width (window-total-width nil))
	 (box-top (/ (nth 0 lem-splash--box-dimensions) 2))
	 (box-left (/ (nth 1 lem-splash--box-dimensions) 2))
	 (calling-window (selected-window)))

    (select-window window)
    (lem-splash--set-local-vars)
    ;; now acctually set window start
    (goto-char lem-splash--insertion-point)
    (set-window-start (selected-window) (point) nil)
    (scroll-left (- (current-column) (window-hscroll)))
    (scroll-down (+ 1 (- (/ height 2) box-top)))
    (scroll-right (- (/ width 2) box-left))
    (select-window calling-window)))


(defun lem-splash-redraw ()
  (interactive)
  "Fix up buffer and recenter."
  (lem-splash--make-splash-buffer)
  (lem-splash--set-window-start (selected-window)))


(defun lem-splash-recenter ()
  (interactive)
  "Fix up buffer and recenter."
  (lem-splash--set-window-start (selected-window)))

(defun lem-splash-initial-buffer ()
  "Function designed to be called by initial buffer."
  (lem-splash-redraw)
  (get-buffer "*splash*"))

(defun lem-splash-window-size-change-function (arg)
  "Funtion to run on window size change."
  ;; get list of windows displaying "*splash*"
  (when (get-buffer "*splash*")
    ;; (if (string= major-mode "lem-splash-mode")
    ;; 	(lem-splash--set-window-start (selected-window)))
    (let ((w-to-update (get-buffer-window-list "*splash*" nil (selected-frame))))
      (-map 'lem-splash--set-window-start w-to-update))
    ))

;; bit of setup to make redisplay nice
(add-hook 'window-size-change-functions 'lem-splash-window-size-change-function)

(setq initial-buffer-choice 'lem-splash-initial-buffer)

;; (add-hook 'window-size-change-functions (lambda (arg) (message "size change detected")))
;; (add-hook 'window-startup-hook 'lem-splash-window-size-change-function)
;;; Define Minor Mode
;; Custom splash screen
(defvar lem-splash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'lem-open-agenda-in-workspace)
    (define-key map (kbd "c") 'lem-open-emacsd-in-workspace)
    (define-key map (kbd "m") 'lem-open-email-in-workspace)
    (define-key map (kbd "n") 'lem-open-notes-in-workspace)
    (define-key map (kbd "p") 'lem-open-existing-project-and-workspace)
    (define-key map (kbd "q") 'splash-screen-kill)
    (define-key map (kbd "esc") 'splash-screen-kill)
    map)
  "Keymap for lem-splash-mode.")

(define-minor-mode lem-splash-mode
  "Emacs minor mode for splash screen."
  :global nil
  :group 'lambda-emacs
  :require 'lem-setup-splash.el

  (buffer-disable-undo)
  (whitespace-mode -1)
  (linum-mode -1)
  (setq-local buffer-read-only t)
  (setq-local cursor-type -1)
  (setq-local hl-line-mode -1)
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)

  ;; No margin or fringe in splash buffer
  (setq-local left-margin-width nil
              right-margin-width nil)
  (set-window-fringes (selected-window) 0 0 nil)

  (when (>= emacs-major-version 26)
    (display-line-numbers-mode -1))
  (setq inhibit-startup-screen t
        truncate-lines nil
        inhibit-startup-message t
        inhibit-startup-echo-area-message t)
  (goto-char (point-min)))

;; Suppress any startup message in the echo area
(run-with-idle-timer 0.05 nil (lambda() (message nil)))

;; Install hook after frame parameters have been applied and only if
;; no option on the command line
(if (and (not (member "--no-splash" command-line-args))
         (not (member "--file"      command-line-args))
         (not (member "--insert"    command-line-args))
         (not (member "--find-file" command-line-args)))
    (progn
      (add-hook 'window-setup-hook 'lem-splash-screen)))

;; (provide 'lem-setup-splash)

;;; lem-splash.el ends here
