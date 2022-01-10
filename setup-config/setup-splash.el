;;; setup-splash.el --- An alternative splash screen -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Nicolas .P Rougier

;; Author: Nicolas P. Rougier <nicolas.rougier@inria.fr>
;; Mondifications: Colin McLear
;; URL: https://github.com/rougier/emacs-splash
;; Keywords: startup
;; Version: 0.1
;; Package-Requires:

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
;;
;;  An alternative splash screen:

;;  - "q" or <esc> kills the splash screen
;;  - Any other key open the about-emacs buffer
;;
;; Note: The screen is not shown if there are opened file buffers. For
;;       example, if you start emacs with a filename on the command
;;       line, the splash is not shown.
;;
;; Usage:
;;
;;  (require 'setup-splash)
;;
;;; Code:
(require 'cl-lib)

(defun point-calc-lines-offset (pt lines)
  (save-excursion
    (goto-char pt)
    (forward-line lines)
    (point)))

;; See https://github.com/emacs-dashboard/emacs-dashboard/blob/master/dashboard-widgets.el
(defcustom splash-init-info
  (lambda ()
    (let ((package-count 0) (time (emacs-init-time)))
      (when (bound-and-true-p package-alist)
        (setq package-count (length package-activated-list)))
      (when (boundp 'straight--profile-cache)
        (setq package-count (+ (hash-table-size straight--profile-cache) package-count)))
      (if (zerop package-count)
          (format "Emacs started in %s" time)
        (format "%d packages loaded in %s" package-count time))))
  "Init info with packages loaded and init time."
  :type '(function string)
  :group 'splash)

(defun splash-screen ()
  "A custom splash screen for Emacs"

  (interactive)

  ;; Hide modeline before window-body-height is computed
  (let* ((splash-buffer (get-buffer-create "*splash*")))
    (with-current-buffer splash-buffer
      (setq header-line-format nil)
      (setq mode-line-format nil)))


  (let* ((splash-buffer  (get-buffer-create "*splash*"))
         (height         (- (window-body-height nil) 1))
         (width         (window-body-width))
         (padding-center (- (/ height 2) 1))
         (padding-bottom (- height (/ height 2) 3))
         (image          (cpm/get-string-from-file "~/.emacs.d/lambda-splash.txt")))

    (with-current-buffer splash-buffer
      (erase-buffer)

      ;; Buffer local settings
      (if (one-window-p)
          (setq mode-line-format nil))
      (setq cursor-type nil)
      (setq vertical-scroll-bar nil)
      (setq horizontal-scroll-bar nil)
      (setq fill-column width)
      (face-remap-add-relative 'link :underline nil)
      (if (not (display-graphic-p)) (menu-bar-mode 0))
      ;; Set padding
      (setq-local left-margin-width 15 right-margin-width 0) ; Define new widths.
      (set-window-buffer nil (current-buffer))

      ;; Add padding at top
      (insert-char ?\n 5)

      ;; Insert image
      (insert (propertize image 'face 'shadow))

      ;; Insert text
      (goto-char width)
      (save-excursion
        (insert (concat
                 (propertize "Welcome to GNU Emacs"  'face 'bold)
                 " "
                 (propertize (format "%d.%d" emacs-major-version emacs-minor-version) 'face 'bold))))

      (goto-char (+ width 140))
      (save-excursion (insert (propertize "Bespoke elisp for your yak shaving pleasure" 'face 'warning)))

      (goto-char (+ width 310))
      (save-excursion (let ((init-info (funcall splash-init-info)))
                        (insert (propertize init-info 'face 'warning))))

      ;; Vertical padding to bottom
      (goto-char (point-max))

      ;; Footer text
      (center-line) (insert "\n")
      (center-line) (insert "\n")
      (save-excursion (insert (propertize
                               "                        Aus so krummem Holze, als woraus der Mensch gemacht ist, kann nichts ganz Gerades gezimmert werden" 'face 'shadow)))

      (goto-char 0)
      (read-only-mode t)

      (local-set-key [t]               'splash-screen-fade-to-default)
      (local-set-key (kbd "C-[")       'splash-screen-fade-to-default)
      (local-set-key (kbd "<escape>")  'splash-screen-fade-to-default)
      (local-set-key (kbd "q")         'splash-screen-fade-to-default)
      (local-set-key (kbd "<mouse-1>") 'mouse-set-point)
      (local-set-key (kbd "<mouse-2>") 'operate-this-button)
      (display-buffer-same-window splash-buffer nil)
      (run-with-idle-timer 10.0 nil    'splash-screen-fade-to-default))
    (switch-to-buffer "*splash*"))


  ;; Mac animation, only available from
  ;;  https://bitbucket.org/mituharu/emacs-mac/src/master/
  ;;  https://github.com/railwaycat/homebrew-emacsmacport
  (defvar mac-animation-locked-p nil))
(defun mac-animation-toggle-lock ()
  (setq mac-animation-locked-p (not mac-animation-locked-p)))
(defun mac-animation-fade-out (duration &rest args)
  (unless mac-animation-locked-p
    (mac-animation-toggle-lock)
    (mac-start-animation nil :type 'fade-out :duration duration)
    (run-with-timer duration nil 'mac-animation-toggle-lock)))

(defun splash-screen-fade-to (about duration)
  "Fade out current frame for duration and goes to command-or-bufffer"
  (interactive)
  (defalias 'mac-animation-fade-out-local
    (apply-partially 'mac-animation-fade-out duration))
  (if (get-buffer "*splash*")
      (progn (if (and (display-graphic-p) (fboundp 'mac-start-animation))
                 (advice-add 'set-window-buffer
                             :before 'mac-animation-fade-out-local))
             (if about (about-emacs))
             (kill-buffer "*splash*")
             (if (and (display-graphic-p) (fboundp 'mac-start-animation))
                 (advice-remove 'set-window-buffer
                                'mac-animation-fade-out-local)))))
(defun splash-screen-fade-to-about ()
  (interactive) (splash-screen-fade-to 1 1.0))
(defun splash-screen-fade-to-default ()
  (interactive) (splash-screen-fade-to nil 0.25))

(defun splash-screen-kill ()
  "Kill the splash screen buffer (immediately)."
  (interactive)
  (if (get-buffer "*splash*")
      (kill-buffer "*splash*")))

;; Suppress any startup message in the echo area
(run-with-idle-timer 0.05 nil (lambda() (message nil)))

;; Install hook after frame parameters have been applied and only if
;; no option on the command line
(if (and (not (member "-no-splash"  command-line-args))
         (not (member "--file"      command-line-args))
         (not (member "--insert"    command-line-args))
         (not (member "--find-file" command-line-args))
         ;; (not inhibit-startup-screen)
         )
    (progn
      (add-hook 'window-setup-hook 'splash-screen)
      (setq inhibit-startup-screen t
            inhibit-startup-message t
            inhibit-startup-echo-area-message t)))

(provide 'setup-splash)


;; (insert-text-button " mclear-tools/dotemacs  "
;;                     'action (lambda (_) (browse-url "https://github.com/mclear-tools/dotemacs"))
;;                     'help-echo "Visit dotemacs repo"
;;                     'face 'warning
;;                     'follow-link t)
;; (center-line)(insert "\n")
;; (insert-text-button " mclear-tools/build-emacs-macos  "
;;                     'action (lambda (_) (browse-url "https://github.com/mclear-tools/build-emacs-macos"))
;;                     'help-echo "Visit build-emacs-macos repo"
;;                     'face 'warning
;;                     'follow-link t)
;; (center-line)(insert "\n")
;; (insert-text-button " mclear-tools/bespoke-themes  "
;;                     'action (lambda (_) (browse-url "https://github.com/mclear-tools/bespoke-themes"))
;;                     'help-echo "Visit bespoke-themes repo"
;;                     'face 'warning
;;                     'follow-link t)
;; (center-line)(insert "\n")
;; (insert-text-button " mclear-tools/bespoke-modeline  "
;;                     'action (lambda (_) (browse-url "https://github.com/mclear-tools/bespoke-modeline"))
;;                     'help-echo "Visit bespoke-modeline repo"
;;                     'face 'warning
;;                     'follow-link t)
;; )
;; (center-line)

;; (goto-char 493)
;; (save-excursion (insert (propertize (format "Initialization time: %s" (emacs-init-time)) 'face 'shadow)))


;;; setup-splash.el ends here
