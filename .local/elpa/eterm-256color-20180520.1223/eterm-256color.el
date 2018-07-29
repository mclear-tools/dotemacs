;;; eterm-256color.el --- Customizable 256 colors for term. -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Diego A. Mundo
;; Author: Diego A. Mundo <diegoamundo@gmail.com>
;; URL: http://github.com/dieggsy/eterm-256color
;; Git-Repository: git://github.com/dieggsy/eterm-256color
;; Created: 2017-11-01
;; Version: 0.3.14
;; Keywords: faces
;; Package-Requires: ((emacs "24.4") (xterm-color "1.7") (f "0.19.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Adds 256 color handling to term/ansi-term by adding 247 customizable faces
;; to ansi-term-color-vector and overriding term-handle-colors-array to handle
;; additional escape sequences.

;;; Code:
(require 'cl-lib)
(require 'xterm-color)
(require 'term)
(require 'f)
(require 'url-handlers)
(eval-when-compile (require 'subr-x))

(defgroup eterm-256color nil
  "256colors for term."
  :group 'term)

(defcustom eterm-256color-disable-bold t
  "Disable bold colors in eterm-256color.

Bold colors will be rendered as bright instead."
  :group 'eterm-256color
  :type 'boolean)

(defgroup eterm-256color-faces nil
  "Faces for eterm-256color"
  :group 'eterm-256color)

(defface eterm-256color-default
  '((t :inherit default))
  "Default face to use in term mode."
  :group 'eterm-256color-faces)

(defvar eterm-256color-base16-names
  '("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"))

;; Generate first 8
(cl-loop for name in eterm-256color-base16-names
         as i = 0 then (1+ i)
         as color = (xterm-color-256 i)
         do (custom-declare-face (intern (concat "eterm-256color-" name))
                                 `((t :foreground ,color :background ,color))
                                 (format "Face used to render %s color code." name)
                                 :group 'eterm-256color-faces))

;; Generate bright colors
(cl-loop for name in eterm-256color-base16-names
         as i = 8 then (1+ i)
         as color = (xterm-color-256 i)
         do (custom-declare-face
             (intern (concat "eterm-256color-bright-" name))
             `((t :foreground ,color :background ,color))
             (format "Face used to render bright or bold %s color code." name)
             :group 'eterm-256color-faces))

(put 'eterm-256color-0 'face-alias 'eterm-256color-black)
(put 'eterm-256color-1 'face-alias 'eterm-256color-red)
(put 'eterm-256color-2 'face-alias 'eterm-256color-green)
(put 'eterm-256color-3 'face-alias 'eterm-256color-yellow)
(put 'eterm-256color-4 'face-alias 'eterm-256color-blue)
(put 'eterm-256color-5 'face-alias 'eterm-256color-magenta)
(put 'eterm-256color-6 'face-alias 'eterm-256color-cyan)
(put 'eterm-256color-7 'face-alias 'eterm-256color-white)
(put 'eterm-256color-8 'face-alias 'eterm-256color-bright-black)
(put 'eterm-256color-9 'face-alias 'eterm-256color-bright-red)
(put 'eterm-256color-10 'face-alias 'eterm-256color-bright-green)
(put 'eterm-256color-11 'face-alias 'eterm-256color-bright-yellow)
(put 'eterm-256color-12 'face-alias 'eterm-256color-bright-blue)
(put 'eterm-256color-13 'face-alias 'eterm-256color-bright-magenta)
(put 'eterm-256color-14 'face-alias 'eterm-256color-bright-cyan)
(put 'eterm-256color-15 'face-alias 'eterm-256color-bright-white)

(defun eterm-256color--define-face (number)
  "Define a face using COLOR for 256 color NUMBER."
  (let ((color (xterm-color-256 number)))
    (custom-declare-face (intern (concat "eterm-256color-" (number-to-string number)))
                         `((t :foreground ,color :background ,color))
                         (format "Color %s" number)
                         :group 'eterm-256color-faces)))

(dolist (j (number-sequence 16 255))
  (eterm-256color--define-face j))

(defvar eterm-256color-vector
  (vconcat
   [eterm-256color-default
    eterm-256color-black
    eterm-256color-red
    eterm-256color-green
    eterm-256color-yellow
    eterm-256color-blue
    eterm-256color-magenta
    eterm-256color-cyan
    eterm-256color-white
    eterm-256color-bright-black
    eterm-256color-bright-red
    eterm-256color-bright-green
    eterm-256color-bright-yellow
    eterm-256color-bright-blue
    eterm-256color-bright-magenta
    eterm-256color-bright-cyan
    eterm-256color-bright-white]
   (mapcar (lambda (j)
             (intern (concat "eterm-256color-" (number-to-string j))))
           (number-sequence 16 255))))

(defvar term-terminal-previous-parameter)
(defvar term-terminal-previous-parameter-2 -1)

(defun eterm-256color-handle-colors (parameter)
  "Handle color sequences specified by PARAMETER.

This function supports 256 color sequences and bright colors."
  (cond
   ;; 256
   ((and (= term-terminal-previous-parameter 5)
         (= term-terminal-previous-parameter-2 38)
         (>= parameter 0)
         (<= parameter 255))
    (setq term-ansi-current-color (+ parameter 1)))

   ((and (= term-terminal-previous-parameter 5)
         (= term-terminal-previous-parameter-2 48)
         (>= parameter 0)
         (<= parameter 255))
    (setq term-ansi-current-bg-color (+ parameter 1)))

   ;; Bold  (terminfo: bold)
   ((eq parameter 1)
    (setq term-ansi-current-bold t))

   ;; Underline
   ((eq parameter 4)
    (setq term-ansi-current-underline t))

   ;; Blink (unsupported by Emacs), will be translated to bold.
   ;; This may change in the future though.
   ((eq parameter 5)
    (setq term-ansi-current-bold t))

   ;; Reverse (terminfo: smso)
   ((eq parameter 7)
    (setq term-ansi-current-reverse t))

   ;; Invisible
   ((eq parameter 8)
    (setq term-ansi-current-invisible t))

   ;; Reset underline (terminfo: rmul)
   ((eq parameter 24)
    (setq term-ansi-current-underline nil))

   ;; Reset reverse (terminfo: rmso)
   ((eq parameter 27)
    (setq term-ansi-current-reverse nil))

   ;; ADDITION
   ((and (>= parameter 90) (<= parameter 97))
    (setq term-ansi-current-color (- parameter 81)))

   ;; Foreground
   ((and (>= parameter 30) (<= parameter 37))
    (setq term-ansi-current-color (- parameter 29)))

   ;; Reset foreground
   ((eq parameter 39)
    (setq term-ansi-current-color 0))

   ;; Background
   ((and (>= parameter 40) (<= parameter 47))
    (setq term-ansi-current-bg-color (- parameter 39)))


   ;; Reset background
   ((eq parameter 49)
    (setq term-ansi-current-bg-color 0))

   ;; 0 (Reset) or unknown (reset anyway)
   (t
    (term-ansi-reset)))

  ;; (message "Debug: U-%d R-%d B-%d I-%d D-%d F-%d B-%d"
  ;;          term-ansi-current-underline
  ;;          term-ansi-current-reverse
  ;;          term-ansi-current-bold
  ;;          term-ansi-current-invisible
  ;;          term-ansi-face-already-done
  ;;          term-ansi-current-color
  ;;          term-ansi-current-bg-color)

  (unless term-ansi-face-already-done
    (if term-ansi-current-invisible
        (let ((color
               (if term-ansi-current-reverse
                   (face-foreground
                    (elt eterm-256color-vector term-ansi-current-color)
                    nil 'default)
                 (face-background
                  (elt eterm-256color-vector term-ansi-current-bg-color)
                  nil 'default))))
          (setq term-current-face
                (list :background color
                      :foreground color))
          ) ;; No need to bother with anything else if it's invisible.
      (setq term-current-face
            (list :foreground
              (face-foreground
               (elt eterm-256color-vector term-ansi-current-color)
               nil 'default)
              :background
              (face-background
               (elt eterm-256color-vector term-ansi-current-bg-color)
               nil 'default)
              :inverse-video term-ansi-current-reverse))

      (when (and term-ansi-current-bold
                 (not eterm-256color-disable-bold))
        (setq term-current-face
              `(,term-current-face :inherit term-bold)))

      (when (and eterm-256color-disable-bold term-ansi-current-bold)
        (let ((pos (cl-position
                    (plist-get term-current-face :foreground)
                    (mapcar (lambda (face) (face-foreground face nil 'default))
                            (cl-subseq eterm-256color-vector 1 9))
                    :test #'string=)))
          (if pos
              (plist-put term-current-face
                         :foreground
                         (face-foreground
                          (elt eterm-256color-vector (+ pos 9)) nil 'default)))))

      (when term-ansi-current-underline
        (setq term-current-face
              `(,term-current-face :inherit term-underline)))))

  ;;	(message "Debug %S" term-current-face)
  ;; FIXME: shouldn't we set term-ansi-face-already-done to t here?  --Stef
  (setq term-ansi-face-already-done nil))

(defun eterm-256color-term-exists-p (term)
  "Check whether term type TERM exists, per the tic man page."
  (let ((checker (lambda (file)
                   (let (case-fold-search)
                     (string= term (f-filename file)))))
        (terminfo-dir (getenv "TERMINFO"))
        (terminfo-dirs (getenv "TERMINFO_DIRS")))
    (or (when (and terminfo-dir (file-exists-p terminfo-dir))
          (f-entries terminfo-dir checker 'recursive))
        (and (file-exists-p "~/.terminfo")
             (f-entries "~/.terminfo" checker 'recursive))
        (when terminfo-dirs
          (cl-loop for dir in (split-string terminfo-dirs ":")
                   as result = (and (file-exists-p dir)
                                    (f-entries dir checker 'recursive))
                   return result))
        (and (file-exists-p "/usr/share/terminfo")
             (f-entries "/usr/share/terminfo" checker 'recursive)))))

(defun eterm-256color-compile ()
  "If eterm-256color isn't a term type, tic eterm-256color.ti.

If eterm-color doesn't exist, prompt to fetch and compile it."
  (unless (eterm-256color-term-exists-p "eterm-256color")
    (if (eterm-256color-term-exists-p "eterm-color")
        (when (y-or-n-p "eterm-256color-mode requires compilation of eterm-256color.ti.
Term may need to be restarted. Compile now? ")
          (let ((package-path (or load-file-name buffer-file-name)))
            (when (or (not package-path)
                      (not (equal (file-name-nondirectory package-path)
                                  "eterm-256color.el")))
              (setq package-path (locate-library "eterm-256color.el")))
            (compilation-start
             (format "tic -s %s" (expand-file-name
                                  "eterm-256color.ti"
                                  (file-name-directory package-path))))))
      (when (y-or-n-p "It seems you don't have the required term type 'eterm-color'.
Fetch from github.com/emacs-mirror/emacs and compile? ")
        (url-copy-file "https://raw.githubusercontent.com/emacs-mirror/emacs/master/etc/e/eterm-color.ti"
                       (expand-file-name
                        "eterm-color.ti"
                        temporary-file-directory)
                       'ok-if-already-exists)
        (compilation-start
         (format "tic -s %s"
                 (expand-file-name
                  "eterm-color.ti"
                  temporary-file-directory)))))))

;;;###autoload
(define-minor-mode eterm-256color-mode
  "Minor mode that adds 256color support to term/ansi-term."
  :init-value nil
  :group 'eterm-256color
  (if eterm-256color-mode
      (progn
        (eterm-256color-compile)
        (setq-local term-term-name "eterm-256color")
        (setq-local term-termcap-format
                    "%s%s:li#%d:co#%d:cl=\\E[H\\E[J:cd=\\E[J:bs:am:xn:cm=\\E[%%i%%d;%%dH\
:nd=\\E[C:up=\\E[A:ce=\\E[K:ho=\\E[H:pt\
:al=\\E[L:dl=\\E[M:DL=\\E[%%dM:AL=\\E[%%dL:cs=\\E[%%i%%d;%%dr:sf=^J\
:dc=\\E[P:DC=\\E[%%dP:IC=\\E[%%d@:im=\\E[4h:ei=\\E[4l:mi:\
:so=\\E[7m:se=\\E[m:us=\\E[4m:ue=\\E[m:md=\\E[1m:mr=\\E[7m:me=\\E[m\
:UP=\\E[%%dA:DO=\\E[%%dB:LE=\\E[%%dD:RI=\\E[%%dC\
:kl=\\EOD:kd=\\EOB:kr=\\EOC:ku=\\EOA:kN=\\E[6~:kP=\\E[5~:@7=\\E[4~:kh=\\E[1~\
:mk=\\E[8m:cb=\\E[1K:op=\\E[39;49m:Co#256:pa#32767:AB=\\E[48;5;%%dm:AF=\\E[38;5;%%dm:cr=^M\
:bl=^G:do=^J:le=^H:ta=^I:se=\\E[27m:ue=\\E[24m\
:kb=^?:kD=^[[3~:sc=\\E7:rc=\\E8:r1=\\Ec:")
        (advice-add 'term-handle-colors-array :override #'eterm-256color-handle-colors))
    (kill-local-variable 'term-term-name)
    (kill-local-variable 'term-termcap-format)
    (advice-remove 'term-handle-colors-array #'eterm-256color-handle-colors)))

(provide 'eterm-256color)

;;; eterm-256color.el ends here
