;;; helm-posframe.el --- Using posframe to show helm window  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Feng Shu
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/helm-posframe
;; Package-Version: 20200103.644
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching, helm
;; Package-Requires: ((emacs "26.0")(posframe "0.1.0")(helm "0.1"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; * helm-posframe README                                :README:

;; ** What is helm-posframe
;; helm-posframe is a helm extension, which let helm use posframe
;; to show its candidate menu.

;; NOTE: helm-posframe requires Emacs 26

;; ** How to enable and disable helm-posframe
;;    #+BEGIN_EXAMPLE
;;    (helm-posframe-enable)
;;    (helm-posframe-disable)
;;    #+END_EXAMPLE

;; ** Tips

;; *** How to show fringe to helm-posframe
;; ;; #+BEGIN_EXAMPLE
;; (setq helm-posframe-parameters
;;       '((left-fringe . 10)
;;         (right-fringe . 10)))
;; ;; #+END_EXAMPLE

;; By the way, User can set *any* parameters of helm-posframe with
;; the help of `helm-posframe-parameters'.

;;; Code:
;; * helm-posframe's code
(require 'cl-lib)
(require 'posframe)
(require 'helm)

(defgroup helm-posframe nil
  "Using posframe to show helm menu"
  :group 'helm
  :prefix "helm-posframe")

(defcustom helm-posframe-poshandler
  #'posframe-poshandler-frame-bottom-left-corner
  "The poshandler of helm-posframe."
  :group 'helm-posframe
  :type 'function)

(defcustom helm-posframe-width nil
  "The width of helm-posframe."
  :group 'helm-posframe
  :type 'number)

(defcustom helm-posframe-height nil
  "The height of helm-posframe."
  :group 'helm-posframe
  :type 'number)

(defcustom helm-posframe-font nil
  "The font used by helm-posframe.
When nil, Using current frame's font as fallback."
  :group 'helm-posframe
  :type 'string)

(defcustom helm-posframe-parameters nil
  "The frame parameters used by helm-posframe."
  :group 'helm-posframe
  :type 'string)

(defvar helm-posframe-buffer nil
  "The posframe-buffer used by helm-posframe.")

;; Fix warn
(defvar emacs-basic-display)

(defun helm-posframe-display (buffer &optional _resume)
  "The display function which is used by `helm-display-function'.
Argument BUFFER."
  (setq helm-posframe-buffer buffer)
  (posframe-show
   buffer
   :position (point)
   :poshandler helm-posframe-poshandler
   :width (or helm-posframe-width (+ (window-width) 2))
   :height (or helm-posframe-height helm-display-buffer-height)
   :min-height 10
   :min-width 50
   :font helm-posframe-font
   :override-parameters helm-posframe-parameters
   :respect-header-line t))

(defun helm-posframe-cleanup (orig-func)
  "Advice function of `helm-cleanup'.

`helm-cleanup' will call `bury-buffer' function, which
will let emacs minimize and restore when helm close.

In this advice function, `burn-buffer' will be temp redefine as
`ignore', do nothing."
  (cl-letf (((symbol-function 'bury-buffer) #'ignore))
    (funcall orig-func)
    (when (posframe-workable-p)
      (posframe-hide helm-posframe-buffer))))

;;;###autoload
(defun helm-posframe-enable ()
  "Enable helm-posframe."
  (interactive)
  (require 'helm)
  (setq helm-display-function #'helm-posframe-display)
  (advice-add 'helm-cleanup :around #'helm-posframe-cleanup)
  (message "helm-posframe is enabled."))

(defun helm-posframe-disable ()
  "Disable helm-posframe"
  (interactive)
  (require 'helm)
  (setq helm-display-function #'helm-default-display-buffer)
  (advice-remove 'helm-cleanup  #'helm-posframe-cleanup)
  (message "helm-posframe is disabled."))

(provide 'helm-posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; helm-posframe.el ends here
