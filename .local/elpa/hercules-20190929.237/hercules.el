;;; hercules.el --- An auto-magical, which-key-based hydra banisher. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/hercules
;; Package-Version: 20190929.237

;; Version: 0.3
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4") (which-key "3.3.2"))

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;; An auto-magical, which-key-based hydra banisher.

;; With almost no set-up code, Hercules lets you call any group of
;; related command sequentially with no prefix keys, while showing a
;; handy popup to remember the bindings for those commands.  It can
;; create both of these (the grouped commands, and the popup) from any
;; keymap.

;;; Code:
(require 'which-key)

(defvar hercules--popup-showing-p nil
  "Whether or not hercules.el has been summoned.
Used in addition to `which-key-persistent-popup' in case other
packages start relying on it.")

(defvar hercules-show-prefix nil
  "One of `which-key-show-prefix'.
Used as value of `which-key-show-prefix' in hercules.el
pop-ups.")

(defun hercules--hide (&optional keymap flatten &rest _)
  "Dismiss hercules.el.
Pop KEYMAP from `overriding-terminal-local-map' when it is not
nil.  If FLATTEN is t, `hercules--show' was called with the same
argument.  Restore `which-key--update' after such a call."
  (setq hercules--popup-showing-p nil
        which-key-persistent-popup nil)
  (which-key--hide-popup)
  (when keymap
    (internal-pop-keymap (symbol-value keymap)
                         'overriding-terminal-local-map))
  (when flatten
    (advice-remove #'which-key--update #'ignore)))

(defun hercules--show (&optional keymap flatten transient &rest _)
  "Summon hercules.el showing KEYMAP.
Push KEYMAP onto `overriding-terminal-local-map' when TRANSIENT
is nil.  Otherwise use `set-transient-map'.  If FLATTEN is t,
show full keymap \(including sub-maps\), and prevent redrawing on
prefix-key press by overriding `which-key--update'."
  (setq hercules--popup-showing-p t
        which-key-persistent-popup t)
  (when keymap
    (let ((which-key-show-prefix hercules-show-prefix))
      (if flatten
          (progn
            (which-key--show-keymap
             (symbol-name keymap) (symbol-value keymap) nil t t)
            (advice-add #'which-key--update :override #'ignore))
        (which-key--show-keymap
         (symbol-name keymap) (symbol-value keymap) nil nil t)))
    (if transient
        (set-transient-map (symbol-value keymap)
                           t #'hercules--hide)
      (internal-push-keymap (symbol-value keymap)
                            'overriding-terminal-local-map))))

(defun hercules--toggle (&optional keymap flatten transient &rest _)
  "Toggle hercules.el showing KEYMAP.
Pass TRANSIENT and FLATTEN to `hercules--hide', and
`hercules--show'."
  (if hercules--popup-showing-p
      (hercules--hide keymap)
    (hercules--show keymap flatten transient)))

(defun hercules--enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun hercules--advise (funs hst &optional keymap flatten transient)
  "Either `hide', `show' or `toggle' hercules.el depending on HST.
Do so when calling FUNS showing KEYMAP.  Pass TRANSIENT to
`hercules--hide', `hercules--show', or `hercules--toggle'."
  (cl-loop
   for fun in (hercules--enlist funs) do
   (progn
     (unless (symbol-function fun)
       (fset fun (lambda () (interactive))))
     (advice-add fun :after
                 (pcase hst
                   ('toggle (apply-partially #'hercules--toggle keymap flatten transient))
                   ('show (apply-partially #'hercules--show keymap flatten transient))
                   ('hide (apply-partially #'hercules--hide keymap flatten)))))))

(defun hercules--graylist (keys funs keymap &optional whitelist)
  "Unbind KEYS and keys bound to FUNS from KEYMAP.
If WHITELIST is t, Unbind all keys not in KEYS or bound to FUNS
from KEYMAP."
  (let ((keymap-alist
         (cl-loop for (key . fun-name)
                  in (which-key--get-keymap-bindings
                      (symbol-value keymap))
                  as fun = (intern fun-name)
                  when
                  (or (member key (hercules--enlist keys))
                      (member fun (hercules--enlist funs)))
                  collect (cons key fun))))

    (if whitelist
        (progn
          (set keymap (make-sparse-keymap))
          (cl-loop for (key . fun) in keymap-alist do
                   (define-key (symbol-value keymap) (kbd key) fun)))
      (cl-loop for (key . fun) in keymap-alist do
               (define-key (symbol-value keymap) (kbd key) nil)))))

(defun hercules--graylist-after-load (keys funs keymap &optional
                                           package whitelist)
  "Call `hercules--graylist' after PACKAGE has been loaded.
Pass KEYS, FUNS, KEYMAP, and WHITELIST directly to it.  If
PACKAGE is nil, simply call `hercules--graylist'."
  (if package
      (with-eval-after-load package
        (hercules--graylist keys funs keymap whitelist))
    (hercules--graylist keys funs keymap whitelist)))

;;;###autoload
(cl-defun hercules-def
    (&key toggle-funs
          show-funs
          hide-funs
          keymap
          flatten
          transient
          blacklist-keys
          whitelist-keys
          blacklist-funs
          whitelist-funs
          package)
  "Summon hercules.el to banish your hydras.

TOGGLE-FUNS, SHOW-FUNS, and HIDE-FUNS define entry and exit
points for hercules.el to show KEYMAP. Both single functions and
lists work. As all other arguments to `hercules-def', these must
be quoted.

KEYMAP specifies the keymap for hercules.el to make a pop-up out
of.  If KEYMAP is nil, it is assumed that one of SHOW-FUNS or
TOGGLE-FUNS results in a `which-key--show-popup' call. This may
be useful for functions such as `which-key-show-top-level'. I use
it to remind myself of some obscure Evil commands from time to
time.

FLATTEN displays all maps and sub-maps without redrawing on
prefix-key presses. This allows for multi-key combinations in a
single hercules.el buffer.

BLACKLIST-KEYS and WHITELIST-KEYS specify
which (`kbd'-interpretable) keys should removed from/allowed to
remain on KEYMAP. Handy if you want to unbind things in bulk and
don't want to get your hands dirty with keymaps. Both single
characters and lists work. Blacklists take precedence over
whitelists.

BLACKLIST-FUNS and WHITELIST-FUNS are analogous to BLACKLIST-KEYS
and WHITELIST-KEYS except that they operate on function
symbols. These might be useful if a keymap specifies multiple
bindings for a commands and pruning it is more efficient this
way. Blacklists again take precedence over whitelists.

PACKAGE must be passed along with BLACKLIST-KEYS, WHITELIST-KEYS,
BLACKLIST-FUNS, or WHITELIST-FUNS if KEYMAP belongs to a lazy
loaded package. Its contents should be the package name as a
quoted symbol.

Setting TRANSIENT to t allows you to get away with not setting
HIDE-FUNS or TOGGLE-FUNS by dismissing hercules.el whenever you
press a key not on KEYMAP."
  ;; tweak keymaps
  (when keymap
    (when (or whitelist-keys whitelist-funs)
      (hercules--graylist-after-load
       whitelist-keys whitelist-funs
       keymap package t))
    (when (or blacklist-keys blacklist-funs)
      (hercules--graylist-after-load
       blacklist-keys blacklist-funs
       keymap package nil)))

  ;; define entry points
  (hercules--advise toggle-funs 'toggle keymap flatten transient)
  (hercules--advise show-funs 'show keymap flatten transient)
  (hercules--advise hide-funs 'hide keymap flatten))

(provide 'hercules)
;;; hercules.el ends here
