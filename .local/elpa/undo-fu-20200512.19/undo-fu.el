;;; undo-fu.el --- Undo helper with redo -*- lexical-binding: t -*-

;; Copyright (C) 2019  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://gitlab.com/ideasman42/emacs-undo-fu
;; Package-Version: 20200512.19
;; Version: 0.3
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Undo/redo convenience wrappers to Emacs default undo commands.
;;
;; The redo operation can be accessed from a key binding
;; and stops redoing once the initial undo action is reached.
;;
;; If you want to cross the initial undo boundary to access
;; the full history, running [keyboard-quit] (typically C-g).
;; lets you continue redoing for functionality not typically
;; accessible with regular undo/redo.
;;
;; If you prefer [keyboard-quit] not interfere with undo behavior
;; You may optionally set `undo-fu-ignore-keyboard-quit' & explicitly
;; call `undo-fu-disable-checkpoint'.
;;

;;; Usage:

;; ;; Bind the keys
;; (global-unset-key (kbd "C-z"))
;; (global-set-key (kbd "C-z")   'undo-fu-only-undo)
;; (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)

;;; Code:

;; ---------------------------------------------------------------------------
;; Custom Variables

(defcustom undo-fu-allow-undo-in-region nil
  "When t, use `undo-in-region' when a selection is present.
Otherwise `undo-in-region' is never used, since it doesn't support `undo-only',
causing undo-fu to work with reduced functionality when a selection exists."
  :group 'undo-fu
  :type 'boolean)

(defcustom undo-fu-ignore-keyboard-quit nil
  "When t, don't use `keyboard-quit' to disable linear undo/redo behavior.

Instead, explicitly call `undo-fu-disable-checkpoint'."
  :group 'undo-fu
  :type 'boolean)

;; ---------------------------------------------------------------------------
;; Internal Variables

;; First undo step in the chain, don't redo past this.
(defvar-local undo-fu--checkpoint nil)
;; The length of 'undo-fu--checkpoint' (lazy initialize).
(defvar-local undo-fu--checkpoint-length nil)
;; Apply undo/redo constraints to stop redo from undoing or
;; passing the initial undo checkpoint.
(defvar-local undo-fu--respect t)
;; Initiated an undo-in region (don't use `undo-only').
;; Only use when `undo-fu-allow-undo-in-region' is true.
(defvar-local undo-fu--in-region nil)
;; Track the last undo/redo direction.
;; Use in conjunction with `undo-fu--was-undo-or-redo' to ensure the value isn't stale.
(defvar-local undo-fu--was-redo nil)

;; ---------------------------------------------------------------------------
;; Internal Functions/Macros

(defun undo-fu--checkpoint-set ()
  "Set the checkpoint."
  (setq undo-fu--checkpoint (cdr buffer-undo-list))
  (setq undo-fu--checkpoint-length nil))

(defun undo-fu--checkpoint-unset ()
  "Unset the checkpoint."
  (setq undo-fu--checkpoint nil)
  (setq undo-fu--checkpoint-length nil))

(defun undo-fu--checkpoint-disable ()
  "Disable using the checkpoint, allowing the initial boundary to be crossed when redoing."
  (setq undo-fu--respect nil)
  (setq undo-fu--in-region nil)
  (undo-fu--checkpoint-unset))

(defmacro undo-fu--with-advice (fn-orig where fn-advice &rest body)
  "Execute BODY with advice temporarily enabled."
  `
  (let ((fn-advice-var ,fn-advice))
    (unwind-protect
      (progn
        (advice-add ,fn-orig ,where fn-advice-var)
        ,@body)
      (advice-remove ,fn-orig fn-advice-var))))

(defmacro undo-fu--with-message-suffix (suffix &rest body)
  "Add text after the message output.
Argument SUFFIX is the text to add at the start of the message.
Optional argument BODY runs with the message suffix."
  (declare (indent 1))
  `
  (undo-fu--with-advice 'message
    :around
    (lambda (fn-orig arg &rest args)
      (apply fn-orig (append (list (concat arg "%s")) args (list ,suffix))))
    ,@body))

(defun undo-fu--undo-enabled-or-error ()
  "Raise a user error when undo is disabled."
  (when (eq t buffer-undo-list)
    (user-error "Undo has been disabled!")))

(defun undo-fu--next-step (list)
  "Get the next undo step in the list.

Argument LIST compatible list `buffer-undo-list'."
  (while (car list)
    (setq list (cdr list)))
  (while (and list (null (car list)))
    (setq list (cdr list)))
  list)

(defun undo-fu--count-step-to-other (list list-to-find count-limit)
  "Count the number of steps to an item in the undo list.

Argument LIST compatible list `buffer-undo-list'.
Argument LIST-TO-FIND the list to search for.
Argument COUNT-LIMIT don't count past this value.

Returns the number of steps to reach this list or COUNT-LIMIT."
  (let ((count 0))
    (while (and list (not (eq list list-to-find)) (< count count-limit))
      (setq list (undo-fu--next-step list))
      (setq count (1+ count)))
    count))

(defun undo-fu--count-redo-available (list-to-find count-limit was-undo)
  "Count the number of redo steps until a previously stored step.

Argument LIST-TO-FIND count the steps up until this undo step.
Argument COUNT-LIMIT don't count past this value.
Argument WAS-UNDO when non-nil,
prevents the `pending-undo-list' from being used.

Returns the number of steps to reach this list or COUNT-LIMIT."
  (undo-fu--count-step-to-other
    (if (or (eq pending-undo-list t) was-undo)
      (undo-fu--next-step buffer-undo-list)
      pending-undo-list)
    list-to-find count-limit))

(defun undo-fu--was-undo-or-redo ()
  "Return t when the last destructive action was undo or redo."
  (let ((undo-list buffer-undo-list))
    (while (and (consp undo-list) (eq (car undo-list) nil))
      (setq undo-list (cdr undo-list)))
    (not (null (gethash undo-list undo-equiv-table)))))

;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun undo-fu-disable-checkpoint ()
  "Remove the undo-fu checkpoint, making all future actions unconstrained.

This command is needed when `undo-fu-ignore-keyboard-quit' is t,
since in this case `keyboard-quit' cannot be used
to perform unconstrained undo/redo actions."
  (interactive)
  ;; Display an appropriate message.
  (if (undo-fu--was-undo-or-redo)
    (if (and undo-fu--respect undo-fu--checkpoint)
      (message "Undo checkpoint cleared!")
      (message "Undo checkpoint already cleared!"))
    (message "Undo checkpoint disabled for next undo action!"))

  (undo-fu--checkpoint-disable))

;;;###autoload
(defun undo-fu-only-redo-all ()
  "Redo all actions until the initial undo step.

wraps the `undo' function."
  (interactive "*")
  (unless undo-fu--checkpoint
    (user-error "Redo checkpoint not found!"))

  (undo-fu--with-message-suffix " All" (undo-fu-only-redo most-positive-fixnum)))

;;;###autoload
(defun undo-fu-only-redo (&optional arg)
  "Redo an action until the initial undo action.

wraps the `undo' function.

Optional argument ARG The number of steps to redo."
  (interactive "*p")
  ;; Raise error since we can't do anything useful in this case.
  (undo-fu--undo-enabled-or-error)

  (let*
    ( ;; Assign for convenience.
      (was-undo-or-redo (undo-fu--was-undo-or-redo))
      (was-redo (and was-undo-or-redo undo-fu--was-redo))
      (was-undo (and was-undo-or-redo (null was-redo)))
      (undo-fu-quit-command
        (if undo-fu-ignore-keyboard-quit
          'undo-fu-disable-checkpoint
          'keyboard-quit)))

    ;; Reset the option to not respect the checkpoint
    ;; after running non-undo related commands.
    (unless undo-fu--respect
      (unless was-undo-or-redo
        (when undo-fu-allow-undo-in-region
          (setq undo-fu--in-region nil))
        (setq undo-fu--respect t)))

    (when (region-active-p)
      (if undo-fu-allow-undo-in-region
        (progn
          (message "Undo in region in use. Undo checkpoint ignored!")
          (undo-fu--checkpoint-disable)
          (setq undo-fu--in-region t))
        ;; Default behavior, just remove selection.
        (deactivate-mark)))

    ;; Allow crossing the boundary, if we press [keyboard-quit].
    ;; This allows explicitly over-stepping the boundary,
    ;; in cases when users want to bypass this constraint.
    (when undo-fu--respect
      (when (member last-command (list undo-fu-quit-command 'undo-fu-disable-checkpoint))
        (undo-fu--checkpoint-disable)
        (message "Redo checkpoint stepped over!")))

    (when undo-fu--respect
      ;; Implement "linear" redo.
      ;; So undo/redo chains before the undo checkpoint never redo an undo step.
      ;;
      ;; Without this, redo is still usable, it's just that after undo,redo,undo, ...
      ;; the redo action will undo, which isn't so useful.
      ;; This makes redo-only the reverse of undo-only.

      (when
        (and
          ;; Not the first redo action.
          (not (eq t pending-undo-list))
          ;; Sanity check, since it's not just being used as a marker
          ;; any (unlikely) issues here would error.
          (not (null undo-fu--checkpoint)))

        ;; Lazy initialize checkpoint length, avoids calculating for every redo.
        (unless undo-fu--checkpoint-length
          (setq undo-fu--checkpoint-length (length undo-fu--checkpoint)))

        ;; Skip to the last matching redo step before the checkpoint.
        (let ((list pending-undo-list))
          (while
            (and
              (setq list (gethash list undo-equiv-table))
              (eq (last list undo-fu--checkpoint-length) undo-fu--checkpoint))
            (setq pending-undo-list list)))))

    (when undo-fu--respect
      (when (or (null was-undo-or-redo) (null undo-fu--checkpoint))
        (user-error
          "Redo without undo step (%s to ignore)"
          (substitute-command-keys (format "\\[%s]" (symbol-name undo-fu-quit-command))))))

    (let*
      (
        ;; It's important to clamp the number of steps before assigning
        ;; 'last-command' since it's used when checking the available steps.
        (steps
          (if (numberp arg)
            (if undo-fu--respect
              (let ((steps-test (undo-fu--count-redo-available undo-fu--checkpoint arg was-undo)))

                ;; Ensure the next steps is a redo action.
                (when (zerop steps-test)
                  (user-error
                    "Redo checkpoint reached (%s to ignore)"
                    (substitute-command-keys (format "\\[%s]" (symbol-name undo-fu-quit-command)))))

                steps-test)

              arg)
            1))
        (last-command
          (cond
            (was-undo
              ;; Break undo chain, avoid having to press [keyboard-quit].
              'ignore)
            (was-redo
              ;; Checked by the undo function.
              'undo)
            ((string-equal last-command 'keyboard-quit)
              ;; This case needs to be explicitly detected.
              ;; If we undo until there is no undo information left,
              ;; then press `keyboard-quit' and redo, it fails without this case.
              'ignore)
            (t
              ;; No change.
              last-command)))
        (success
          (condition-case err
            (progn
              (undo-fu--with-message-suffix
                (if undo-fu--respect
                  ""
                  " (unconstrained)")
                (undo steps))
              t)
            (error
              (progn
                (message "%s" (error-message-string err))
                nil)))))

      (when success
        (setq undo-fu--was-redo t))

      (setq this-command 'undo-fu-only-redo)
      success)))

;;;###autoload
(defun undo-fu-only-undo (&optional arg)
  "Undo the last action.

wraps the `undo-only' function.

Optional argument ARG the number of steps to undo."
  (interactive "*p")
  ;; Raise error since we can't do anything useful in this case.
  (undo-fu--undo-enabled-or-error)

  (let*
    ( ;; Assign for convenience.
      (was-undo-or-redo (undo-fu--was-undo-or-redo))
      (undo-fu-quit-command
        (if undo-fu-ignore-keyboard-quit
          'undo-fu-disable-checkpoint
          'keyboard-quit)))

    ;; Special case, for first execution, `was-undo-or-redo' may be true
    ;; based on saved undo history, yet the `undo-fu--checkpoint' can be nil.
    ;; In this case it's simplest to behave as if the last command was not undo.
    (when (and was-undo-or-redo undo-fu--respect (null undo-fu--checkpoint))
      (setq was-undo-or-redo nil))

    ;; Reset the option to not respect the checkpoint
    ;; after running non-undo related commands.
    (unless undo-fu--respect
      (unless was-undo-or-redo
        (when undo-fu-allow-undo-in-region
          (setq undo-fu--in-region nil))
        (setq undo-fu--respect t)))

    (when (not was-undo-or-redo)
      (undo-fu--checkpoint-set))

    (when (region-active-p)
      (if undo-fu-allow-undo-in-region
        (progn
          (message "Undo in region in use. Undo checkpoint ignored!")
          (undo-fu--checkpoint-disable)
          (setq undo-fu--in-region t))
        ;; Default behavior, just remove selection.
        (deactivate-mark)))

    ;; Allow crossing the boundary, if we press [keyboard-quit].
    ;; This allows explicitly over-stepping the boundary,
    ;; in cases when users want to bypass this constraint.
    (when undo-fu--respect
      (when (member last-command (list undo-fu-quit-command 'undo-fu-disable-checkpoint))
        (undo-fu--checkpoint-disable)
        (message "Undo checkpoint ignored!")))

    (let*
      ;; Swap in 'undo' for our own function name.
      ;; Without this undo won't stop once the first undo step is reached.
      (
        (steps (or arg 1))
        (last-command
          (cond
            (was-undo-or-redo
              ;; Checked by the undo function.
              'undo)
            (t
              ;; No change.
              last-command)))
        (success
          (condition-case err
            (progn
              (undo-fu--with-message-suffix
                (if undo-fu--respect
                  ""
                  " (unconstrained)")
                (if (or (not undo-fu--respect) undo-fu--in-region)
                  (undo steps)
                  (undo-only steps)))
              t)
            (error
              (progn
                (message "%s" (error-message-string err))
                nil)))))

      (when success
        (setq undo-fu--was-redo nil))

      (setq this-command 'undo-fu-only-undo)
      success)))

;; Evil Mode (setup if in use)
;;
;; Don't let these commands repeat.
;;
;; Notes:
;; - Use `with-eval-after-load' once Emacs version 24.4 is the minimum supported version.
;; - Package lint complains about using this command,
;;   however it's needed to avoid issues with `evil-mode'.
(declare-function evil-declare-not-repeat "ext:evil-common")
(eval-after-load
  'evil
  '
  (progn
    (mapc
      #'evil-declare-not-repeat
      (list
        'undo-fu-disable-checkpoint
        'undo-fu-only-redo
        'undo-fu-only-redo-all
        'undo-fu-only-undo))))

(provide 'undo-fu)

;;; undo-fu.el ends here
