;;; vterm.el --- This package implements a terminal via libvterm -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019  by Lukas Fürmetz
;;
;; Author: Lukas Fürmetz <fuermetz@mailbox.org>
;; Version: 0.0.1
;; URL: https://github.com/akermu/emacs-libvterm
;; Keywords: terminals
;; Package-Requires: ((emacs "25.1"))


;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This Emacs module implements a bridge to libvterm to display a terminal in a
;; Emacs buffer.

;;; Installation

;; And add this to your `init.el`:

;; ```
;; (add-to-list 'load-path "path/to/emacs-libvterm")
;; (require 'vterm)
;; ```


;;; Code:

(unless module-file-suffix
  (error "VTerm needs module support. Please compile your Emacs with the --with-modules option!"))

(require 'term)

(defvar vterm-install-buffer-name " *Install vterm"
  "Name of the buffer used for compiling vterm-module.")

;;;###autoload
(defun vterm-module-compile ()
  "This function compiles the vterm-module."
  (interactive)
  (let ((default-directory (file-name-directory (file-truename (locate-library "vterm")))))
    (unless (file-executable-p (concat default-directory "vterm-module.so" ))
      (let* ((buffer (get-buffer-create vterm-install-buffer-name))
             status)
        (pop-to-buffer vterm-install-buffer-name)
        (setq status (call-process "sh" nil buffer t "-c"
                                   "mkdir -p build;                             \
                                    cd build;                                   \
                                    cmake -DCMAKE_BUILD_TYPE=RelWithDebInfo ..; \
                                    make") )
        (if (eq status 0)
            (message "Compilation of emacs-libvterm module succeeded")
          (error "Compilation of emacs-libvterm module failed!"))))))


(unless (require 'vterm-module nil t)
  (vterm-module-compile)
  (require 'vterm-module))

(require 'subr-x)
(require 'cl-lib)
(require 'color)

(defcustom vterm-shell shell-file-name
  "The shell that gets run in the vterm."
  :type 'string
  :group 'vterm)

(defcustom vterm-max-scrollback 1000
  "Maximum 'scrollback' value."
  :type 'number
  :group 'vterm)

(defcustom vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v" "C-y" "M-y")
  "Exceptions for vterm-keymap.

If you use a keybinding with a prefix-key, add that prefix-key to
this list. Note that after doing so that prefix-key cannot be sent
to the terminal anymore."
  :type '(repeat string)
  :group 'vterm)

(defcustom vterm-exit-functions nil
  "Shell exit hook.

This hook applies only to new vterms, created after setting this
value with `add-hook'.

Note that this hook will not work if another package like
`shell-pop' sets its own sentinel to the `vterm' process."
  :type 'hook
  :group 'vterm)

(defcustom vterm-set-title-functions nil
  "Shell set title hook.

those functions are called one by one, with 1 arguments.
`vterm-set-title-functions' should be a symbol, a hook variable.
The value of HOOK may be nil, a function, or a list of functions.
for example
    (defun vterm--rename-buffer-as-title (title)
    (rename-buffer (format \"vterm %s\" title)))
    (add-hook 'vterm-set-title-functions 'vterm--rename-buffer-as-title)

see http://tldp.org/HOWTO/Xterm-Title-4.html about how to set terminal title
for different shell"
  :type 'hook
  :group 'vterm)

(defface vterm-color-default
  `((t :inherit default))
  "The default normal color and bright color.
the foreground color are used for normal color,
and background color are used for bright color. "
  :group 'vterm)

(defface vterm-color-black
  `((t :inherit term-color-black))
  "Face used to render black color code.
the foreground color are used for normal color,
and background color are used for bright color. "
  :group 'vterm)

(defface vterm-color-red
  `((t :inherit term-color-red))
  "Face used to render red color code.
the foreground color are used for normal color,
and background color are used for bright color. "
  :group 'vterm)

(defface vterm-color-green
  `((t :inherit term-color-green))
  "Face used to render green color code.
the foreground color are used for normal color,
and background color are used for bright color. "
  :group 'vterm)

(defface vterm-color-yellow
  `((t :inherit term-color-yellow))
  "Face used to render yellow color code.
the foreground color are used for normal color,
and background color are used for bright color. "
  :group 'vterm)

(defface vterm-color-blue
  `((t :inherit term-color-blue))
  "Face used to render blue color code.
the foreground color are used for normal color,
and background color are used for bright color. "
  :group 'vterm)

(defface vterm-color-magenta
  `((t :inherit term-color-magenta))
  "Face used to render magenta color code.
the foreground color are used for normal color,
and background color are used for bright color. "
  :group 'vterm)

(defface vterm-color-cyan
  `((t :inherit term-color-cyan))
  "Face used to render cyan color code.
the foreground color are used for normal color,
and background color are used for bright color. "
  :group 'vterm)

(defface vterm-color-white
  `((t :inherit term-color-white))
  "Face used to render white color code.
the foreground color are used for normal color,
and background color are used for bright color. "
  :group 'vterm)

(defvar vterm-color-palette
  [vterm-color-black
   vterm-color-red
   vterm-color-green
   vterm-color-yellow
   vterm-color-blue
   vterm-color-magenta
   vterm-color-cyan
   vterm-color-white]
  "Color palette for the foreground and background.")

(defvar-local vterm--term nil
  "Pointer to Term.")

(defvar-local vterm--process nil
  "Shell process of current term.")

(defvar-local vterm--redraw-timer nil)

(defvar vterm-timer-delay 0.01
  "Delay for refreshing the buffer after receiving updates from libvterm.
Improves performance when receiving large bursts of data.
If nil, never delay")

(define-derived-mode vterm-mode fundamental-mode "VTerm"
  "Major mode for vterm buffer."
  (buffer-disable-undo)
  (setq vterm--term (vterm--new (window-body-height)
                                (window-body-width)
                                vterm-max-scrollback))

  (setq buffer-read-only t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)

  (if (version< emacs-version "27")
      (add-hook 'window-size-change-functions #'vterm--window-size-change-26 t t)
    (add-hook 'window-size-change-functions #'vterm--window-size-change t t))
  (let ((process-environment (append '("TERM=xterm-256color"
                                       "INSIDE_EMACS=vterm"
                                       "LINES"
                                       "COLUMNS")
                                     process-environment))
        (process-adaptive-read-buffering nil))
    (setq vterm--process
          (make-process
           :name "vterm"
           :buffer (current-buffer)
           :command `("/bin/sh" "-c"
                      ,(format "stty -nl sane iutf8 erase ^? rows %d columns %d >/dev/null && exec %s"
                               (window-body-height)
                               (window-body-width)
                               vterm-shell))
           :coding 'no-conversion
           :connection-type 'pty
           :filter #'vterm--filter
           :sentinel (when vterm-exit-functions #'vterm--sentinel))))
  (vterm--set-pty-name vterm--term (process-tty-name vterm--process)))


;; Function keys and most of C- and M- bindings
(mapc (lambda (key)
        (define-key vterm-mode-map (kbd key) #'vterm--self-insert))
      (append (cl-loop for number from 1 to 12
                       for key = (format "<f%i>" number)
                       unless (member key vterm-keymap-exceptions)
                       collect key)
              (cl-loop for prefix in '("C-" "M-")
                       append (cl-loop for char from ?a to ?z
                                       for key = (format "%s%c" prefix char)
                                       unless (member key vterm-keymap-exceptions)
                                       collect key))))

;; Keybindings
(define-key vterm-mode-map [tab]                       #'vterm-send-tab)
(define-key vterm-mode-map (kbd "TAB")                 #'vterm-send-tab)
(define-key vterm-mode-map [backtab]                   #'vterm--self-insert)
(define-key vterm-mode-map [backspace]                 #'vterm-send-backspace)
(define-key vterm-mode-map (kbd "DEL")                 #'vterm-send-backspace)
(define-key vterm-mode-map [M-backspace]               #'vterm-send-meta-backspace)
(define-key vterm-mode-map (kbd "M-DEL")               #'vterm-send-meta-backspace)
(define-key vterm-mode-map [return]                    #'vterm-send-return)
(define-key vterm-mode-map (kbd "RET")                 #'vterm-send-return)
(define-key vterm-mode-map [left]                      #'vterm-send-left)
(define-key vterm-mode-map [right]                     #'vterm-send-right)
(define-key vterm-mode-map [up]                        #'vterm-send-up)
(define-key vterm-mode-map [down]                      #'vterm-send-down)
(define-key vterm-mode-map [home]                      #'vterm--self-insert)
(define-key vterm-mode-map [end]                       #'vterm--self-insert)
(define-key vterm-mode-map [escape]                    #'vterm--self-insert)
(define-key vterm-mode-map [remap yank]                #'vterm-yank)
(define-key vterm-mode-map [remap yank-pop]            #'vterm-yank-pop)
(define-key vterm-mode-map (kbd "C-SPC")               #'vterm--self-insert)
(define-key vterm-mode-map (kbd "C-_")                 #'vterm--self-insert)
(define-key vterm-mode-map (kbd "C-/")                 #'vterm-undo)
(define-key vterm-mode-map (kbd "M-.")                 #'vterm-send-meta-dot)
(define-key vterm-mode-map (kbd "M-,")                 #'vterm-send-meta-comma)
(define-key vterm-mode-map (kbd "C-c C-y")             #'vterm--self-insert)
(define-key vterm-mode-map (kbd "C-c C-c")             #'vterm-send-ctrl-c)
(define-key vterm-mode-map [remap self-insert-command] #'vterm--self-insert)

(define-key vterm-mode-map (kbd "C-c C-t")             #'vterm-copy-mode)

(defvar vterm-copy-mode-map (make-sparse-keymap)
  "Minor mode map for `vterm-copy-mode'.")
(define-key vterm-copy-mode-map (kbd "C-c C-t")        #'vterm-copy-mode)

(defvar-local vterm--copy-saved-point nil)

(define-minor-mode vterm-copy-mode
  "Toggle vterm copy mode."
  :group 'vterm
  :lighter " VTermCopy"
  :keymap vterm-copy-mode-map
  (if vterm-copy-mode
      (progn                            ;enable vterm-copy-mode
        (use-local-map nil)
        (vterm-send-stop)
        (setq vterm--copy-saved-point (point)))
    (if vterm--copy-saved-point
        (goto-char vterm--copy-saved-point))
    (use-local-map vterm-mode-map)
    (vterm-send-start)))

(defun vterm--self-insert ()
  "Sends invoking key to libvterm."
  (interactive)
  (when vterm--term
    (let* ((modifiers (event-modifiers last-input-event))
           (shift (memq 'shift modifiers))
           (meta (memq 'meta modifiers))
           (ctrl (memq 'control modifiers)))
      (when-let ((key (key-description (vector (event-basic-type last-input-event)))))
        (vterm-send-key key shift meta ctrl)))))

(defun vterm-send-key (key &optional shift meta ctrl)
  "Sends KEY to libvterm with optional modifiers SHIFT, META and CTRL."
  (when vterm--term
    (let ((inhibit-redisplay t)
          (inhibit-read-only t))
      (when (and (not (symbolp last-input-event)) shift (not meta) (not ctrl))
        (setq key (upcase key)))
      (vterm--update vterm--term key shift meta ctrl))))

(defun vterm-send-start ()
  "Output from the system is started when the system receives START."
  (interactive)
  (vterm-send-key "<start>"))

(defun vterm-send-stop ()
  "Output from the system is stopped when the system receives STOP."
  (interactive)
  (vterm-send-key "<stop>"))

(defun vterm-send-return ()
  "Sends `<return>' to the libvterm."
  (interactive)
  (vterm-send-key "<return>"))

(defun vterm-send-tab ()
  "Sends `<tab>' to the libvterm."
  (interactive)
  (vterm-send-key "<tab>"))

(defun vterm-send-backspace ()
  "Sends `<backspace>' to the libvterm."
  (interactive)
  (vterm-send-key "<backspace>"))

(defun vterm-send-meta-backspace ()
  "Sends `M-<backspace>' to the libvterm."
  (interactive)
  (vterm-send-key "<backspace>" nil t))

(defun vterm-send-up ()
  "Sends `<up>' to the libvterm."
  (interactive)
  (vterm-send-key "<up>"))

(defun vterm-send-down ()
  "Sends `<down>' to the libvterm."
  (interactive)
  (vterm-send-key "<down>"))

(defun vterm-send-left()
  "Sends `<left>' to the libvterm."
  (interactive)
  (vterm-send-key "<left>"))

(defun vterm-send-right()
  "Sends `<right>' to the libvterm."
  (interactive)
  (vterm-send-key "<right>"))

(defun vterm-send-meta-dot()
  "Sends `M-.' to the libvterm."
  (interactive)
  (vterm-send-key "." nil t))

(defun vterm-send-meta-comma()
  "Sends `M-,' to the libvterm."
  (interactive)
  (vterm-send-key "," nil t))

(defun vterm-send-ctrl-c ()
  "Sends `C-c' to the libvterm."
  (interactive)
  (vterm-send-key "c" nil nil t))

(defun vterm-undo ()
  "Sends `C-_' to the libvterm."
  (interactive)
  (vterm-send-key "_" nil nil t))

(defun vterm-yank (&optional arg)
  "Implementation of `yank' (paste) in vterm."
  (interactive "P")
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function 'insert-for-yank)
               #'(lambda(str) (vterm-send-string str t))))
      (yank arg))))

(defun vterm-yank-pop(&optional arg)
  "Implementation of `yank-pop' in vterm."
  (interactive "p")
  (let ((inhibit-read-only t)
        (yank-undo-function #'(lambda(_start _end) (vterm-undo))))
    (cl-letf (((symbol-function 'insert-for-yank)
               #'(lambda(str) (vterm-send-string str t))))
      (yank-pop arg))))

(defun vterm-send-string (string &optional paste-p)
  "Send the string STRING to vterm.
Optional argument PASTE-P paste-p."
  (when vterm--term
    (when paste-p
      (vterm--update vterm--term "<start_paste>" nil nil nil))
    (dolist (char (string-to-list string))
      (vterm--update vterm--term (char-to-string char) nil nil nil))
    (when paste-p
      (vterm--update vterm--term "<end_paste>" nil nil nil))))

(defun vterm--invalidate()
  "The terminal buffer is invalidated, the buffer needs redrawing."
  (if vterm-timer-delay
      (unless vterm--redraw-timer
        (setq vterm--redraw-timer
              (run-with-timer vterm-timer-delay nil
                              #'vterm--delayed-redraw (current-buffer))))
    (vterm--delayed-redraw (current-buffer))))

(defun vterm--delayed-redraw(buffer)
  "Redraw the terminal buffer .
Argument BUFFER the terminal buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-redisplay t)
            (inhibit-read-only t))
        (when vterm--term
          (vterm--redraw vterm--term)))
      (setq vterm--redraw-timer nil))))

;;;###autoload
(defun vterm (&optional buffer-name)
  "Create a new vterm."
  (interactive)
  (let ((buffer (generate-new-buffer (or buffer-name "vterm"))))
    (with-current-buffer buffer
      (vterm-mode))
    (switch-to-buffer buffer)))

;;;###autoload
(defun vterm-other-window ()
  "Create a new vterm."
  (interactive)
  (let ((buffer (generate-new-buffer "vterm")))
    (with-current-buffer buffer
      (vterm-mode))

    (pop-to-buffer buffer)))

(defun vterm--flush-output (output)
  "Sends the virtual terminal's OUTPUT to the shell."
  (process-send-string vterm--process output))

(defun vterm--filter (process input)
  "I/O Event. Feeds PROCESS's INPUT to the virtual terminal.

Then triggers a redraw from the module."
  (let ((inhibit-redisplay t)
        (inhibit-read-only t)
        (buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer  buf
        (vterm--write-input vterm--term input)
        (vterm--update vterm--term)))))

(defun vterm--sentinel (process _event)
  "Sentinel of vterm PROCESS.
Argument EVENT process event."
  (let ((buf (process-buffer process)))
    (run-hook-with-args 'vterm-exit-functions
                        (if (buffer-live-p buf) buf nil))))

(defun vterm--window-size-change-26 (frame)
  "Callback triggered by a size change of the FRAME.

This is only used, when variable `emacs-version' < 27. Calls
`vterm--window-size-change' for every window of FRAME."
  (dolist (window (window-list frame))
    (vterm--window-size-change window)))

(defun vterm--window-size-change (window)
  "Callback triggered by a size change of the WINDOW.

Feeds the size change to the virtual terminal."
  (with-current-buffer (window-buffer window)
    (when (and (processp vterm--process)
               (process-live-p vterm--process))
      (let ((height (window-body-height window))
            (width (window-body-width window))
            (inhibit-read-only t))
        (set-process-window-size vterm--process height width)
        (vterm--set-size vterm--term height width)))))

(defun vterm--delete-lines (line-num count &optional delete-whole-line)
  "Delete COUNT lines from LINE-NUM.

 If option DELETE-WHOLE-LINE is non-nil, then this command kills
 the whole line including its terminating newline"
  (save-excursion
    (when (vterm--goto-line line-num)
      (delete-region (point) (point-at-eol count))
      (when (and delete-whole-line
                 (looking-at "\n"))
        (delete-char 1)))))

(defun vterm--goto-line(n)
  "Go to line N and return true on success."
  (goto-char (point-min))
  (let ((succ (eq 0 (forward-line (1- n)))))
    succ))

(defun vterm--buffer-line-num()
  "Return the maximum line number."
  (count-lines (point-min) (point-max)))

(defun vterm--set-title (title)
  "Run the `vterm--set-title-hook' with TITLE as argument."
  (run-hook-with-args 'vterm-set-title-functions title))

(defun vterm--set-directory (path)
  "Set `default-directory' to PATH."
  (if (string-match "^\\(.*?\\)@\\(.*?\\):\\(.*?\\)$" path)
      (progn
        (let ((user (match-string 1 path))
              (host (match-string 2 path))
              (dir (match-string 3 path)))
          (if (and (string-equal user user-login-name)
                   (string-equal host (system-name)))
              (progn
                (when (file-directory-p dir)
                  (setq default-directory dir)))
            (setq default-directory (concat "/-:" path)))))
    (when (file-directory-p path)
      (setq default-directory path))))

(defun vterm--get-color(index)
  "Get color by index from `vterm-color-palette'.
Argument INDEX index of color."
  (cond
   ((and (>= index 0)(< index 8))
    (face-foreground
     (elt vterm-color-palette index)
     nil 'default))
   ((and (>= index 8 )(< index 16))
    (face-background
     (elt vterm-color-palette (% index 8))
     nil 'default))
   ((= index -1)               ;-1 foreground
    (face-foreground 'vterm-color-default nil 'default))
   (t                                   ;-2 background
    (face-background 'vterm-color-default nil 'default))))

(provide 'vterm)
;;; vterm.el ends here
