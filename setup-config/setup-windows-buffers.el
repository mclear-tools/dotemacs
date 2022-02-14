;; Windows (and Buffers...)

;;; Windows
;; Vertical window divider
(use-package frame
  :straight (:type built-in)
  :custom
  (window-divider-default-right-width 12)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t))
;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)

;;; Window Movement

;; Quickly switch windows in Emacs
(use-package ace-window
  :commands (ace-window ace-swap-window aw-flip-window cpm/window-exchange))

(defun cpm/other-window ()
  (interactive)
  (other-window 1))
(bind-key* "C-c C-o" 'cpm/other-window)

(use-package emacs-winum
  :straight (winum :type git :host github :repo "deb0ch/emacs-winum")
  :hook (after-init . winum-mode)
  :custom
  ;; seems to require being set in custom to take effect
  (winum-auto-setup-mode-line nil)
  :config
  (setq window-numbering-scope            'global
        winum-reverse-frame-list          nil
        winum-auto-assign-0-to-minibuffer t
        ;; winum-format                      " %s
        ;; winum-mode-line-position          1
        winum-ignored-buffers             '(" *which-key*")
        winum-ignored-buffers-regexp      '(" \\*Treemacs-.*")))

(use-package windmove
  :commands (windmove-up windmove-down windmove-left windmove-right)
  :bind (("H-l" . #'windmove-right)
         ("H-j" . #'windmove-down)
         ("H-h" . #'windmove-left)
         ("H-k" . #'windmove-up))
  :config
  (windmove-default-keybindings))
(defun cpm/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (require 'windmove)
  (split-window-right)
  (windmove-right))
(defun cpm/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (require 'windmove)
  (split-window-below)
  (windmove-down))

;;; Window Restore
;; Winner mode is a built-in package for restoring window configurations
;; https://www.emacswiki.org/emacs/WinnerMode
(use-package winner
  :straight nil
  :hook (after-init . winner-mode))

;;; Windows & Buffers
;; switch-to-buffer tries to preserve window-point
(setq switch-to-buffer-preserve-window-point 'already-displayed)

;;; Unique buffers
(use-package uniquify
  :straight (:type built-in)
  :defer 3
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;;; Buffer Modes
;; from http://www.jurta.org/en/emacs/dotemacs, set the major mode
;; of buffers that are not visiting a file
(setq-default major-mode (lambda ()
                           (if buffer-file-name
                               (fundamental-mode)
                             (let ((buffer-file-name (buffer-name)))
                               (set-auto-mode)))))

;;; Revert All Buffers

(use-package revert-buffer-all
  :straight (:type git :host gitlab :repo "ideasman42/emacs-revert-buffer-all")
  :commands (revert-buffer-all))

;;; iBuffer
(use-package ibuffer
  :straight (:type built-in)
  :commands (ibuffer)
  :custom
  (ibuffer-default-sorting-mode 'major-mode)
  (ibuffer-filter-group-name-face 'outline-1)
  (ibuffer-movement-cycle t)
  (ibuffer-old-time 12)
  (ibuffer-modified-char ?*)
  (ibuffer-read-only-char ?R)
  (ibuffer-marked-char ?➤)
  (ibuffer-locked-char ?L)
  (ibuffer-deletion-char ?🗙)
  (ibuffer-use-header-line nil)
  :config
  ;; Fix function for displaying groups
  (defun ibuffer-insert-filter-group (name display-name filter-string format bmarklist)
    (add-text-properties
     (point)
     (progn
       (insert display-name)
       (point))
     `(ibuffer-filter-group-name
       ,name
       font-lock-face ,ibuffer-filter-group-name-face
       keymap ,ibuffer-mode-filter-group-map
       mouse-face highlight
       help-echo ,(let ((echo '(if tooltip-mode
				                   "mouse-1: toggle marks in this group\nmouse-2: hide/show this filtering group"
			                     "mouse-1: toggle marks  mouse-2: hide/show")))
		            (if (> (length filter-string) 0)
		                `(concat ,filter-string
			                     (if tooltip-mode "\n" " ")
			                     ,echo)
		              echo))))
    (insert "\n")
    (when bmarklist
      (put-text-property
       (point)
       (progn
         (dolist (entry bmarklist)
	       (ibuffer-insert-buffer-line (car entry) (cdr entry) format))
         (point))
       'ibuffer-filter-group
       name)))
  )

(use-package ibuffer-vc
  :straight (:host github :repo "purcell/ibuffer-vc")
  :defer 2
  :config
  ;; To include vc status info in the ibuffer list, add either
  ;; vc-status-mini or vc-status to `ibuffer-formats':
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                vc-relative-file)))

  ;; Don't need to display "Git" for every project; use a symbol instead
  (defun ibuffer-vc-generate-filter-groups-by-vc-root ()
    "Create a set of ibuffer filter groups based on the vc root dirs of buffers."
    (let ((roots (ibuffer-remove-duplicates
                  (delq nil (mapcar 'ibuffer-vc-root (buffer-list))))))
      (mapcar (lambda (vc-root)
                (cons (format " %s" (cdr vc-root))
                      `((vc-root . ,vc-root))))
              roots)))

  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))


;;; Custom Functions
;;;; Exchange Windows
;; Swap buffers in windows and leave the cursor in the original window. Courtesy of
;; Mike Zamansky's video.
;; http://cestlaz.github.io/posts/using-emacs-36-touch-of-elisp/#.WX5Wg0czpcx

(defun cpm/window-exchange-buffer ()
  "Swap buffer in windows and leave focus in original window"
  (interactive)
  (ace-swap-window)
  (aw-flip-window))

;;;; Rotate Windows
;; from magnars modified by ffevotte for dedicated windows support
(defun cpm/rotate-windows (count)
  "Rotate your windows.
  Dedicated windows are left untouched. Giving a negative prefix
  argument takes the kindows rotate backwards."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))

                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

(defun cpm/rotate-windows-backward (count)
  "Rotate your windows backward."
  (interactive "p")
  (cpm/rotate-windows (* -1 count)))

;;;; Split Windows
(defun cpm/toggle-window-split ()
  "Move from a horizontal to a vertical split and vice versa"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


;;;; Jump to Minibuffer Window
(defun cpm/goto-minibuffer-window ()
  "locate point to minibuffer window if it is active."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

;; (with-eval-after-load 'general
;; (general-def "C-c m" #'cpm/goto-minibuffer-window))

;;;; Blank Buffer New Frame
;; Make a blank buffer when opening a new frame. From
;; https://stackoverflow.com/a/25792276.

(defun cpm/new-buffer-new-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))


;;;; Create new buffer
(defun cpm/create-new-buffer ()
  (interactive)
  (let ((buffer (generate-new-buffer "*new*")))
    (set-window-buffer nil buffer)
    (with-current-buffer buffer
      (funcall (default-value 'major-mode)))))

;;;; Make Temp Buffer
(defun cpm/tmp-buffer()
  "Make a temporary buffer and switch to it"
  (interactive)
  (switch-to-buffer (get-buffer-create (concat "tmp-" (format-time-string "%m.%dT%H.%M.%S"))))
  (delete-other-windows))

;;;; Revert all buffers
;;
(defun cpm/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

;;;; Clipboard to/from Buffer
;; http://stackoverflow.com/a/10216338/4869
(defun cpm/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun cpm/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;;;; Useful Buffers
;; TODO: make this respect workspace buffers
(defun cpm/user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
  Typically, if buffer name starts with *, it's not considered a user buffer.
  This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
  You can override this function to get your idea of “user buffer”.
  version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

(defun cpm/next-user-buffer ()
  "Switch to the next user buffer.
  “user buffer” is determined by `cpm/user-buffer-q'.
  URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
  Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (cpm/user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun cpm/previous-user-buffer ()
  "Switch to the previous user buffer.
  “user buffer” is determined by `cpm/user-buffer-q'.
  URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
  Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (cpm/user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

;;;; Eval emacs buffer until error

(defun cpm/eval-buffer-until-error ()
  "Evaluate emacs buffer until error occured."
  (interactive)
  (goto-char (point-min))
  (while t (eval (read (current-buffer)))))

;;;; Kill Current Buffer
;; (kill-this-buffer) is unreliable when not invoked from the menubar. So here's a
;; wrapper on (kill-buffer) to kill the current buffer. This is sometimes better
;; than (evil-delete-buffer) since it keeps the window.

(defun cpm/kill-this-buffer ()
  (interactive)
  (kill-buffer))

;;;; Show Filename of Buffer

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun cpm/show-and-copy-buffer-full-filename ()
  "Show the full path to the current file in the minibuffer and copy to clipboard."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun cpm/show-and-copy-buffer-filename ()
  "Show the abbreviated path to the current file in the minibuffer and copy to clipboard."
  (interactive)
  (let ((file-name (abbreviate-file-name buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

;;;; Switch previous buffer

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;; End Windows & Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-windows-buffers)
