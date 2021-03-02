;;; Modeline

;;;; Hide Modeline
(use-package emacs-hide-mode-line
  :straight (:type git :host github :repo "hlissner/emacs-hide-mode-line")
  :commands hide-mode-line-mode)

;;;; Modeline Bell
(use-package mode-line-bell
  :straight t
  :hook (after-init . mode-line-bell-mode))

;;;; Modeline Position

;; Put modeline at top of buffer
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format'(""))
(setq x-underline-at-descent-line t)


;;;; Modeline Underline
;; set modeline to underline
;; (set-face-attribute 'mode-line nil
;;                     :underline (face-foreground 'default)
;;                     :overline nil
;;                     :box nil
;;                     :foreground (face-background 'default)
;;                     :background (face-background 'default))
;; (set-face 'mode-line-inactive                            'mode-line)

;;;; Clean Mode Line

(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas/minor-mode . " υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (abbrev-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (emacs-lisp-mode . "EL")
    (nxhtml-mode . "nx"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (require'cl)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; Header Line
;;;; Functions for Header Line
;; https://www.gonsie.com/blorg/modeline.html
(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "" (substring-no-properties vc-mode
                                             (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ---------------------------------------------------------------------
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))



;;;; Header Line Setup

;; Mode line (this might be slow because of the "☰" that requires substitution)
;; This line below makes things a bit faster
(set-fontset-font "fontset-default"  '(#x2600 . #x26ff) "Fira Code 16")

(define-key mode-line-major-mode-keymap [header-line]
  (lookup-key mode-line-major-mode-keymap [mode-line]))

(defun mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))
(setq-default header-line-format
              '((:eval
                 (mode-line-render
                  (format-mode-line (list
                                     ;;FIXME this next line throws error at startup
                                     (format " %s " (winum-get-number-string))
                                     (propertize "|" 'face `(:inherit face-faded)
                                                 'help-echo "Mode(s) menu"
                                                 'mouse-face 'mode-line-highlight
                                                 'local-map   mode-line-major-mode-keymap)
                                     " %b "
                                     (if (and buffer-file-name (buffer-modified-p))
                                         (propertize "(**)" 'face `(:foreground "#f08290")))
                                     (propertize " %m " 'face `(:inherit face-faded))))
                  (format-mode-line (list
                                     (vc-branch)
                                     (propertize "%4l:%2c  " 'face `(:inherit face-faded))))
                  ))))



;;; End Modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-modeline)
