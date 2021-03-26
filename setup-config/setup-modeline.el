;;; Modeline

;;;; Hide Modeline
(use-package emacs-hide-mode-line
  :straight (:type git :host github :repo "hlissner/emacs-hide-mode-line")
  :commands hide-mode-line-mode)

;;;; Modeline Appearance
;; see bespoke theme for further mode line appearance settings

;; set value of mode-line in case we want to revert from header-line
(setq cpm--default-mode-line
      '((:eval
         (mode-line-render
          (format-mode-line (list
                             (format " %s " (winum-get-number-string))
                             "|"
                             ;; (shorten-directory default-directory 32)
                             " %b "
                             ;; (propertize evil-mode-line-tag 'face `(:inherit mode-line))
                             (cond ((and buffer-file-name (buffer-modified-p))
                                    (propertize "(**)" 'face `(:foreground "#f08290")))
                                   (buffer-read-only "(RO)" ))
                             (if (buffer-narrowed-p)
                                 ("⇥"))
                             " %m "))
          (format-mode-line (list
                             (vc-branch)
                             " %4l:%2c:%o"
                             ;;https://emacs.stackexchange.com/a/10637/11934
                             "  ")
                            )))))

;;;; Clean Mode Line
;; https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
;; NOTE: this is only for minor and major modes
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
    (nxhtml-mode . "nx")
    (markdown-mode . "MD")
    (fundamental-mode . "FL"))
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
                                             (+ (if (eq backend 'Hg) 2 3) 2)) " "))  nil))

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

;;;; Header line format
(use-package emacs
  :straight nil
  ;; :after (winum bespoke-themes)
  :config
  ;; Mode line in header
  (define-key mode-line-major-mode-keymap [header-line]
    (lookup-key mode-line-major-mode-keymap [mode-line]))

  ;; Organize mode line
  (defun mode-line-render (left right)
    (let* ((available-width (- (window-width) (length left) )))
      (format (format "%%s %%%ds" available-width) left right)))

  ;; Inactive Header line
  ;; https://emacs.stackexchange.com/a/3522/11934
  (defun cpm-update-header ()
    (mapc
     (lambda (window)
       (with-current-buffer (window-buffer window)
         ;; don't mess with buffers that don't have a header line
         (when header-line-format
           (let ((original-format (get 'header-line-format 'original))
                 (inactive-face 'fringe)) ; change this to your favorite inactive header line face
             ;; if we didn't save original format yet, do it now
             (when (not original-format)
               (put 'header-line-format 'original header-line-format)
               (setq original-format header-line-format))
             ;; check if this window is selected, set faces accordingly
             (if (eq window (selected-window))
                 (setq header-line-format original-format)
               (setq header-line-format `(:propertize ,original-format face ,inactive-face)))))))
     (window-list)))
  (add-hook 'buffer-list-update-hook #'cpm-update-header)

  (setq-default header-line-format
                '((:eval
                   (mode-line-render
                    (format-mode-line (list
                                       ;; (format " %s " (winum-get-number-string))
                                       (cond ((and buffer-file-name (buffer-modified-p))
                                              (propertize " ⨀" 'face `(:inherit error)))
                                             ;; ✱ ⊕ Ⓡ
                                             (buffer-read-only
                                              (propertize " ⨂" 'face `(:inherit font-lock-string-face)))
                                             (t
                                              (propertize " ⨁" 'face `(:inherit isearch :weight normal))))
                                       (propertize " | " 'face `(:inherit fringe)
                                                   'help-echo "Mode(s) menu"
                                                   'mouse-face 'mode-line-highlight
                                                   'local-map   mode-line-major-mode-keymap)
                                       "%b"
                                       (when buffer-file-name
                                         (propertize (concat "•" (file-name-nondirectory (directory-file-name default-directory)) "/") 'face `(:inherit fringe)))

                                       ;; (propertize evil-mode-line-tag 'face `(:inherit bespoke-faded))
                                       (if (buffer-narrowed-p)
                                           (propertize " ⇥"  'face `(:inherit fringe)))
                                       (propertize " %m" 'face `(:inherit fringe))))
                    (format-mode-line (list
                                       (propertize "%l:%c " 'face `(:inherit fringe))
                                       ;;https://emacs.stackexchange.com/a/10637/11934
                                       ;; (propertize (format "%3d%%" (/ (window-start) 0.01 (point-max))) 'face `(:inherit bespoke-faded))
                                       ;; show project name
                                       (when buffer-file-name
                                         (when (bound-and-true-p projectile-mode)
                                           (let ((project-name (projectile-project-name)))
                                             (unless (string= " " project-name)
                                               (propertize (format "%s•" project-name) 'face `(:inherit isearch))))))
                                       (vc-branch)
                                       " "
                                       ))))))

  ;; (setq-default header-line-format nil)
  ;; (setq-default mode-line-format cpm--default-mode-line)
  )



;;; End Modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-modeline)
