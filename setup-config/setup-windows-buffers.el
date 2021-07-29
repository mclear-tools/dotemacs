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
;; Move to other window
(general-define-key :states '(normal motion visual insert)
  "C-c w" 'other-window)

;; Quickly switch windows in Emacs
(use-package ace-window
  :commands (ace-window ace-swap-window aw-flip-window cpm/window-exchange))

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


;; Numbered window shortcuts for Emacs
(use-package window-numbering
  :disabled
  :hook (after-init . window-numbering-mode)
  :config
  (defun window-numbering-install-mode-line (&optional position)
    "Do nothing, the display is handled by the powerline.")
  (setq window-numbering-auto-assign-0-to-minibuffer nil)

  

  ;; make sure imenu list is always 0
  (defun cpm/window-numbering-assign ()
    "Custom number assignment for imenu-list"
    (when (and (boundp 'imenu-list-buffer-name)
               (string= (buffer-name) imenu-list-buffer-name)
               ;; in case there are two neotree windows. Example: when
               ;; invoking a transient state from neotree window, the new
               ;; window will show neotree briefly before displaying the TS,
               ;; causing an error message. the error is eliminated by
               ;; assigning 0 only to the top-left window
               (eq (selected-window) (window-at 0 0)))
      0))

  ;; ;; make sure neotree is always 0
  ;; (defun spacemacs//window-numbering-assign ()
  ;;   "Custom number assignment for neotree."
  ;;   (when (and (boundp 'neo-buffer-name)
  ;;              (string= (buffer-name) neo-buffer-name)
  ;;              ;; in case there are two neotree windows. Example: when
  ;;              ;; invoking a transient state from neotree window, the new
  ;;              ;; window will show neotree briefly before displaying the TS,
  ;;              ;; causing an error message. the error is eliminated by
  ;;              ;; assigning 0 only to the top-left window
  ;;              (eq (selected-window) (window-at 0 0)))
  ;;     0))

  ;; using lambda to work-around a bug in window-numbering, see
  ;; https://github.com/nschum/window-numbering.el/issues/10
  (setq window-numbering-assign-func
        (lambda () (cpm/window-numbering-assign))))

;; Unset window keys
;; A nice tip from Pragmatic emacs
;; http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/
;; unset C- and M- digit keys
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n))))

(use-package windmove
  :after general
  :commands (windmove-up windmove-down windmove-left windmove-right)
  ;; :general
  ;; (cpm/leader-keys
  ;;   "w"   #'(:ignore t :which-key "Windows")
  ;;   "w l" #'windmove-right
  ;;   "w h" #'windmove-left
  ;;   "w j" #'windmove-down
  ;;   "w k" #'windmove-up)
  :config
  (windmove-default-keybindings)
  (defun cpm/split-window-right-and-focus ()
    "Split the window horizontally and focus the new window."
    (interactive)
    (split-window-right)
    (windmove-right))
  (defun cpm/split-window-below-and-focus ()
    "Split the window vertically and focus the new window."
    (interactive)
    (split-window-below)
    (windmove-down)))


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
                (ibuffer-do-sort-by-alphabetic))))
  )

;;; Bufler
;; ;; A nicer ibuffer
;; ;; https://github.com/alphapapa/bufler.el
;; (use-package bufler
;;   :straight (:host github :repo "alphapapa/bufler.el" :files (:defaults (:exclude "helm*")))
;;   :commands (bufler bufler-switch-buffer))



;;; End Windows & Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-windows-buffers)
