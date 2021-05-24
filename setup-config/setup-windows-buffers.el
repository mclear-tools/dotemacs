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


;;; End Windows & Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-windows-buffers)
