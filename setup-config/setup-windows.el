;; Windows (and Buffers...)

;;; Window Movement
;; Move to other window
(general-define-key :states '(normal motion visual insert)
  "C-c w" 'other-window)

;; Quickly switch windows in Emacs
(use-package ace-window
  :commands (ace-window ace-swap-window aw-flip-window cpm/window-exchange))

;; Numbered window shortcuts for Emacs
(use-package window-numbering
  :defer 1
  :config
  (defun window-numbering-install-mode-line (&optional position)
    "Do nothing, the display is handled by the powerline.")
  (setq window-numbering-auto-assign-0-to-minibuffer nil)

  (window-numbering-mode 1)

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
  :general
  (cpm/leader-keys
    "w"   #'(:ignore t :which-key "Windows")
    "w l" #'windmove-right
    "w h" #'windmove-left
    "w j" #'windmove-down
    "w k" #'windmove-up)
  :config
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
  :commands (winner-undo winner-redo winner-mode)
  :config
  (winner-mode 1))

;;; Windows & Buffers
(setq switch-to-buffer-preserve-window-point 'already-displayed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-windows)
