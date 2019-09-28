;; Windows (and Buffers...)

;;; Window Movement
;; Move to other window
(general-define-key :states '(normal motion visual insert)
  "C-o" 'other-window)

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

;; make sure neotree is always 0
(defun spacemacs//window-numbering-assign ()
  "Custom number assignment for neotree."
   (when (and (boundp 'neo-buffer-name)
              (string= (buffer-name) neo-buffer-name)
              ;; in case there are two neotree windows. Example: when
              ;; invoking a transient state from neotree window, the new
              ;; window will show neotree briefly before displaying the TS,
              ;; causing an error message. the error is eliminated by
              ;; assigning 0 only to the top-left window
              (eq (selected-window) (window-at 0 0)))
     0))

 ;; using lambda to work-around a bug in window-numbering, see
 ;; https://github.com/nschum/window-numbering.el/issues/10
 (setq window-numbering-assign-func
       (lambda () (spacemacs//window-numbering-assign))))

;; Unset window keys
;; A nice tip from Pragmatic emacs
;; http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/
;; unset C- and M- digit keys
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n))))

(use-package windmove
  :commands (windmove-up windmove-down windmove-left windmove-right)
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
    (windmove-down))
  ;; add edit mode keybindings
  (global-set-key (kbd "<H-up>")     'windmove-up)
  (global-set-key (kbd "<H-down>")   'windmove-down)
  (global-set-key (kbd "<H-left>")   'windmove-left)
  (global-set-key (kbd "<H-right>")  'windmove-right))

;;; Window Sizing
;; Automatic resizing of Emacs windows to the golden ratio
;; FIXME: currently disabled due to poor interaction with eyebrowse
;; https://github.com/roman/golden-ratio.el/issues/72
(eval-when-compile
  (quelpa
   '(golden-ratio
     :fetcher github :repo "roman/golden-ratio.el")))

(use-package golden-ratio
  :disabled
  :ensure nil
  ;; :after (:any perspective helm nameframe projectile)
  ;; :demand t
  :defer 2
  :config
  (setq golden-ratio-exclude-buffer-names '("*Ilist*" "*Deft*"))
  (setq golden-ratio-exclude-buffer-regexp '("Ilist"))
  (setq golden-ratio-exclude-modes '("dired" "peep-dired"))
  ;; inhibit in helm windows
  (defun cpm--helm-alive-p ()
    (if (boundp 'helm-alive-p)
        (symbol-value 'helm-alive-p)))
  (add-to-list 'golden-ratio-inhibit-functions 'cpm--helm-alive-p)
  ;;fix for ispell
  (defun cpm--ispell-alive-p ()
    (get-buffer ispell-choices-buffer))
  (add-to-list 'golden-ratio-inhibit-functions 'cpm--ispell-alive-p)
  ;; use golden ratio for the following
  (setq golden-ratio-extra-commands
        (append golden-ratio-extra-commands
                '(evil-window-left
                  evil-window-right
                  evil-window-up
                  evil-window-down
                  buf-move-left
                  buf-move-right
                  buf-move-up
                  buf-move-down
                  window-number-select
                  select-window
                  select-window-1
                  select-window-2
                  select-window-3
                  select-window-4
                  select-window-5
                  select-window-6
                  select-window-7
                  select-window-8
                  select-window-9
                  previous-multiframe-window
                  magit-status)))
  (golden-ratio-mode 1))

;;; Window Restore
;; Winner mode is a built-in package for restoring window configurations
;; https://www.emacswiki.org/emacs/WinnerMode
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo winner-mode)
  :config
  (winner-mode 1))

;;; Windows & Buffers
(setq switch-to-buffer-preserve-window-point 'already-displayed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-windows)
