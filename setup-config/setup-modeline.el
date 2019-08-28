;;; Modeline

;;;; Hide Modeline
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;;;; Doom Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (doom-modeline-bar ((t (:inherit highlight :inverse-video t :background "#268bd2"))))
  (doom-modeline-eyebrowse ((t (:inherit highlight))))
  (doom-modeline-inactive-bar ((t (:inherit highlight))))
  :config
  (setq doom-modeline-bar-width 3
        doom-modeline-height 38
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-major-mode-color-icon t
        doom-modeline-enable-word-count t
        doom-modeline-persp-name t
        doom-modeline-buffer-encoding nil
        doom-modeline-persp-name-icon t
        find-file-visit-truename t
        doom-modeline-minor-modes nil)

  ;; Change the evil tag
  (setq evil-normal-state-tag   (propertize " 🅝" )
        evil-emacs-state-tag    (propertize " 🅔" )
        evil-insert-state-tag   (propertize " 🅘" )
        evil-replace-state-tag  (propertize " 🅡" )
        evil-motion-state-tag   (propertize " 🅜" )
        evil-visual-state-tag   (propertize " 🅥" )
        evil-operator-state-tag (propertize " 🅞" ))

  (doom-modeline-def-segment evil-state
    "The current evil state. Requires `evil-mode' to be enabled."
    (when (bound-and-true-p evil-local-mode)
      (let ((tag (evil-state-property evil-state :tag t)))
        (propertize tag 'face
                    (if (doom-modeline--active)
                        (cond ((eq tag evil-normal-state-tag)   '(:foreground "DarkGoldenrod2" :height 1.25))
                              ((eq tag evil-emacs-state-tag)    '(:foreground "SkyBlue2" :height 1.25))
                              ((eq tag evil-insert-state-tag)   '(:foreground "chartreuse3" :height 1.25))
                              ((eq tag evil-motion-state-tag)   '(:foreground "plum3" :height 1.25))
                              ((eq tag evil-replace-state-tag)  '(:foreground "red" :height 1.25))
                              ((eq tag evil-visual-state-tag)   '(:foreground "gray" :height 1.25))
                              ((eq tag evil-operator-state-tag) '(:foreground "red" :height 1.25))))))))


  ;; window number faces & formatting
  (doom-modeline-def-segment window-number
    (if (bound-and-true-p window-numbering-mode)
        (propertize (format " %s " (window-numbering-get-number-string))
                    'face (if (doom-modeline--active)
                              'doom-modeline-active-window-number
                            'doom-modeline-inactive-window-number))
      ""))

  ;; workspace number faces & formatting
  (doom-modeline-def-segment workspace-name
    "The current workspace name or number.
  Requires `eyebrowse-mode' to be enabled."
    (if (and (bound-and-true-p eyebrowse-mode)
             (< 1 (length (eyebrowse--get 'window-configs))))
        (let* ((num (eyebrowse--get 'current-slot))
               (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
               (str (if (and tag (< 0 (length tag)))
                        tag
                      (when num (int-to-string num)))))
          (assq-delete-all 'eyebrowse-mode mode-line-misc-info)
          (concat
           (propertize (format " %s " str) 'face
                       (if (doom-modeline--active)
                           '(:foreground "#2aa198")
                         'mode-line-inactive))
           (propertize "|" 'face '(:foreground "#586e75"))))))

  ;;
  ;; Mode line setup
  ;;
  (doom-modeline-def-modeline 'cpm/my-doom-mode-line
    '(workspace-name window-number bar evil-state buffer-info vcs matches remote-host parrot selection-info)
    '(misc-info persp-name input-method buffer-encoding process checker buffer-position " "))

  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'cpm/my-doom-mode-line 'default))

  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)

  (defface doom-modeline-active-window-number
    '((t (:inherit warning)))
    "Face for active window number segment of the mode-line."
    :group 'doom-modeline)
  (defface doom-modeline-inactive-window-number
    '((t (:inherit mode-line-emphasis)))
    "Face for inactive window number segment of the mode-line."
    :group 'doom-modeline)

  :custom-face
  (doom-modeline-eyebrowse ((t (:inherit highlight))))
  (doom-modeline-bar ((t (:inherit highlight :inverse-video t :background "#268bd2"))))
  (doom-modeline-inactive-bar ((t (:inherit highlight)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-modeline)
