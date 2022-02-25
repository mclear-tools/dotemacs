;; UI & Appearance

;;; Scrolling
(use-package emacs
  :straight (:type built-in)
  :config
  ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
  ;; for tall lines.
  (setq auto-window-vscroll nil)
  ;; Settings for better cursor
  ;; see https://two-wrongs.com/centered-cursor-mode-in-vanilla-emacs
  ;;  (NOTE: A number of 101+ disables re-centering.)
  (setq maximum-scroll-margin 0.5
        scroll-margin 99999
        scroll-conservatively 101
        scroll-preserve-screen-position 1))

(use-package mwheel
  :straight (:type built-in)
  :config
  ;; Optimize mouse wheel scrolling for smooth-scrolling trackpad use.
  ;; Trackpads send a lot more scroll events than regular mouse wheels,
  ;; so the scroll amount and acceleration must be tuned to smooth it out.
  (setq
   ;; If the frame contains multiple windows, scroll the one under the cursor
   ;; instead of the one that currently has keyboard focus.
   mouse-wheel-follow-mouse 't
   ;; Completely disable mouse wheel acceleration to avoid speeding away.
   mouse-wheel-progressive-speed nil
   mwheel-coalesce-scroll-events t
   ;; The most important setting of all! Make each scroll-event move 2 lines at
   ;; a time (instead of 5 at default). Simply hold down shift to move twice as
   ;; fast, or hold down control to move 3x as fast. Perfect for trackpads.
   mouse-wheel-scroll-amount '(2 ((shift) . 4) ((control) . 6))))


;;; Frames

;;;; Frame defaults
(use-package frame
  :straight (:type built-in)
  :config
  ;; Make a clean & minimalist frame
  (setq-default initial-frame-alist
                (append (list
                         '(fullscreen . maximized)
                         '(internal-border-width . 10)
                         '(left-fringe    . 0)
                         '(right-fringe   . 0)
                         '(tool-bar-lines . 0)
                         '(menu-bar-lines . 0)
                         '(vertical-scroll-bars . nil)
                         '(horizontal-scroll-bars . nil)
                         '(height . 45)
                         '(width . 85)
                         )))
  (setq-default default-frame-alist
                (append (list
                         '(frame-title-format . nil)
                         '(internal-border-width . 20)
                         '(left-fringe    . 0)
                         '(right-fringe   . 0)
                         '(tool-bar-lines . 0)
                         '(menu-bar-lines . 0)
                         '(vertical-scroll-bars . nil)
                         '(horizontal-scroll-bars . nil)
                         )))
  ;; Resize pixel-wise to avoid gaps
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t)
  ;; Don't show icon in frame
  (setq-default ns-use-proxy-icon nil))

;;;; Fix titlebar titling colors
;; see also https://github.com/d12frosted/homebrew-emacs-plus/issues/55
(use-package ns-auto-titlebar
  :commands ns-auto-titlebar-mode
  :if (eq system-type 'darwin)
  :init (ns-auto-titlebar-mode))

;;;; Center Frames
;; https://christiantietze.de/posts/2021/06/emacs-center-window-single-function/
(defun cpm/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame) frame) (selected-frame)))
           (frame-w (frame-pixel-width frame))
           (frame-h (frame-pixel-height frame))
           ;; frame-monitor-workarea returns (x y width height) for the monitor
           (monitor-w (nth 2 (frame-monitor-workarea frame)))
           (monitor-h (nth 3 (frame-monitor-workarea frame)))
           (center (list (/ (- monitor-w frame-w) 2)
                         (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))

;; (add-hook 'after-init-hook #'cpm/frame-recenter)
(add-hook 'after-make-frame-functions #'cpm/frame-recenter)

;;;; Fringe
(use-package fringe
  :straight (:type built-in)
  :custom
  ;; minimal fringe
  (fringe-mode '(0 . 0)))

;;; Color
(setq-default ns-use-srgb-colorspace t)

;;; Fonts

(use-package fontset
  :straight (:type built-in)
  :config
  ;; Use symbola for proper symbol glyphs
  (when (member "Symbola" (font-family-list))
    (set-fontset-font
     t 'symbol "Symbola" nil 'prepend))
  ;; Use Apple emoji
  ;; NOTE that emoji here must be set to unicode to get color emoji
  (when (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font
     t 'unicode (font-spec :family "Apple Color Emoji") nil 'prepend)))

(use-package faces
  :straight (:type built-in)
  :config
  (set-face-attribute 'default nil
                      :font   "SF Mono"
                      :height 130
                      :weight 'normal)
  (set-face-attribute 'variable-pitch nil
                      :font "Avenir Next"
                      :height 200
                      :weight 'normal))

;; Set default line spacing (in pixels)
(setq-default line-spacing 0.05)


;;; Font Lock
(use-package font-lock
  :straight (:type built-in)
  :custom
  ;; Max font lock decoration (set nil for less)
  (font-lock-maximum-decoration t)
  ;; No limit on font lock
  (font-lock-maximum-size nil))


;;; Scale Text
;; When using `text-scale-increase', this sets each 'step' to about one point size.
(setq text-scale-mode-step 1.08)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-adjust)

;;; Bidirectional Text
;; Disable bidirectional text support. Why?
;; .. slight performance improvement.
(setq bidi-display-reordering nil)

;;; Line Numbers
(use-package display-line-numbers
  :straight (:type built-in)
  ;; :hook (markdown-mode prog-mode)
  :commands display-line-numbers-mode
  :init
  (setq-default display-line-numbers-type 'visual)
  (setq-default display-line-numbers-width-start t))


;;; Empty Lines
;; Don't show empty lines.
;; .. Allows you to tell if there are blank lines at the end of the file.
(setq-default indicate-empty-lines nil)

;;; Dialogs and popups
;; No file dialog
(setq use-file-dialog nil)
;; No dialog box
(setq use-dialog-box nil)
;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)
;; Set popup windows
(setq-default pop-up-windows t)
;; Set popup frames
(setq-default pop-up-frames nil)


;;; Highlight
;;;; Highlight Lines
;; Highlight lines. You can toggle this off
(use-package hl-line+
  :straight t
  :custom-face
  ;; subtle highlighting
  (hl-line ((t (:inherit highlight))))
  :custom
  (global-hl-line-mode nil)
  (hl-line-flash-show-period 1.0)
  (hl-line-inhibit-highlighting-for-modes '(dired-mode))
  (hl-line-when-idle-interval 2)
  :config
  (toggle-hl-line-when-idle 1 t)
  )

;;;; Highlight Numbers & TODOS
(use-package highlight-numbers
  :defer t
  :commands highlight-numbers-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package hl-todo
  :defer t
  :commands hl-todo-mode
  :init
  ;; (add-hook 'org-mode-hook #'hl-todo-mode)
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'markdown-mode-hook #'hl-todo-mode))

;; hydra for TODOs
(with-eval-after-load 'hydra
  (defhydra cpm/hydra-todo
    (:pre
     (hl-todo-mode 1)
     :post
     (hl-todo-mode -1))
    "Todo"
    ("n" hl-todo-next "Next")
    ("p" hl-todo-previous "Previous")
    ("o" hl-todo-occur "Occur")
    ("q" nil "Quit" :color blue :exit t)))

;; ;;https://github.com/erickgnavar/dotfiles/tree/master/.emacs.d#highlight-todo-fixme-etc
;; (defun cpm/highlight-todo-like-words ()
;;   (font-lock-add-keywords
;;    nil `(("\\<\\(FIXME\\|TODO\\|NOTE\\)"
;;           1 font-lock-warning-face t))))


;; (add-hook 'prog-mode-hook 'my/highlight-todo-like-words)


;;;; Highlight Cursor Line with Pulse
;; From https://karthinks.com/software/batteries-included-with-emacs/
;; Replace external package with internal command

(use-package pulse
  :straight (:type built-in)
  :defer 1
  :commands (pulse-line pulse-momentary-highlight-one-line)
  :config
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (interactive)
    (pulse-momentary-highlight-one-line (point)))
  ;; pulse for commands
  (dolist (command '(scroll-up-command scroll-down-command
                                       recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line))
  ;; pulse on window change
  (push 'pulse-line window-selection-change-functions)
  )

;;;; Crosshair Highlighting
(use-package crosshairs
  :straight t
  :commands (crosshairs-highlight
             crosshairs-mode
             flash-crosshairs)
  :bind (:map cpm+toggle-keys
         ("c" . crosshairs-mode))
  :custom-face
  (col-highlight ((t (:inherit hl-line))))
  :config
  ;; same colors for both hlines
  (setq col-highlight-vline-face-flag t))

;;; Icons
(use-package all-the-icons
  :defer t)

(use-package font-lock+
  :defer 1)
;; icons for dired
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :defer t
  :commands all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; No ugly button for checkboxes
(setq widget-image-enable nil)


;;; Emoji
(use-package emojify
  :commands (emojify-mode emojify-apropos-emoji)
  ;; :hook ((prog-mode markdown-mode) . emojify-mode)
  :config
  (setq emojify-emojis-dir (concat cpm-etc-dir "emojis")))

;;; Underline
(setq x-underline-at-descent-line t)

;;; Helpful Information
;; Much better lookup both in details and headings/aesthetics
;; Better help info

;; NOTE: emacs 29 has a breaking change so using el-patch to keep helpful working
;; see https://github.com/Wilfred/helpful/pull/283

(use-package helpful
  :defer t
  :bind (("C-h f"   . #'helpful-function)
         ("C-h k"   . #'helpful-key)
         ("C-h o"   . #'helpful-symbol)
         ("C-h v"   . #'helpful-variable)
         ("C-h C-." . #'helpful-at-point)
         ("C-h C-l" . #'find-library))
  :init
  ;; HACK - see https://github.com/hlissner/doom-emacs/issues/6063
  (defvar read-symbol-positions-list nil)
  :config/el-patch
  (defun helpful--autoloaded-p (sym buf)
    "Return non-nil if function SYM is autoloaded."
    (-when-let (file-name (buffer-file-name buf))
      (setq file-name (s-chop-suffix ".gz" file-name))
      (help-fns--autoloaded-p sym)))

  (defun helpful--skip-advice (docstring)
    "Remove mentions of advice from DOCSTRING."
    (let* ((lines (s-lines docstring))
           (relevant-lines
            (--take-while
             (not (or (s-starts-with-p ":around advice:" it)
                      (s-starts-with-p "This function has :around advice:" it)))
             lines)))
      (s-trim (s-join "\n" relevant-lines)))))

;; Display file commentary section
(global-set-key (kbd "C-h C-c") 'finder-commentary)


;;;; Helpful Demos
(use-package elisp-demos
  :defer 1
  :config
  ;; inject demos into helpful
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))


;;; Better Info
;; Better looking info pages
(use-package info-colors
  :straight (:host github :repo "ubolonton/info-colors")
  :defer t
  :config
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

;;; Xwidget Browser
(use-package xwwp-follow-link
  :straight (:host github :repo "canatella/xwwp")
  :custom
  (xwwp-follow-link-completion-system 'default)
  :bind (:map xwidget-webkit-mode-map
         ("v" . xwwp-follow-link)))

;;; Mode line
;;;; Hide Modeline
(use-package emacs-hide-mode-line
  :straight (:type git :host github :repo "hlissner/emacs-hide-mode-line")
  :commands hide-mode-line-mode)

;;; Dim inactive windows
(use-package dimmer
  :straight (:host github :repo "gonewest818/dimmer.el")
  :hook (after-init . dimmer-mode)
  :config
  ;; (setq mini-frame-create-lazy nil)
  (setq dimmer-prevent-dimming-predicates '(window-minibuffer-p))
  (setq dimmer-fraction 0.5)
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-use-colorspace :rgb)
  (setq dimmer-watch-frame-focus-events nil)
  (dimmer-configure-which-key)
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (dimmer-configure-posframe)
  (dimmer-configure-vertico))

(defun dimmer-configure-vertico ()
  "Convenience settings for vertico-buffer users."
  (with-no-warnings
    (add-to-list
     'dimmer-buffer-exclusion-regexps "^ \\*Vertico\\*$")))
    ;; (add-to-list
     ;; 'dimmer-prevent-dimming-predicates #'vertico-buffer-mode)))


;;; Cursor
(use-package emacs
  :straight (:type built-in)
  :custom
  ;; don't show cursor in inactive windows
  (cursor-in-non-selected-win dows nil))

;;; Reveal Mode
;; Toggle uncloaking of invisible text near point, including folded org headlines (Reveal mode).
(use-package reveal
  :straight (:type built-in)
  :defer 1
  :config
  (setq reveal-auto-hide nil)
  (global-reveal-mode))

;;; End UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-ui)
