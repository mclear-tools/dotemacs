;;;; Icomplete
(use-package icomplete-vertical
  :disabled
  :straight (:type built-in)
  :hook (after-init . icomplete-vertical-mode)
  :general
  (:keymaps 'icomplete-minibuffer-map
   "C-v"    'icomplete-vertical-toggle
   "RET"    'icomplete-force-complete-and-exit
   "TAB"    'icomplete-force-complete-and-exit
   "C-M-i"  'minibuffer-complete
   "M-RET"  'exit-minibuffer
   "<down>" 'icomplete-forward-completions
   "C-j"    'icomplete-forward-completions
   "<up>"   'icomplete-backward-completions
   "C-k"    'icomplete-backward-completions)
  :custom
  (icomplete-show-matches-on-no-input t)
  (icomplete-hide-common-prefix nil)
  (icomplete-compute-delay 0.0)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode))

;; ;;;; Vertico
;; (use-package vertico
;;   :general
;;   (:keymaps 'vertico-map
;;    "C-j"    'vertico-next
;;    "C-k"    'vertico-previous)
;;   :init
;;   (vertico-mode)
;;   :config
;;   ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
;;   (setq vertico-cycle t))

;; ;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   (defun crm-indicator (args)
;;     (cons (concat "[CRM] " (car args)) (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Grow and shrink minibuffer
;;   ;;(setq resize-mini-windows t)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t))

;; ;; Persist history over Emacs restarts using savehist (see setup-settings). Vertico sorts by history position.

;; (use-package orderless
;;   :init
;;   (setq completion-styles '(orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))

;; (selectrum-mode -1)

;;;; Sticky Buffer
;; Stick/Lock buffer to window, courtesy of ShingoFukuyama.
;; https://gist.github.com/ShingoFukuyama/8797743

;; (defvar sticky-buffer-previous-header-line-format)
;; (define-minor-mode sticky-buffer-mode
;;   "Make the current window always display this buffer."
;;   nil " sticky" nil
;;   (if sticky-buffer-mode
;;       (progn
;;         (set (make-local-variable 'sticky-buffer-previous-header-line-format)
;;              header-line-format)
;;         (set-window-dedicated-p (selected-window) sticky-buffer-mode))
;;     (set-window-dedicated-p (selected-window) sticky-buffer-mode)
;;     (setq header-line-format sticky-buffer-previous-header-line-format)))
