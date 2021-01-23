;;; Narrowing

;;;; Icomplete vertical narrowing
(use-package icomplete
  :after general
  :demand t
  :straight nil
  :general
  (:keymaps 'icomplete-minibuffer-map
   "RET"    'icomplete-force-complete-and-exit
   "C-M-i"  'minibuffer-complete
   "M-RET"  'exit-minibuffer
   "<down>" 'icomplete-forward-completions
   "C-j"    'icomplete-forward-completions
   "<up>"   'icomplete-backward-completions
   "C-k"    'icomplete-backward-completions)
  :custom
  (icomplete-show-matches-on-no-input t)
  (icomplete-hide-common-prefix nil))

(use-package icomplete-vertical
  :straight (icomplete-vertical :type git :host github :repo "oantolin/icomplete-vertical")
  :after general
  :demand t
  :custom
  (completion-styles '(partial-completion substring))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode)
  :general
  (:keymaps 'icomplete-minibuffer-map
   "C-v"    'icomplete-vertical-toggle))

;;;; Embark
(use-package embark
  :straight (embark :type git :host github :repo "oantolin/embark")
  :after (icomplete-vertical)
  :commands (embark-act embark-keymap-help)
  :bind ("C-o" . embark-act)
  :general
  (:keymaps 'minibuffer-local-completion-map
   "C-:" 'embark-act
   "C-J" 'embark-switch-to-live-occur
   "M-q" 'embark-occur-toggle-view)
  (:keymaps 'completion-list-mode-map
   ";"  'embark-act)
  (:keymaps 'embark-file-map
   "x"  'consult-file-externally)
  :config
  (add-to-list 'embark-allow-edit-commands 'consult-imenu)
  (setq embark-quit-after-action t)
  ;; (defun unique-completion ()
  ;;   (when (= (length (embark-minibuffer-candidates)) 1)
  ;;     (run-at-time 0 nil #'minibuffer-force-complete-and-exit)))
  ;; (setf (alist-get 'variable embark-keymap-alist) 'embark-symbol-map)
  ;; (defun resize-embark-live-occur-window (&rest _)
  ;;   (when (string-match-p "Live" (buffer-name))
  ;;     (fit-window-to-buffer (get-buffer-window)
  ;;                           (floor (frame-height) 2) 1)))
  ;; (add-hook 'embark-pre-action-hook #'completion--flush-all-sorted-completions))

  )

;;;; Marginalia
;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  :after icomplete-vertical
  :general
  (:keymaps 'minibuffer-local-map
   "C-M-a"  'marginalia-cycle)
  ;; When using the Embark package, you can bind `marginalia-cycle' as an Embark action
  (:keymaps 'embark-general-map
   "A"  'marginalia-cycle)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)
  ;; ;; When using Selectrum, ensure that Selectrum is refreshed when cycling annotations.
  ;; (advice-add #'marginalia-cycle :after
  ;;             (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;;;; Consult
;; Example configuration for Consult
(use-package consult
  :straight (consult :type git :host github :repo "minad/consult")
  :commands (consult-buffer consult-find consult-apropos consult-yank-pop)
  :init
  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)
  :config
  (setq consult--find-cmd '("mdfind" "-name"))
  ;; Optionally configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-async-min-input 0)
  (setq consult-async-input-debounce 0.01)

  ;; Optionally configure narrowing key.
  ;; Both < and C-+ work reasonably well.
  ;; (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optional configure a view library to be used by `consult-buffer'.
  ;; The view library must provide two functions, one to open the view by name,
  ;; and one function which must return a list of views as strings.
  ;; Example: https://github.com/minad/bookmark-view/
  ;; (setq consult-view-open-function #'bookmark-jump
  ;;       consult-view-list-function #'bookmark-view-names)

  ;; Optionally enable previews. Note that individual previews can be disabled
  ;; via customization variables.
  ;; (consult-preview-mode)

  )



;;;; Orderless
(use-package orderless
  :ensure t
  :straight t
  :init (icomplete-mode) ; optional but recommended!
  :custom (completion-styles '(orderless)))

;;; End Setup Narrowing
(provide 'setup-narrowing)
