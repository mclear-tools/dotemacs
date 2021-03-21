;; All packages related to narrowing and completion

;;; Narrowing Completion

;;;; Icomplete
(use-package icomplete-vertical
  :disabled
  :straight (icomplete-vertical :type git :host github :repo "oantolin/icomplete-vertical")
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

;;;; Selectrum
;; selectrum is nice but I think I like Icomplete better
;; Icomplete is less buggy and orderless works well.

(use-package selectrum
  :straight t
  :hook (after-init . selectrum-mode)
  :general
  (:keymaps 'selectrum-minibuffer-map
   ;; "RET"    'icomplete-force-complete-and-exit
   "C-M-i"  'minibuffer-complete
   "M-RET"  'exit-minibuffer
   "<down>" 'selectrum-next-candidate
   "C-j"    'selectrum-next-candidate
   "<up>"   'selectrum-previous-candidate
   "C-k"    'selectrum-previous-candidate)
  :config
  (setq selectrum-num-candidates-displayed 10)
  (setq selectrum-fix-vertical-window-height t)
  (setq selectrum-extend-current-candidate-highlight t)
  (setq selectrum-count-style 'current/matches)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  (setq selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-mode +1))

(use-package selectrum-prescient
  :straight t
  :after selectrum
  :custom-face
  (selectrum-prescient-primary-highlight ((t (:weight bold :foreground "#EBCB8B"))))
  (selectrum-prescient-secondary-highlight ((t (:weight bold :foreground "#81A1C1"))))
  :config
  (setq selectrum-prescient-enable-filtering nil)
  (setq prescient-save-file (concat cpm-cache-dir "prescient-save.el"))
  (prescient-persist-mode)
  (selectrum-prescient-mode +1))

;;;; Orderless
;; ordering of narrowed candidates
(use-package orderless
  :straight t
  :after selectrum
  :custom-face
  (orderless-match-face-0 ((t (:weight bold :foreground "#873AB7"))))
  (orderless-match-face-1 ((t (:weight bold :foreground "#D08770"))))
  (orderless-match-face-2 ((t (:weight bold :foreground "#A3BE8C"))))
  (orderless-match-face-3 ((t (:weight bold :foreground "#FFAB91"))))
  :config
  (setq completion-styles '(orderless))
  (setq orderless-skip-highlighting (lambda () selectrum-is-active)))


;;;; Embark
;; Actions on narrowed candidates
(use-package embark
  :straight (embark :type git :host github :repo "oantolin/embark")
  ;; :after (icomplete-vertical)
  :commands (embark-act embark-keymap-help)
  :bind ("C-o" . embark-act)
  :general
  (:keymaps 'minibuffer-local-completion-map
   "C-;" 'embark-act-noexit
   "C-:" 'embark-act
   "C-J" 'embark-switch-to-live-occur
   "M-q" 'embark-occur-toggle-view)
  (:keymaps 'completion-list-mode-map
   ";"  'embark-act)
  (:keymaps 'embark-file-map
   "x"  'consult-file-externally)
  :config
  (add-to-list 'embark-allow-edit-commands 'consult-imenu)
  ;; use which key for commands
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
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

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

;;;; Marginalia
;; Enable richer annotations using the Marginalia package
;; Info about candidates pulled from metadata
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  ;; :after icomplete-vertical
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
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle' to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;;;; Consult
;; Example configuration for Consult
;; Useful functions; drop-in replacement for ivy/swiper

(use-package consult
  :straight (consult :type git :host github :repo "minad/consult")
  :commands (consult-buffer consult-find consult-apropos consult-yank-pop)
  :init
  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)
  :config
  (setq consult-locate-command "mdfind -name ARG OPTS")
  ;; Optionally configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-async-min-input 0)
  (setq consult-async-input-debounce 0.01)
  )

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




;;; Buffer Completion
;;;; Company
(use-package company
  :after evil
  :hook ((markdown-mode org-mode prog-mode) . global-company-mode)
  :general
  (:keymaps 'company-active-map
   "C-/"   #'company-search-candidates
   "C-M-/" #'company-filter-candidates
   "C-d"   #'company-show-doc-buffer
   "C-j"   #'company-select-next
   "C-k"   #'company-select-previous
   "C-l"   #'company-complete-selection)
  :init
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-show-numbers t)
  :config
  ;; Default backends
  (add-to-list 'company-backends 'company-keywords)
  (add-to-list 'company-backends 'company-files))

(use-package company-bibtex
  :after (:any (:all company org) (:all company markdown))
  :demand t
  :general
  (:states 'insert
   "<C-tab>" #'company-bibtex)
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography "~/Dropbox/Work/bibfile.bib")
  (setq company-bibtex-org-citation-regex "-?cite:"))

(use-package company-prescient
  :after company
  :demand t
  :config
  (company-prescient-mode t))

;;;; Yasnippet
;; the official snippet collection https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets  :after yasnippet :demand t)
(use-package yasnippet
  ;; :hook (after-init . yas-global-mode)
  :defer 2
  :config
  ;; snippet directory
  (setq-default yas-snippet-dirs '("~/.emacs.d/.local/snippets/cpm-snippets"
                                   yasnippet-snippets-dir))
  ;; see https://emacs.stackexchange.com/a/30150/11934
  (defun cpm/yas-org-mode-hook ()
    (setq-local yas-buffer-local-condition
                '(not (org-in-src-block-p t))))
  (add-hook 'org-mode-hook #'cpm/yas-org-mode-hook)
  (yas-global-mode)
  ;; Adding yasnippet support to company
  (with-eval-after-load 'company-mode
    (add-to-list 'company-backends '(company-yasnippet)))
  (yas-reload-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; End Completion
(provide 'setup-completion)
