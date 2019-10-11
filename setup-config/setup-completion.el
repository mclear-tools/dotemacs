;;; Completion
(use-package company
  :ensure t
  :after evil
  :hook ((markdown-mode org-mode prog-mode) . global-company-mode)
  :custom-face
  ;; Nicer looking faces
  (company-tooltip-common
   ((t (:inherit company-tooltip :weight bold :underline nil))))
  (company-tooltip-common-selection
   ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
  ;; ;; key bindings
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
        company-dabbrev-downcase nil)
  :config
  ;; Default backends
  (add-to-list 'company-backends 'company-keywords)
  (add-to-list 'company-backends 'company-files))


(use-package company-bibtex
  :ensure t
  :after company
  :demand t
  :general
  (:states 'insert
   "<C-tab>" #'company-bibtex)
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography "~/Dropbox/Work/bibfile.bib")
  (setq company-bibtex-org-citation-regex "-?@"))

(use-package yasnippet
  :defer 1
  :config
  ;; see https://emacs.stackexchange.com/a/30150/11934
  (defun cpm/yas-org-mode-hook ()
    (setq-local yas-buffer-local-condition
            '(not (org-in-src-block-p t))))
  (add-hook 'org-mode-hook #'cpm/yas-org-mode-hook)

  ;; snippet directory
  (setq yas-snippet-dirs '("~/.emacs.d/.local/snippets/cpm-snippets"
                           yasnippet-snippets-dir))
  ;; the official snippet collection https://github.com/AndreaCrotti/yasnippet-snippets
  (use-package yasnippet-snippets :ensure t :after yasnippet :demand t)

  ;; Adding yasnippet support to company
  (with-eval-after-load 'company-mode
  (add-to-list 'company-backends '(company-yasnippet)))
  (yas-reload-all)
  (yas-global-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-completion)
