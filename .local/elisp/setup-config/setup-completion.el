;;; Completion
(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)
         (text-mode . company-mode))
  :custom-face
  ;; Nicer looking faces
  (company-tooltip-common
    ((t (:inherit company-tooltip :weight bold :underline nil))))
  (company-tooltip-common-selection
    ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
  :init
  (setq company-idle-delay 0.35
        company-minimum-prefix-length 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  :config
  ;; Default backends
  (add-to-list 'company-backends 'company-files)
  ;; key bindings
  (let ((map company-active-map))
   (define-key map (kbd "TAB") 'company-complete-selection)
   (define-key map (kbd "C-/") 'company-search-candidates)
   (define-key map (kbd "C-M-/") 'company-filter-candidates)
   (define-key map (kbd "C-d") 'company-show-doc-buffer)
   (define-key map (kbd "C-j") 'company-select-next)
   (define-key map (kbd "C-k") 'company-select-previous)
   (define-key map (kbd "C-l") 'company-complete-selection))
)

(use-package company-bibtex
  :ensure t
  :after company
  :demand t
  :hook (text-mode . company-bibtex)
  :config
  (setq company-bibtex-bibliography "~/Dropbox/Work/bibfile.bib")
  (setq company-bibtex-org-citation-regex "-?@")
  (add-to-list 'company-backends 'company-bibtex))

  (use-package yasnippet
    :hook ((prog-mode text-mode) . yas-minor-mode)
    :commands (yas-expand yas-minor-mode)
    :diminish (yas-minor-mode . " Ⓨ")
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
    (yas-reload-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-completion)
