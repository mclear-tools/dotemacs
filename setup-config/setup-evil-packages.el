;; Evil Packages

;;; Evil Collection
(use-package evil-collection
  :ensure t
  :after evil
  :hook (evil-after-load-hook . evil-collection-init)
  :custom (evil-collection-company-use-tng nil))

;;; Evil Indent
(use-package evil-indent-textobject :commands (evil-indent))

;;; Evil Surround
(use-package evil-surround
  :defer 1
  :commands (evil-surround-region evil-surround-change evil-surround-delete)
  :general
  (:states '(visual)
   "s" 'evil-surround-region
   "S" 'evil-substitute)
  :config (global-evil-surround-mode 1))

(use-package evil-embrace
  :after evil-surround
  :demand t
  :hook ((markdown org) . embrace-org-mode-hook)
  :config
  (with-eval-after-load 'evil-surround
    (evil-embrace-enable-evil-surround-integration))
  (setq evil-embrace-show-help-p nil)
  ;; (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (defun embrace-markdown-mode-hook ()
    (dolist (lst '((?* "*" . "*")
                   (?\ "\\" . "\\")
                   (?$ "$" . "$")
                   (?/ "/" . "/")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
  (add-hook 'markdown-mode-hook 'embrace-markdown-mode-hook))

;;; Evil Comments
(use-package evil-commentary
  :commands (evil-commentary evil-commentary-line)
  :config
  (evil-commentary-mode))

;;; Evil Undo
(use-package undo-tree
  :commands (undo-tree-undo undo-tree-redo undo-tree-visualize)
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-enable-undo-in-region nil)
  ;; supposedly causes errors in undo read
  ;; see https://emacs.stackexchange.com/a/34214/11934
  (setq undo-tree-enable-undo-in-region nil)
  ;; stop littering - set undo directory
  (let ((undo-dir (concat cpm-cache-dir "undo")))
    (setq undo-tree-history-directory-alist `(("." . ,undo-dir)))
    (unless (file-directory-p undo-dir)
      (make-directory undo-dir t)))
  (setq undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))


;;; Evil Multi-Cursor
(use-package evil-visualstar
  :commands (evil-visualstar/begin-search-forward evil-visualstar/begin-search-backward))

(use-package evil-multiedit
  :ensure t
  :after evil-visualstar
  :config
  ;; Default keybindings
  ;; Highlights all matches of the selection in the buffer.
  (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

  ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
  ;; incrementally add the next unmatched match.
  (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
  ;; Match selected region.
  (define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-and-next)
  ;; Insert marker at point
  (define-key evil-insert-state-map (kbd "M-d") 'evil-multiedit-toggle-marker-here)

  ;; Same as M-d but in reverse.
  (define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
  (define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-and-prev)

  ;; OPTIONAL: If you prefer to grab symbols rather than words, use
  ;; `evil-multiedit-match-symbol-and-next` (or prev).

  ;; Restore the last group of multiedit regions.
  (define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)

  ;; RET will toggle the region under the cursor
  (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

  ;; ...and in visual mode, RET will disable all fields outside the selected region
  (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

  ;; For moving between edit regions
  (define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
  (define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
  (define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
  (define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)

  ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match))

(use-package evil-mc
  :ensure t
  :commands (evil-mc-make-all-cursors evil-mc-make-and-goto-next-match))


;;; Evil Numbers
(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (general-define-key
   :states '(normal visual insert emacs)
   "H-s" 'evil-numbers/inc-at-pt
   "H-a" 'evil-numbers/dec-at-pt))

;;; End Evil-Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-evil-packages)
