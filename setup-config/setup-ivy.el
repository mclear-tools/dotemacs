;;;; Ivy
(use-package ivy
  :general
  (:keymaps 'ivy-mode-map
   "M-o" 'ivy-dispatching-done
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line
   "C-<SPC>" 'cpm/ivy-toggle-mark)
  (:keymaps 'ivy-switch-buffer-map
   "C-d" 'ivy-switch-buffer-kill)
  :defines (projectile-completion-system
            magit-completing-read-function)
  :commands (ivy--format-function-generic
             ivy--add-face)
  :hook ((after-init . ivy-mode))
  :config
  ;; fixes for use with help/helpful
  ;; https://github.com/Wilfred/helpful/issues/218#issue-476517687
  (ivy-set-actions
   'counsel-M-x
   `(("d" counsel--find-symbol "definition")
     ("h" ,(lambda (x) (helpful-callable (intern x))) "help")))
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-height 15  ; Number of result lines to display
        ivy-count-format "(%d/%d) " ;; Display count displayed and total
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil ; No regexp by default
        ivy-wrap t ; wrap candidates
        )
  (defun cpm/ivy-toggle-mark ()
    "Toggle mark for current candidate and move forwards."
    (interactive)
    (if (ivy--marked-p)
        (ivy-unmark)
      (ivy-mark))))

(use-package ivy-hydra
  :requires (ivy)
  :after ivy
  :demand t)

;;;; Ivy-rich
;; More friendly display transformer for Ivy
(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode)
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-path-style 'abbrev)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; Better experience with icons
;; Enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :if window-system
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :init
  (setq all-the-icons-ivy-rich-icon-size 0.85))

;; for some reason all-the-icons-ivy is clobbering my settings for ivy-rich
;; below fixes that
(with-eval-after-load 'all-the-icons-ivy-rich
  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'counsel-M-x
             '(:columns
               ((all-the-icons-ivy-rich-function-icon)
                (counsel-M-x-transformer (:width 50))
                (ivy-rich-counsel-function-docstring (:face font-lock-doc-face :width 60)))))
  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'counsel-describe-function
             '(:columns
               ((all-the-icons-ivy-rich-function-icon)
                (counsel-describe-function-transformer (:width 50))
                (ivy-rich-counsel-function-docstring (:face font-lock-doc-face :width 60)))))
  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'counsel-describe-variable
             '(:columns
               ((all-the-icons-ivy-rich-variable-icon)
                (counsel-describe-variable-transformer (:width 50))
                (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face :width 60)))))
  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'counsel-set-variable
             '(:columns
               ((all-the-icons-ivy-rich-variable-icon)
                (counsel-describe-variable-transformer (:width 50))
                (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face :width 60))))))






;;;; Counsel
(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :defines
  (projectile-completion-system magit-completing-read-function)
  :commands (counsel-projectile counsel-org-goto jump-in-buffer counsel-org-tag)
  :general
  (:keymaps 'counsel-mode-map
   "C-j" #'ivy-next-line
   "C-k" #'ivy-previous-line)
  ("C-h l" #'counsel-find-library
   "C-h C-l" #'view-lossage)
  :init
  ;; Use faster search tool: ripgrep (rg)
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S -M 120 -L -i --line-number --no-heading --color never %s %s")
    (setq counsel-rg-base-command "rg -S -M 120 -L -i --with-filename --no-heading --line-number --color never %s"))
  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")
  :config
  (setq counsel-find-file-ignore-regexp "\\.elc\\'")
  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (add-to-list 'ivy-format-functions-alist '(counsel-describe-face . counsel--faces-format-function))
  ;; Add action to open file literally
  (ivy-add-actions 'counsel-find-file
                   `(("l" find-file-literally "Open literally")))
  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))
  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read)))



;;;; Mcfly Search
;; Pre-fill search keywords
;; @see https://www.reddit.com/r/emacs/comments/b7    g1px/withemacs_execute_commands_like_marty_mcfly/
(defvar my-ivy-fly-commands '(query-replace-regexp
                              flush-lines
                              keep-lines
                              ivy-read
                              swiper
                              swiper-backward
                              swiper-all
                              swiper-isearch
                              swiper-isearch-backward
                              counsel-grep-or-swiper
                              counsel-grep-or-swiper-backward
                              counsel-grep
                              counsel-ack
                              counsel-ag
                              counsel-rg
                              counsel-pt))

(defun my-ivy-fly-back-to-present ()
  ;; (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)
  (cond ((and (memq last-command my-ivy-fly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((or (memq this-command '(self-insert-command
                                  yank
                                  ivy-yank-word
                                  counsel-yank-pop))
             (equal (this-command-keys-vector) (kbd "M-n")))
         (delete-region (point)
                        (point-max)))))

(defun my-ivy-fly-time-travel ()
  (when (memq this-command my-ivy-fly-commands)
    (let* ((kbd (kbd "M-n"))
           (cmd (key-binding kbd))
           (future (and cmd
                        (with-temp-buffer
                          (when (ignore-errors
                                  (call-interactively cmd) t)
                            (buffer-string))))))
      (when future
        (save-excursion
          (insert (propertize (replace-regexp-in-string
                               "\\\\_<" ""
                               (replace-regexp-in-string
                                "\\\\_>" ""
                                future))
                              'face 'shadow)))
        (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

(add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)

;;;; Swiper
(use-package swiper
  :commands (swiper swiper-all)
  :config
  ;; Improve search experience of `swiper'
  ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
  (defun my-swiper-toggle-counsel-rg ()
    "Toggle `counsel-rg' with current swiper input."
    (interactive)
    (let ((text (replace-regexp-in-string
                 "\n" ""
                 (replace-regexp-in-string
                  "\\\\_<" ""
                  (replace-regexp-in-string
                   "\\\\_>" ""
                   (replace-regexp-in-string "^.*Swiper: " ""
                                             (thing-at-point 'line t)))))))
      (ivy-quit-and-run
        (counsel-rg text default-directory))))
  (general-def "<C-return>" #'my-swiper-toggle-counsel-rg :keymaps 'swiper-map)

  (with-eval-after-load 'rg
    (defun my-swiper-toggle-rg-dwim ()
      "Toggle `rg-dwim' with current swiper input."
      (interactive)
      (ivy-quit-and-run (rg-dwim default-directory)))
    (general-def "<M-return>" #'my-swiper-toggle-rg-dwim :keymaps 'swiper-map)
    (general-def "<M-return>" #'my-swiper-toggle-rg-dwim :keymaps: 'ivy-minibuffer-map)))

;;;; Supporting Packages
;; Enhance M-x
(use-package amx
  :hook (ivy-mode . amx-mode)
  :commands amx-mode
  :config
  (setq amx-history-length 20)
  (setq amx-save-file (concat cpm-cache-dir "amx-items")))

;; Better sorting and filtering
(use-package prescient
  :commands prescient-persist-mode
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode 1)
  (setq prescient-save-file (concat cpm-cache-dir "prescient-save.el")))

(use-package ivy-prescient
  :commands (ivy-prescient-mode ivy-prescient-re-builder)
  :hook (counsel-mode . ivy-prescient-mode)
  :custom-face (ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
  :preface
  (defun ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))
  :hook (ivy-mode . ivy-prescient-mode)
  :config
  (setq ivy-prescient-retain-classic-highlighting t
        ivy-re-builders-alist '((counsel-ag . ivy-prescient-non-fuzzy)
                                (counsel-rg . ivy-prescient-non-fuzzy)
                                (counsel-pt . ivy-prescient-non-fuzzy)
                                (counsel-grep . ivy-prescient-non-fuzzy)
                                (counsel-yank-pop . ivy-prescient-non-fuzzy)
                                (swiper . ivy-prescient-non-fuzzy)
                                (swiper-isearch . ivy-prescient-non-fuzzy)
                                (swiper-all . ivy-prescient-non-fuzzy)
                                (t . ivy-prescient-re-builder))))

;; :config
;; (ivy-prescient-mode t)
;; ;; Explicitly disable sorting by ivy-prescient for these commands.
;; ;; See https://github.com/raxod502/prescient.el/issues/38
;; (general-add-hook 'ivy-sort-functions-alist
;;                   '((swiper . nil)
;;                     (counsel-minibuffer-history . nil)
;;                     (counsel-mark-ring . nil)))
;; ;; Don't fallback to ivy-prescient.
;; (general-remove-hook 'ivy-sort-functions-alist
;;                      '((t . ivy-prescient-sort-function))))




;; Counsel integration for Projectile
(use-package counsel-projectile
  :hook (projectile . counsel-projectile-mode)
  :commands (counsel-projectile-mode
             counsel-projectile
             counsel-projectile-switch-project
             counsel-projectile-find-file
             counsel-projectile-find-file-dwim
             counsel-projectile-find-dir
             counsel-projectile-switch-to-buffer
             counsel-projectile-bookmark)
  :init
  (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
  :config
  (setq projectile-known-projects-file
        (concat cpm-cache-dir "projectile-bookmarks.eld")))


;; Integrate yasnippet
(use-package ivy-yasnippet
  :commands ivy-yasnippet--preview
  :general ("C-c C-y"  'ivy-yasnippet)
  :config (advice-add #'ivy-yasnippet--preview :override #'ignore))

;; Select from xref candidates with Ivy
(use-package ivy-xref
  :init
  (when (boundp 'xref-show-definitions-function)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; Correcting words with flyspell via Ivy
(use-package flyspell-correct-ivy
  :after flyspell
  :general (:keymaps 'flyspell-mode-map
            [remap flyspell-correct-word-before-point]  'flyspell-correct-previous-word-generic))

;; Quick launch apps
(use-package counsel-osx-app
  :general (:keymaps 'counsel-mode-map
            "s-<f6>" 'counsel-osx-app))

;; Display world clock using Ivy
(use-package counsel-world-clock
  :general (:keymaps 'counsel-mode-map
            "C-c c k"  'counsel-world-clock))

;; Tramp ivy interface
(use-package counsel-tramp
  :general (:keymaps 'counsel-mode-map
            "C-c c v" 'counsel-tramp))


;;;; End Ivy Config
(provide 'setup-ivy)
