;;;; Ivy
(use-package ivy
  :bind
  (:map ivy-mode-map
   ("M-o"  . ivy-dispatching-done)
   ("C-j"  . ivy-next-line)
   ("C-k"  . ivy-previous-line)
   ("C-<SPC>" . cpm/ivy-toggle-mark))
  (:map ivy-switch-buffer-map
   ("C-d" . ivy-switch-buffer-kill))
  :config
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

;;;; Ivy & Tabspaces
(defun tabspaces-ivy-switch-buffer (buffer &optional norecord force-same-window)
  "Display the local buffer BUFFER in the selected window.
This is the frame/tab-local equivalent to `switch-to-buffer'."
  (interactive
   (list
    (let ((blst (mapcar #'buffer-name (tabspaces-buffer-list))))
      (read-buffer
       "Switch to local buffer: " blst nil
       (lambda (b) (member (if (stringp b) b (car b)) blst))))))
  (ivy-switch-buffer buffer))

;;;; Counsel
(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :defines
  (projectile-completion-system magit-completing-read-function)
  :commands (counsel-projectile counsel-org-goto jump-in-buffer counsel-org-tag)
  :bind
  (:map counsel-mode-map
   ("C-j"     . ivy-next-line)
   ("C-k"     . ivy-previous-line)
   ("C-h l"    . counsel-find-library)
   ("C-h C-l" . view-lossage))
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



;;;; End Ivy Config
(provide 'cpm-setup-ivy)
