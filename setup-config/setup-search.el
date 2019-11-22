;;; Search
(use-package ag
  :config
  (progn
    (defun ag/jump-to-result-if-only-one-match ()
      "Jump to the first ag result if that ag search came up with just one match."
      (let (only-one-match)
        (when (member "--stats" ag-arguments)
          (save-excursion
            (goto-char (point-min))
            (setq only-one-match (re-search-forward "^1 matches\\s-*$" nil :noerror)))
          (when only-one-match
            (next-error)
            (kill-buffer (current-buffer))
            (message (concat "ag: Jumping to the only found match and "
                             "killing the *ag* buffer."))))))
    (add-hook 'ag-search-finished-hook #'ag/jump-to-result-if-only-one-match)

    ;; Set default ag arguments
    ;; It looks like the ~/.agignore is used when launching ag from emacs too.
    ;; So the ignores from ~/.agignore don't have to be set here again.
    (setq ag-highlight-search t)
    ;; By default, ag.el will open results in a different window in the frame, so
    ;; the results buffer is still visible. You can override this so the results
    ;; buffer is hidden and the selected result is shown in its place:
    (setq ag-reuse-window nil)
    ;; reuse the same *ag* buffer for all your searches
    (setq ag-reuse-buffers t)
    ;; ;; To save buffer automatically when `wgrep-finish-edit'
    ;; (setq wgrep-auto-save-buffer t)
    (with-eval-after-load 'projectile
      ;; Override the default function to use the projectile function instead
      (defun ag/project-root (file-path)
        (let ((proj-name (projectile-project-root)))
          (if proj-name
              proj-name ; return `projectile-project-root' if non-nil
            ;; Else condition is same as the `ag/project-root' definition
            ;; from ag.el
            (if ag-project-root-function
                (funcall ag-project-root-function file-path)
              (or (ag/longest-string
                   (vc-git-root file-path)
                   (vc-svn-root file-path)
                   (vc-hg-root file-path))
                  file-path))))))))

;;;; Deadgrep
(use-package deadgrep
  :ensure t
  :general
  (:states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "sg" #'deadgrep))
  ;; (:states '(normal motion visal)
  ;;  :keymaps 'deadgrep-mode-map

  ;;  "j" #'deadgrep-forward
  ;;  "k" #'deadgrep-backward
  ;;  "l" #'deadgrep-visit-result))

;;;; Ripgrep
(use-package rg :commands rg)

;;;; Search Notes
(defvar cpm-notes-dir "~/Dropbox/Notes")
(defun cpm/search-all-notes ()
  (interactive)
  (cd cpm-notes-dir)
  (call-interactively #'deadgrep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-search)
