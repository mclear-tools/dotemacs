;;;; Dired
;; I used to use ranger but it was buggy and I can get almost everything I want from
;; dired. See https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer for
;; discussion of how to avoid creating lots of dired buffers.
(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-jump-other-window)
  :general
  (:keymaps 'dired-mode-map
   :states '(normal motion)
   "l" #'dired-find-alternate-file
   "h" #'cpm/dired-updirectory
   "q" #'quit-window)
  :config
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  ;; list directories first
  (setq dired-listing-switches "-laGh1v --group-directories-first")
  ;; don't ask about killing buffer visiting file
  (setq dired-clean-confirm-killing-deleted-buffers t)
  ;; stop asking about recurisve actions
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  )

(defun cpm/dired-updirectory ()
  (interactive)
  (find-alternate-file ".."))

;; I used thism mainly for getting rid of unnecesary dired buffers, but I think I have that solved independently now
(use-package dired+
  :disabled t
  :ensure t
  :defer 2
  :init
  (setq font-lock-maximum-decoration nil)
  (setq dired-omit-files-regexp "^\\.?#\\|^\\.$\\|^\\.\\.$")
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-toggle-find-file-reuse-dir 1))

(use-package peep-dired
  :ensure t
  :commands (peep-dired)
  :general
  (:keymaps 'dired-mode-map
   :states '(normal motion)
   "p" #'peep-dired)
  (:keymaps 'peep-dired-mode-map
   :states '(normal)
   "j" #'peep-dired-next-file
   "k" #'peep-dired-prev-file
   "TAB" #'cpm/peep-dired-open)
  :config
  (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "pdf" "gif")
        peep-dired-max-size 5242880))

;; helper function for opening files in full window
(defun cpm/peep-dired-open ()
"open files from peep-dired & clean-up"
  (interactive)
  (peep-dired-kill-buffers-without-window)
  (dired-find-file)
  (delete-other-windows))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-dired)
