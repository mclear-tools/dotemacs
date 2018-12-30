;; ZETTELKASTEN
;; a minor mode for making working with deft the ultimate zettelkasten
;; experience

(require 's)
(require 'dash)
(require 'thingatpt)

;; file to record Zettel statistics (currently only growth of the Zettelkasten)
(setq zk-stats-file (concat deft-directory "/000000000000 Zettel-Stats.txt"))
(setq zetteltags-file "~/.emacs.d/zetteltags")

(setq zk-position-list ())
(setq zk-buffer-list ())
(setq zk-jump-position 0)

(defun zk-push-current-pos ()
  (interactive)
  (push (point) zk-position-list)
  (push (buffer-name) zk-buffer-list)
  )

(defun zk-save-current-position ()
  (interactive)
  (zk-push-current-pos)
  (message "Saved current position.")
  )

(defun zk-jump-back ()
  (interactive)
  (setq zk-jump-position (+ zk-jump-position 1))
  (switch-to-buffer (nth zk-jump-position zk-buffer-list))
  (goto-char (nth zk-jump-position zk-position-list))
  )

(defun zk-jump-forward ()
  (interactive)
  (setq zk-jump-position (- zk-jump-position 1))
  (switch-to-buffer (nth zk-jump-position zk-buffer-list))
  (goto-char (nth zk-jump-position zk-position-list))
  )

(defun zk-current-time ()
  (format-time-string "%Y%m%d%H%M")
  )

(defun zk-follow-internal-link ()
  (interactive)
  (zk-push-current-pos)
  (setq zk-search-string (word-at-point))
  (deft)
  (setq deft-filter-regexp (list zk-search-string))
  (deft-filter-update)
  (deft-refresh-browser)
  )

(defun zk-insert-timestamp ()
  (interactive)
  (insert (format-time-string "%Y%m%d%k%M"))
  )

(defun zk-new-with-timestamp ()
  (interactive)
  (setq zk-time-string (zk-current-time))
  (deft)
  (setq deft-filter-regexp (list zk-time-string))
  (deft-filter-update)
  (deft-refresh-browser)
  )

(defun zk-insert-timestamp-for-internal-link ()
  (interactive)
  (setq zk-all-dated-files (directory-files deft-directory nil "^[0-9]\\{12\\}\\.*"))
  (setq zk-link-file (ido-completing-read "Link? " zk-all-dated-files))
  (insert (concat "[[" (car (s-match "^[0-9]\\{12\\}" zk-link-file)) "]]"))
  )

(defun zk-insert-full-internal-link ()
  (interactive)
  (setq zk-all-dated-files (directory-files deft-directory nil "^[0-9]\\{12\\}\\.*"))
  (setq zk-link-file (ido-completing-read "Link? " zk-all-dated-files))
  (insert (concat "[[" (file-name-sans-extension zk-link-file) "]]"))
  )

(defun zk-insert-org-link-to-zettel ()
  (interactive)
  (setq zk-all-dated-files (directory-files deft-directory nil "^[0-9]\\{12\\}\\.*"))
  (setq zk-link-file (ido-completing-read "Link? " zk-all-dated-files))
  (insert (org-make-link-string (concat "file:" deft-directory "/" zk-link-file)
                                (concat "Zettel:" (car (s-match "^[0-9]\\{12\\}" zk-link-file)))))
  )

(defun zk-get-tag-list ()
  "gets all tags from all notes in fast way"
  (interactive)
  (shell-command (concat "grep -rhE '^tags:' " deft-directory
                         " | tr -s \" \" \"\012\" | grep \"#\" | sort | uniq > "
                         zetteltags-file))
  (setq zk-tag-list (s-split "\n" (f-read zetteltags-file)))
  )

(defun zk-complete-tag ()
  "completes tags from all previously used tags"
  (interactive)
  (zk-get-tag-list)
  (insert (concat (ido-completing-read "Schlagwort? " zk-tag-list) " " )))

(defun zk-insert-tagline ()
  (interactive)
  (zk-get-tag-list)
  (backward-page)
  (open-line 2)
  (insert (concat "tags: " (ido-completing-read "Schlagwort? " zk-tag-list) " " )))

(defun zk-find-similar ()
  (interactive)
  (setq zk-this-file (file-name-base))
  (setq zk-id-this-file (car (s-match "^[0-9]\\{12\\}" zk-this-file)))
  (setq zk-similar (shell-command-to-string (concat "~/.emacs.d/R/concordance " zk-id-this-file)))
  (other-window 1)
  (switch-to-buffer-other-window "*Ähnliche Notizen*")
  (insert "Ähnliche Notizen:\n" zk-similar)
  (zk-mode)
  )

(defun zk-zettel-reference-at-point ()
  (interactive)
  (setq zk-all-dated-files (directory-files deft-directory nil "^[0-9]\\{12\\}\\.*"))
  (setq current-id (thing-at-point 'symbol))
  (message (seq-find (lambda (zettel)
                       (s-starts-with? current-id zettel))
                     zk-all-dated-files "No matching Zettel found."))
  )

(defun zk-goto-zettel-at-point ()
  (interactive)
  (zk-push-current-pos)
  (setq zk-all-dated-files (directory-files deft-directory nil "^[0-9]\\{12\\}\\.*"))
  (setq current-id (thing-at-point 'symbol))
  (setq zettel-at-point (seq-find (lambda (zettel)
                                    (s-starts-with? current-id zettel))
                                  zk-all-dated-files ""))
  (if (s-equals? "" zettel-at-point)
      (message "No matching Zettel found.")
    (find-file (concat deft-directory "/" zettel-at-point))
    )
  )

(defun zk-insert-reference-skeleton ()
  (interactive)
  (insert "---")
  (newline)
  (clipboard-yank)
  (insert "---")
  (newline)
  (insert "tags: #ref #todo")
  (newline)
  (insert "tldr:")
  (newline)
  )

(defun zk-count-zettels ()
  (length (directory-files deft-directory "\\.txt\\'"))
  )

(defun zk-write-stats ()
  (interactive)
  (append-to-file (concat (zk-current-time) " " (number-to-string (zk-count-zettels)) "\n") nil zk-stats-file)
  )

(define-minor-mode zk-mode
  "Some functionality to provide a decent Zettelkasten-Workflow for Deft."
  :lighter " zk"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c o") 'zk-follow-internal-link)
            (define-key map (kbd "C-c d") 'zk-new-with-timestamp)
            (define-key map (kbd "C-c l") 'zk-insert-timestamp-for-internal-link)
            (define-key map (kbd "C-c [") 'zk-insert-full-internal-link)
            (define-key map (kbd "C-c t") 'zk-insert-tagline)
            (define-key map (kbd "C-c #") 'zk-complete-tag)
            (define-key map (kbd "C-c s") 'zk-find-similar)
            (define-key map (kbd "C-c i") 'zk-zettel-reference-at-point)
            (define-key map (kbd "M-.")   'zk-goto-zettel-at-point)
            (define-key map (kbd "C-c _") 'zk-jump-back)
            (define-key map (kbd "C-c *") 'zk-jump-forward)
            (define-key map (kbd "C-c .") 'zk-save-current-position)
            (define-key map (kbd "C-c §") 'zk-insert-reference-skeleton)
            map)
  (auto-fill-mode)
  (font-lock-add-keywords 'markdown-mode
                          '(("\\(\\[\\[\\)\\(\\(.*\n\\)*.*\\)\\(\\]\\]\\)"
                             (1 font-lock-comment-face)
                             (2 font-lock-doc-face)
                             (3 font-lock-comment-face))
                            ("#[a-z0-9_]*" . font-lock-keyword-face)
                            ))
  )

(defun zk-minor-mode-on ()
  "Turn on `zk' mode."
  (interactive)
  (zk-mode 1))

(add-hook 'markdown-mode-hook 'zk-minor-mode-on)

;; use other face in markdown buffers
(defun zk-set-buffer-face ()
   "Sets a fixed width (monospace) font in current buffer"
   (interactive)
   (setq buffer-face-mode-face '(:family "SF Mono" :height 160))
   (buffer-face-mode))
(add-hook 'markdown-mode-hook (lambda() (zk-set-buffer-face)))

(add-hook 'deft-mode-hook (lambda () (zk-get-tag-list)))
(add-hook 'deft-mode-hook (lambda () (deft-refresh)))
(add-hook 'deft-mode-hook (lambda () (zk-write-stats)))
