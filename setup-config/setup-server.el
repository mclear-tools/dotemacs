;;; Server
;; start server for emacsclient
(use-package server
  :if window-system
  :defer 2
  :config
  ;; avoid warning screen
  (or (server-running-p)
      (server-start))
  ;; no instructions
  (setq server-client-instructions nil))
;; generates warning on startup of another emacs instance
;; :hook (after-init . server-mode))

;; have these functions available for server
(defun cpm/activate-capture-frame ()
  "run org-capture in capture frame"
  (require 'org)
  (select-frame-by-name "capture")
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-capture))

(defun cpm/weather-journal-capture ()
  (interactive)
  (require 'org)
  (select-frame-by-name "capture")
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (cpm/org-journal)
  (cpm/insert-weather)
  (goto-char (point-max)))

;; functions for killing server-related emacsen
(defun cpm/kill-all-emacsen ()
  (interactive)
  (progn
    (save-buffers-kill-emacs)
    (shell-command-to-string "pkill -i emacs")))

(defun cpm/kill-emacs-capture-daemon ()
  (interactive)
  (shell-command-to-string "pkill -f /Applications/Emacs.app/Contents/MacOS/emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-server)
