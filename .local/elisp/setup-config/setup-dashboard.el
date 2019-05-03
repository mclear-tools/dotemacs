;;;;; Dashboard
;; dashboard dependency
(use-package page-break-lines
  :defer t
  :diminish "")

(use-package dashboard
 :commands (dashboard-insert-startupify-lists cpm/dashboard)
 :if (< (length command-line-args) 2)
 :preface
 ;; from https://www.reddit.com/r/emacs/comments/8jaflq/tip_how_to_use_your_dashboard_properly/

 (defun cpm/dashboard-banner ()
   "Sets a dashboard banner including information on package initialization
 time and garbage collections."
   (setq dashboard-banner-logo-title
      (format "Emacs ready in %.2f seconds with %d garbage collections."
              (float-time
               (time-subtract after-init-time before-init-time)) gcs-done)))
  :init
    ;; (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (add-hook 'dashboard-mode-hook 'cpm/dashboard-banner)
  :config
  (dashboard-setup-startup-hook)
  ;; (set-frame-name "Dashboard")
  (setq dashboard-startup-banner "~/.emacs.d/.local/icons/64x64@2x.png")
  (evil-set-initial-state 'dashboard-mode 'motion)
  (setq dashboard-center-content t)
  (setq dashboard-items '((agenda . 5)
                          (recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)))
  (map! (:map dashboard-mode-map
    :ni     "TAB" 'widget-forward
    :ni     "C-i" 'widget-forward
    :ni     "backtab" 'widget-backward
    :ni     "RET" 'widget-button-press
    :ni     "down-mouse-1" 'widget-button-click
    :ni     "g" #'dashboard-insert-startupify-lists
    :ni     "a" (dashboard-insert-shortcut "a" "Agenda for today:")
    :ni     "r" (dashboard-insert-shortcut "r" "Recent Files:")
    :ni     "m" (dashboard-insert-shortcut "m" "Bookmarks:")
    :ni     "p" (dashboard-insert-shortcut "p" "Projects:"))))

;; functions to call dashboard when it kas been killed or not loaded
(defun cpm/dashboard ()
 "load dashboard and swith to buffer"
(interactive)
(let ((buffer "*dashboard*"))
  (when (not (get-buffer buffer))
    (dashboard-insert-startupify-lists))
  (switch-to-buffer buffer)))

(defun goto-dashboard ()
  "goto the dashboard"
  (interactive)
  (switch-to-buffer "*dashboard*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-dashboard)
