;;; Dashboard 
;; dashboard dependency
(use-package page-break-lines
  :defer t
  :diminish "")

(use-package dashboard
  :demand t
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
  (add-hook 'dashboard-mode-hook 'cpm/dashboard-banner)
  :general
  (:states '(normal motion emacs)
   :keymaps 'dashboard-mode-map
      "TAB" 'widget-forward
      "C-i" 'widget-forward
      "<backtab>" 'widget-backward
      "RET" 'widget-button-press
      "<down-mouse-1>" 'widget-button-click
      "g" #'dashboard-insert-startupify-lists
      "a" (dashboard-insert-shortcut "a" "Agenda for today:")
      "r" (dashboard-insert-shortcut "r" "Recent Files:")
      "m" (dashboard-insert-shortcut "m" "Bookmarks:")
      "p" (dashboard-insert-shortcut "p" "Projects:"))
  :config
  (setq dashboard-startup-banner (concat cpm-local-dir "icons/128x128@2x.png"))
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)))
  (dashboard-setup-startup-hook))

;; don't use imagemagick to create banner as it is notably worse in image quality
(defun dashboard-insert-image-banner (banner)
  "Display an image BANNER."
  (when (file-exists-p banner)
    (let* ((title dashboard-banner-logo-title)
           (spec
              (create-image banner))
           (size (image-size spec))
           (width (car size))
           (left-margin (max 0 (floor (- dashboard-banner-length width) 2))))
      (goto-char (point-min))
      (insert "\n")
      (insert (make-string left-margin ?\ ))
      (insert-image spec)
      (insert "\n\n")
      (when title
        (insert (make-string (max 0 (floor (/ (- dashboard-banner-length
                                                 (+ (length title) 1)) 2))) ?\ ))
        (insert (format "%s\n\n" (propertize title 'face 'dashboard-banner-logo-title)))))))

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
