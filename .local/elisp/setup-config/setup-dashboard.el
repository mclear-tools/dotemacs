;;; Dashboard 
;; dashboard dependency
(use-package page-break-lines
  :defer t
  :diminish ""
  :config
  (set-fontset-font "fontset-default"
                  (cons page-break-lines-char page-break-lines-char)
                  (face-attribute 'default :family)))

(use-package dashboard
  :demand t
  :if (< (length command-line-args) 2)
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
  :custom-face
  (dashboard-heading ((t (:inherit font-lock-variable-name-face))))
  :config
  (setq dashboard-startup-banner (concat cpm-local-dir "icons/128x128@2x.png"))
  (setq dashboard-banner-logo-title "Sapere aude")
  (setq dashboard-center-content t)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-footer t)
  (setq dashboard-footer-icon (all-the-icons-fileicon "emacs"
                                                      :height 1.1
                                                      :v-adjust -0.05
                                                      :face 'font-lock-string-face))

  ;; add icons
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t)
  (setq dashboard-items '((bookmarks . 5)
                          (recents  . 8)
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

;; Open projects in new frames
(defun dashboard-insert-projects (list-size)
  "Add the list of LIST-SIZE items of projects."
  (require 'projectile)
  (projectile-load-known-projects)
  (dashboard-insert-section
   "Projects:"
   (dashboard-subseq (projectile-relevant-known-projects)
                     0 list-size)
   list-size
   "p"
   `(lambda (&rest ignore)
        (let ((buffer (generate-new-buffer "untitled")))
  (set-buffer-major-mode buffer)
  (display-buffer buffer '(display-buffer-pop-up-frame . nil)))
  (crux-create-scratch-buffer)
  (projectile-switch-project-by-name ,el)
  (setq frame-title-format
    '(""
      "%b"
      (:eval
       (let ((project-name (projectile-project-name)))
         (unless (string= "-" project-name)
           (format " in [%s]" project-name)))))))
(abbreviate-file-name el)))

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
