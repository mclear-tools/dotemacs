;;; early-config.el --- User config for early init   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear
;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: tools, convenience

;; ;; Set colors correctly so there is no flash at startup
;; (cond ((string= (shell-command-to-string "dark-mode status") "on\n")
;;        (push '(background-color . "#141414") initial-frame-alist)
;;        (setq active-theme 'dark-theme))
;;       (t
;;        (push '(background-color . "#FFFEFD") initial-frame-alist)
;;        (setq active-theme 'light-theme)))

(setq-default initial-frame-alist
              (append (list
                       '(internal-border-width . 12)
                       '(tool-bar-lines . 0)
                       '(vertical-scroll-bars . nil)
                       '(horizontal-scroll-bars . nil)
                       '(undecorated . t))))

;; Resize pixel-wise to avoid gaps
(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)

;; Don't show icon in frame
(setq-default ns-use-proxy-icon nil)

;; No modeline at startup
(setq mode-line-format nil)
(setq header-line-format nil)


;;; early-config.el ends here
