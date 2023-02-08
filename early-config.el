;;; early-config.el --- User config for early init   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear
;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: tools, convenience

;;; Frame & Colors
;; ;; Set colors correctly so there is no flash at startup
(cond ((string= (shell-command-to-string "dark-mode status") "on\n")
       (setq active-theme 'dark-theme))
      (t
       (setq active-theme 'light-theme)))

(setq-default initial-frame-alist
              (append (list
                       '(fullscreen . maximized)
                       `(background-color . ,(if (eq active-theme 'light-theme) "#fffefd" "#141414"))
                       '(internal-border-width . 12)
                       '(tool-bar-lines . 0)
                       '(vertical-scroll-bars . nil)
                       '(horizontal-scroll-bars . nil)
                       '(undecorated . nil))))

;; No modeline at startup
(setopt mode-line-format nil)
(setopt header-line-format nil)

;;;; Package settings
(setopt lem-package-ensure-packages nil)

;;; early-config.el ends here
