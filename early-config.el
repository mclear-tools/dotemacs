;;; early-config.el --- User config for early init   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear
;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: tools, convenience

;; Make a clean & minimalist frame
;; Better to do this in early-init so that you don't get frame resizing at startup
(setq-default initial-frame-alist
              (append (list
                       '(fullscreen . maximized)
                       ;; '(width . 175)
                       ;; '(height . 60)
                       ;; Don't flash startup background
                       (if (string= (shell-command-to-string "dark-mode status") "on\n")
                           '(background-color . "#141414")
                         '(background-color . "#FFFEFD"))
                       '(internal-border-width . 12)
                       '(tool-bar-lines . 0)
                       '(vertical-scroll-bars . nil)
                       '(horizontal-scroll-bars . nil))))
(setq-default default-frame-alist
              (append (list
                       '(frame-title-format . nil)
                       '(internal-border-width . 12)
                       '(tool-bar-lines . 0)
                       '(vertical-scroll-bars . nil)
                       '(horizontal-scroll-bars . nil)
                       )))

;; Resize pixel-wise to avoid gaps
(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)

;; Don't show icon in frame
(setq-default ns-use-proxy-icon nil)

;; No modeline at startup
(setq mode-line-format nil)


;;; early-config.el ends here
