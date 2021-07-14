;;; Load Init
(load-file (concat user-emacs-directory "init.el"))
;;; Update Packages

(straight-x-fetch-all)
(switch-to-buffer "*straight*")
(delete-other-windows)
(straight-merge-all)
(view-echo-area-messages)
(straight-check-all)
(kill-emacs)
