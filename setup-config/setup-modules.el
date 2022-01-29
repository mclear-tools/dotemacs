;; Setup config modules and helper functions

;;; Load Modules
;; Load all the setup modules

;;;; Core Modules
;; These are the "can't live without" modules
(defun cpm--config-core ()
  (progn
    (require 'setup-libraries)
    (require 'setup-functions-macros)
    (require 'setup-keybindings)
    (require 'setup-modal)
    (require 'setup-settings)))

  (if (cpm--emacs-switches "-core")
      (progn
        (message "Loading core modules only")
        (cpm--config-core)
        ;; start with built in vertical completion
        (require 'setup-icomplete))
    ;; Otherwise load entire config
  (progn
    (cpm--config-core)

;;;; Other Modules
    (require 'setup-splash)
    (require 'setup-theme)
    (require 'setup-modeline)
    (require 'setup-server)
    (require 'setup-org)
    (require 'setup-org-extensions)
    (require 'setup-dired)
    (require 'setup-completion)
    (require 'setup-osx)
    (require 'setup-windows-buffers)
    (require 'setup-ui)
    (require 'setup-navigation)
    (require 'setup-search)
    (require 'setup-vc)
    (require 'setup-projects)
    (require 'setup-teaching)
    (require 'setup-childframe)
    (require 'setup-shell)
    (require 'setup-writing)
    (require 'setup-notes)
    (require 'setup-citation)
    (require 'setup-programming)
    (require 'setup-pdf)
    (require 'setup-calendars)
    (require 'setup-testing)
    ))

;;; Provide
(provide 'setup-modules)

;;; Setup Config Modules ends here
