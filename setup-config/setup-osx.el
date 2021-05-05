;;; OSX Settings
;;;; Clipboad
(use-package simpleclip
  :disabled t
  :defer 1
  :config
  (simpleclip-mode 1))

;; Integrate with MacOS clipboard
(setq select-enable-clipboard t)

;; Saving whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
(setq save-interprogram-paste-before-kill t)

;; Copy/Paste functions
;; https://github.com/dakrone/eos/blob/master/eos-core.org#mac-osx
(defun copy-from-osx ()
  "Handle copy/paste intelligently on osx."
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "/usr/bin/pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function 'paste-to-osx
      interprogram-paste-function 'copy-from-osx)

;; Fix for non-ascii characters
;; see https://gist.github.com/the-kenny/267162#gistcomment-2883522
(setenv "LANG" "en_US.UTF-8")

;;;; General Settings

(setq IS-LINUX (eq system-type 'gnu/linux)
      IS-MAC (eq system-type 'darwin))
(when IS-MAC
  ;; make fonts look better with anti-aliasing
  (setq mac-allow-anti-aliasing t)
  ;; delete files by moving them to the trash
  (use-package osx-trash
    :straight t
    :hook (after-init . osx-trash-setup)
    :config
    (setq delete-by-moving-to-trash t))

  ;; this might also work NOTE: not tested!
  ;; https://emacs.stackexchange.com/a/63342/11934
  (defun cpm/system-move-file-to-trash (filename)
    "Move file or directory named FILENAME to the trash."
    (ns-do-applescript
     (format
      "tell application \"Finder\" to delete POSIX file \"%s\""
      filename)))

  ;; (Do not) make new frames when opening a new file with Emacs unless on scratch buffer
  (setq ns-pop-up-frames nil)

  ;; fullscreen (disable for non-space full screen)
  (setq ns-use-native-fullscreen nil)

  ;; disable emacs-mac smooth scrolling because it is seriously janky
  (setq mac-mouse-wheel-smooth-scroll nil)

  ;; Set modifier keys
  (setq mac-option-modifier 'meta) ;; Bind meta to ALT
  (setq mac-command-modifier 'super) ;; Bind apple/command to super if you want
  (setq mac-function-modifier 'hyper) ;; Bind function key to hyper if you want
  (setq mac-right-option-modifier 'none) ;; unbind right key for accented input

  ;; Make forward delete work
  (global-set-key (kbd "<H-backspace>") 'delete-forward-char)

  ;; Keybindings
  (global-set-key (kbd "s-q") 'cpm/delete-frame-or-quit)
  (global-set-key (kbd "H-q") 'cpm/kill-all-emacsen)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-c") 'evil-yank)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-n") 'eyebrowse-create-window-config)
  (global-set-key (kbd "s-N") 'make-frame)
  (global-set-key (kbd "s-z") 'evil-undo)
  (global-set-key (kbd "s-Z") 'evil-redo)
  (global-set-key (kbd "s-s")
                  (lambda ()
                    (interactive)
                    (call-interactively (key-binding "\C-x\C-s"))))
  ;; (global-set-key (kbd "s-Z") 'undo-tree-redo)
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
  ;; Emacs sometimes registers C-s-f as this weird keycode
  (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen))

;;;; Reveal in Finder
(use-package reveal-in-osx-finder
  :defer 2)

;;;; Get mac links from safari
(use-package grab-mac-link
  :defer 1)

(with-eval-after-load 'org-mac-link
  (defun org-mac-message-open (message-id)
    "Visit the message with MESSAGE-ID.
This will use the command `open' with the message URL."
    (start-process (concat "open message:" message-id) nil
                   "open" (concat "message://" (substring message-id 2) ""))))

;;;; Homebrew
(use-package homebrew
  :straight (homebrew :host github :repo "jdormit/homebrew.el")
  :commands
  (homebrew-install homebrew-upgrade homebrew-update homebrew-edit homebrew-info homebrew-package-info))

;;;; Security Keychain
;; Seehttps://www.reddit.com/r/emacs/comments/ew75ib/emacs_mu4e_and_mbsync_setup_for_fastmail_on_macos/fg23tcj?utm_source=share&utm_medium=web2x&context=3
(eval-after-load 'auth-source
  '(when (member window-system '(mac ns))
     (add-to-list 'auth-sources 'macos-keychain-internet)
     (add-to-list 'auth-sources 'macos-keychain-generic)))


;;; End OSX Settings
(provide 'setup-osx)
