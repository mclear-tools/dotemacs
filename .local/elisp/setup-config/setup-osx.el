;;; OSX Settings
;;;; Clipboad
(use-package simple-clip
  :ensure t
  :git "https://github.com/rolandwalker/simpleclip.git"
  :defer 1
  :config
  (simpleclip-mode 1))

;;;; General Settings

(setq IS-LINUX (eq system-type 'gnu/linux)
      IS-MAC (eq system-type 'darwin))
(when IS-MAC
  ;; make fonts look better with anti-aliasing
  (setq mac-allow-anti-aliasing t)
  ;; delete files by moving them to the trash
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")

  ;; Make new frames when opening a new file with Emacs unless on scratch buffer
  (setq ns-pop-up-frames 1)

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
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-c") 'evil-yank)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-n") 'make-frame)
  (global-set-key (kbd "s-N") 'nameframe-create-frame)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-s")
                  (lambda ()
                    (interactive)
                    (call-interactively (key-binding "\C-x\C-s"))))
  (global-set-key (kbd "s-Z") 'undo-tree-redo)
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
  ;; Emacs sometimes registers C-s-f as this weird keycode
  (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen))

;;;; Reveal in Finder
(use-package reveal-in-osx-finder
  :ensure t
  :defer 2)

;;;; Get mac links from safari
(use-package grab-mac-link
  :ensure t
  :defer 1)

(provide 'setup-osx)
