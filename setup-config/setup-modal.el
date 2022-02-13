;;; Personal Leader Key

(defvar cpm+leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> key, for use with modal keybindings.")

;; Use cpm-prefix as leader namespace
(bind-keys :prefix-map cpm+leader-map
           :prefix cpm-prefix)

;;; Meow Setup
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))

  ;; Set leader
  (add-to-list 'meow-keymap-alist (cons 'leader cpm+leader-map))

  (meow-leader-define-key

   ;;  ;; here we create bindings for necessary, high frequency commands
   '("?" . consult-apropos)
   ;; high frequency keybindings
   '(")" . "C-)")
   '("}" . "C-}")
   '("." . "M-.")
   '("[" . cpm/previous-user-buffer)
   '("]" . cpm/next-user-buffer)
   '("TAB" . cpm/tab-bar-select-tab-dwim)
   '("SPC" . execute-extended-command)
   ;; high frequency commands
   '(";" . comment-line)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("=" . hl-line-mode)
   '("a" . consult-org-agenda)
   '("b" . cpm+buffer-keys)
   '("C" . cpm+config-keys)
   '("d" . dired-jump)
   '("D" . dired-jump-other-window)
   '("e" . cpm+eval-keys)
   '("E" . restart-emacs-start-new-emacs)
   '("f" . cpm+file-keys)
   '("F" . consult-recent-file)
   '("i" . cpm/find-files-setup-config-directory)
   '("I" . cpm/search-setup-config-files)
   '("j" . avy-goto-char-timer)
   '("J" . crux-top-join-line)
   '("k" . consult-yank-from-kill-ring)
   '("l" . vertico-repeat)
   '("L" . consult-locate)
   '("M" . cpm+compile-keys)
   '("n" . cpm+notes-keys)
   '("N" . consult-notes-search-all)
   `("p" . ,project-prefix-map)
   '("q" . cpm+quit-keys)
   '("r" . consult-register)
   '("s" . cpm+search-keys)
   '("S" . cpm/search-in-input-dir)
   '("t" . cpm+toggle-keys)
   '("u" . cpm+user-keys)
   '("v" . cpm+vc-keys)
   '("V" . multi-vterm-dedicated-toggle)
   '("w" . cpm+window-keys)
   '("y" . yas-minor-mode-map)
   )


  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '(":" . meow-goto-line)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . beginning-of-buffer)
   '("G" . end-of-buffer)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . overwrite-mode)
   '("s" . meow-kill)
   '("S" . embrace-commander)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-swap-grab)
   '("y" . meow-clipboard-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("&" . meow-query-replace-regexp)
   '("%" . meow-query-replace)
   '("=" . meow-grab)
   '("C-}" . cpm/next-user-buffer)
   '("C-{" . cpm/previous-user-buffer)
   '("<escape>" . meow-cancel-selection)))


;;; Meow
(use-package meow
  :straight (:type git :host github :repo "meow-edit/meow")
  :config
  ;; set colors in bespoke theme
  (setq meow-use-dynamic-face-color nil)
  (setq meow-use-cursor-position-hack t)
  ;; Make sure delete char means delete char
  ;; see https://github.com/meow-edit/meow/issues/112
  (setq meow--kbd-delete-char "<deletechar>")
  (setq meow-use-clipboard t)
  (setq meow-goto-line-function 'consult-goto-line)
  (meow-thing-register 'angle '(regexp "<" ">") '(regexp "<" ">"))
  (add-to-list 'meow-char-thing-table '(?a . angle))

  (meow-setup)
  (meow-global-mode 1))


;;; End Setup-Modal
(provide 'setup-modal)
