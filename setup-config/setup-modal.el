;;; Meow Setup
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))

  (meow-leader-define-key
   `("y" . ,cpm-prefix) ;; my namespaced keybindings
   ;; here we create bindings for necessary, high frequency commands
   '("?" . consult-apropos)
   ;; high frequency keybindings
   '(")" . "C-)")
   '("}" . "C-}")
   '("." . "M-.")
   '("," . "C-c")
   '("SPC" . execute-extended-command)
   '("<backtab>" . crux-switch-to-previous-buffer)
   '("TAB" . cpm/tab-bar-select-tab-dwim)
   '("C-M-t" . tab-bar-new-tab)
   ;; high frequency commands
   '(";" . comment-line)
   '("\\" . multi-vterm-dedicated-toggle)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("{" . tab-bar-switch-to-prev-tab)
   '("}" . tab-bar-switch-to-next-tab)
   '("[" . cpm/previous-user-buffer     )
   '("]" . cpm/next-user-buffer         )
   '("=" . hl-line-mode)
   '("a" . execute-extended-command)
   '("A" . consult-org-agenda)
   '("b" . cpm+buffer-keys)
   '("c" . cpm/find-files-setup-config-directory)
   '("C" . cpm/search-setup-config-files)
   '("d" . dired-jump-other-window)
   '("D" . dired-jump)
   '("e" . cpm+eval-keys)
   '("E" . cpm/call-emacs)
   '("f" . cpm+file-keys)
   '("F" . consult-recent-file)
   '("j" . avy-goto-char-timer)
   '("J" . crux-top-join-line)
   '("k" . consult-yank-from-kill-ring)
   '("l" . vertico-repeat)
   '("L" . consult-locate)
   '("m" . consult-mark)
   '("n" . cpm+notes-keys)
   '("N" . consult-notes-search-all)
   '("p" . cpm+project-keys)
   '("q" . cpm+quit-keys)
   '("r" . consult-register)
   '("s" . cpm+search-keys)
   '("S" . cpm/search-in-input-dir)
   '("t" . cpm+toggle-keys)
   '("u" . cpm+user-keys)
   '("v" . cpm+vc-keys)
   '("w" . cpm+window-keys)
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
   '("Q" . meow-goto-line)
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
  ;; FIXME: Would be nice to get this working with which-key
  (setq meow-keypad-describe-keymap-function 'meow-describe-keymap)
  ;; Make sure delete char means delete char
  ;; see https://github.com/meow-edit/meow/issues/112
  (setq meow--kbd-delete-char "<deletechar>")
  (setq meow-use-clipboard t)
  (meow-thing-register 'angle '(regexp "<" ">") '(regexp "<" ">"))
  (add-to-list 'meow-char-thing-table '(?a . angle))

  (meow-setup)
  (meow-global-mode 1))


;;; End Setup-Modal
(provide 'setup-modal)
