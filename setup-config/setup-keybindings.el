;; Keybindings for config I put all keybindings here. This has its
;; disadvantages (e.g. separating functions or packages from keybindings) but
;; it also makes it the place to go to deal with all keybindings.

;;; General
(use-package general
  :demand t
  :config
  (general-create-definer cpm-leader-def
    ;; Choose a prefix that won't likely conflict with other namespaced bindings
    ;; See https://karl-voit.at/2018/07/08/emacs-key-bindings/
    :prefix "C-c C-<SPC>")
  )

;;; Meow
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; here we create bindings for necessary, high frequency commands
   ;; cheatsheet
   '("?" . meow-cheatsheet)
   ;; high frequency keybindings
   '("e" . "C-x C-e")
   '(")" . "C-)")
   '("}" . "C-}")
   '("." . "M-.")
   '("," . "M-,")
   '("y" . "C-c C-<SPC>") ;; my namespaced keybindings
   ;; window management
   '("h" . split-window-below-and-focus)
   '("H" . split-window-below)
   '("o" . delete-other-windows)
   '("v" . split-window-right-and-focus)
   '("V" . split-window-right)
   '("w" . cpm/other-window)
   '("W" . window-swap-states)
   ;; high frequency commands
   '(";" . comment-dwim)
   '("\\" . multi-vterm-dedicated-toggle)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("[" . persp-prev)
   '("]" . persp-next)
   '("=" . hl-line-mode)
   '("a" . execute-extended-command)
   '("b" . cpm/persp-consult-buffer)
   '("c" . cpm/find-files-setup-config-directory)
   '("C" . cpm/search-setup-config-files)
   '("d" . dired-jump-other-window)
   '("D" . dired-jump)
   '("f" . find-file)
   '("F" . consult-recent-file)
   '("g" . magit-status)
   '("i" . ibuffer)
   '("j" . cpm/jump-in-buffer)
   '("J" . crux-top-join-line)
   '("k" . kill-this-buffer)
   '("l" . consult-locate)
   '("m" . consult-mark)
   '("n" . consult-notes)
   '("N" . consult-notes-search-all)
   '("p" . consult-projectile)
   '("P" . persp-switch)
   '("q" . cpm/delete-frame-or-quit)
   '("Q" . restart-emacs)
   '("r" . consult-recent-file)
   '("s" . consult-line)
   '("S" . cpm/search-in-input-dir)
   ;; toggles
   '("T" . toggle-dark-light-theme)
   '("L" . display-line-numbers-mode)
   '("A" . consult-org-agenda))

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
   '("p" . meow-clipboard-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . overwrite-mode)
   '("s" . meow-kill)
   '("S" . meow-swap-grab)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-clipboard-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("&" . meow-query-replace-regexp)
   '("%" . meow-query-replace)
   '("=" . meow-grab)
   '("C-}" . cpm/next-user-buffer)
   '("C-{" . cpm/previous-user-buffer)
   '("<escape>" . meow-cancel-selection)
   '("<return>" . org-open-at-point-global)))

(use-package meow
  :straight (:type git :host github :repo "meow-edit/meow")
  :custom-face
  ;; Make sure bespoke-theme is loaded prior to meow
  (meow-normal-cursor ((t (:background ,bespoke-yellow))))
  (meow-insert-cursor ((t (:background ,bespoke-critical))))
  (meow-keypad-cursor ((t (:background ,bespoke-brown))))
  (meow-motion-cursor ((t (:background ,bespoke-green))))
  (meow-kmacro-cursor ((t (:background ,bespoke-salient))))
  (meow-beacon-fake-selection ((t (:background ,bespoke-modeline))))
  (meow-beacon-fake-cursor ((t (:background ,bespoke-yellow))))
  :config
  ;; FIXME: Would be nice to get this working with which-key
  (setq meow-keypad-describe-keymap-function 'meow-describe-keymap)
  ;; Make sure delete char means delete char
  ;; see https://github.com/meow-edit/meow/issues/112
  (setq meow--kbd-delete-char "<deletechar>")

  (meow-thing-register 'angle '(regexp "<" ">") '(regexp "<" ">"))
  (add-to-list 'meow-char-thing-table '(?a . angle))

  (meow-setup)
  (meow-global-mode 1))

;;; Which Key
(use-package which-key
  ;; :after general
  :defer 1
  :diminish ""
  :config
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; Set the time delay (in seconds) for the which-key popup to appear.
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay .75)
  (setq which-key-idle-secondary-delay 0.05)
  ;; use widow
  (setq which-key-popup-type 'side-window)
  (setq which-key-allow-imprecise-window-fit t)
  (setq which-key-side-window-location 'top)
  ;; use minibuffer
  ;; (which-key-setup-minibuffer)
  ;; separator
  (setq which-key-separator " → ")
  (which-key-mode))

;;; Outline Bindings

(general-define-key
 :keymaps 'outline-minor-mode-map
 "<tab>"   'outline-cycle
 "S-<tab>" 'outline-cycle-buffer)

(general-define-key
 :keymaps 'outline-minor-mode-map
 ;; "gh"    'outline-previous-visible-heading
 ;; "gj"    'outline-forward-same-level
 ;; "gk"    'outline-backward-same-level
 ;; "gl"    'outline-next-visible-heading
 ;; "gu"    'outline-up-heading
 "M-j"   'outline-move-subtree-down
 "M-k"   'outline-move-subtree-up
 "M-h"   'outline-promote
 "M-l"   'outline-demote)

;;; Namespaced Keybindings

;;;; Application Keybindings
(cpm-leader-def
  "a"  '(:ignore t :which-key "Applications")
  "ac" '(:ignore t :which-key "Cmus")
  "ad" 'dired-jump
  "ae" 'eshell
  "am" 'multi-term
  "ar" 'ranger
  "as" 'vterm
  "av" 'vterm-other-window
  "aw" 'wttrin
  )

;;;; Buffer Keybindings
(cpm-leader-def
  "b"  '(:ignore t :which-key "Buffers")
  "ba" 'ibuffer
  "bb" 'cpm/persp-consult-buffer
  ;; "bb" 'persp-switch-to-buffer
  ;; "bb" 'helm-mini
  "bc" 'cpm/copy-whole-buffer-to-clipboard
  "bD" 'kill-buffer-and-window
  "bd" 'cpm/kill-this-buffer
  "be" 'erase-buffer
  ;; "bf" 'cpm/browse-file-directory
  "bf" 'reveal-in-osx-finder
  "bi" 'ibuffer-jump
  "bj" 'cpm/jump-in-buffer
  "bk" 'evil-delete-buffer
  "bK" 'crux-kill-other-buffers
  "bm" 'helm-evil-markers
  "bn" 'evil-buffer-new
  "bN" 'cpm/new-buffer-new-frame
  "bp" 'consult-projectile
  "bP" 'persp-temporarily-display-buffer
  "br" 'revert-buffer
  "bR" 'crux-rename-buffer-and-file
  "bs" 'consult-buffer-other-window
  "bt" 'cpm/open-dir-in-iterm
  )

;;;; Comment Keybindings
(cpm-leader-def
  "c"   '(:ignore t :which-key "Commenting")
  "cb"  '(nil :wk "Block Wrap")
  "cbo" 'org-block-wrap
  "cby" 'cpm/yaml-wrap
  "cc"  'evil-commentary
  "cl"  'evil-commentary-line
  "cy"  'evil-commentary-yank-line
  )

;;;; Config Keybindings
(cpm-leader-def
  "C"  '(:ignore t :which-key "Config")
  "Cc" 'goto-custom.el
  "Cd" 'goto-dotfiles.org
  "CD" 'goto-emacs-dir
  "CE" 'goto-cpm-elisp-dir
  "Ce" 'goto-early-init.el
  "Cf" 'cpm/find-files-setup-config-directory
  "Ck" 'cpm/byte-compile-dotemacs
  "CK" 'cpm/delete-byte-compiled-files
  "Cl" 'load-config
  "Ci" 'goto-init.el
  "CI" 'cpm/load-init-file
  "Co" 'goto-org-files
  "Cp" 'goto-pandoc-config
  "Cs" 'cpm/search-setup-config-files
  )

;;;; File Keybindings
(cpm-leader-def
  "f"  '(:ignore t :which-key "Files")
  "fb" 'consult-bookmark
  ;; "fb" 'helm-bookmarks
  "ff" 'find-file
  ;; "ff" 'helm-find-files
  "fl" 'consult-locate
  ;; "fl" 'helm-locate
  "fo" 'crux-open-with
  "fs" 'save-buffer
  "fr" 'consult-recent-file
  ;; "fr" 'helm-recentf
  "fy" 'cpm/show-and-copy-buffer-filename
  )

;;;; General Keybindings
(cpm-leader-def
  "A" 'consult-apropos
  "?" 'consult-man
  ;; "A" 'helm-apropos
  ;; "B" #'cpm/dashboard
  ;; "?" 'counsel-descbinds
  ;; "?" 'helm-descbinds
  "<SPC>" 'execute-extended-command
  ;; "<SPC>" 'helm-M-x
  ;; "d" #'deer
  ;; "c" #'company-complete
  "d" #'dired-jump
  "D" #'dired-jump-other-window
  ;; "D" #'cpm/deer-split-window
  "E" 'cpm/call-emacs
  ;; "e" 'server-edit
  "e" 'cpm/email-save-and-kill
  ;; "e" 'cpm/org-to-mail-rtf
  "G" 'general-describe-keybindings
  ;; "j" 'avy-goto-char
  "k" 'consult-yank-pop
  ;; "k" 'helm-show-kill-ring
  "l" 'vertico-repeat
  ;; "l" 'selectrum-repeat
  ;; "l" 'helm-resume
  ;; "N" 'research-notes
  ;; "n" 'consult-notes
  "r" 'cpm/resume-last-jump
  "S" 'hydra-spelling/body
  ;; "W" 'woman
  "#" 'universal-argument
  ;; "`" 'beacon-blink
  ;; "'" 'shell-pop
  ;; "\\" 'vterm-toggle-cd
  "\\" 'multi-vterm-dedicated-toggle
  "," 'recenter-top-bottom
  "." 'quick-commit
  ";" 'evil-commentary-line
  "[" 'cpm/previous-user-buffer
  "]" 'cpm/next-user-buffer
  "TAB" 'switch-to-previous-buffer
  ":" 'shell-command
  )

;;;; Compile Keybindings
(cpm-leader-def
  "M"   '(:ignore t :which-key "Make/Compile")
  "Mm"  'compile
  "MM"  'multi-compile-run
  "Me"  'compile-goto-error
  "Mk"  'cpm/compile-next-makefile
  "MK"  'kill-compilation
  "Mr"  'recompile
  "Mv"  'cpm/make-move
  )

;;;; Project Keybindings
(global-set-key (kbd "C-h C-c") 'finder-commentary)
(cpm-leader-def
  "p" '(:ignore t :which-key "Projects")
  "p!"  'projectile-run-shell-command-in-root
  "p&"  'projectile-run-async-shell-command-in-root
  "pa"  'projectile-toggle-between-implementation-and-test
  ;; "pb"  'projectile-switch-to-buffer
  "pb"  'consult-projectile
  ;; "pc"  'consult-projectile
  "pc"  'projectile-compile-project
  "pd"  'projectile-find-dir
  "pD"  'projectile-dired
  "pf"  'projectile-find-file
  "pF"  'projectile-find-file-other-window
  "pg"  'cpm/goto-projects
  ;; "ph"  'projectile
  "pi"  'consult-project-imenu
  "pJ"  'bookmark
  "pG"  'projectile-regenerate-tags
  "pI"  'projectile-invalidate-cache
  "pk"  'projectile-kill-buffers
  "pn"  #'cpm/open-new-buffer-and-workspace
  "pN"  #'cpm/create-new-project-and-workspace
  "po"  #'cpm/open-existing-project-and-workspace
  "pp"  'projectile-switch-project
  "pr"  'recentf
  "pR"  'projectile-replace
  "ps"  #'projectile-ag
  ;; "ps1" #'cpm/load-phil101
  ;; "ps2" #'cpm/load-phil232
  ;; "ps5" #'cpm/load-phil105
  ;; "ps8" #'cpm/load-phil871
  ;; "psa" #'cpm/load-kant-apperception-substance
  ;; "psb" #'(:ignore t :which-key "Books")
  ;; "psba" #'cpm/load-kant-agency-book
  ;; "psbr" #'cpm/load-kant-rationality-book
  ;; "psc" #'cpm/load-emacs-config
  ;; "psf" #'cpm/load-kant-free-thought
  ;; "psr" #'cpm/load-kant-reflection
  ;; "pst" #'cpm/load-org-agenda-todo
  ;; "psw" #'cpm/load-website
  ;; "psz" #'cpm/load-zettelkasten
  ;; "pt"  #'org-projectile-helm-template-or-project
  "pT"  'projectile-find-test-file
  "pV"  'projectile-vc
  "py"  'projectile-find-tag
  )

;;;; Quit Keybindings
(cpm-leader-def
  "q"  '(:ignore t :which-key "Quit")
  ;; "qq" 'cpm/save-desktop-save-buffers-kill-emacs
  "qd" 'cpm/kill-emacs-capture-daemon
  "qq" 'save-buffers-kill-emacs
  "qQ" 'cpm/kill-all-emacsen
  "qr" 'restart-emacs
  )

;;;; Search Keybindings
(cpm-leader-def
  "s" '(:ignore t :which-key "Search")
  ;; "sa" 'helm-org-rifle-agenda-files
  "sa" 'consult-org-agenda
  "sd" 'affe-grep; search current buffer's directory
  "sD" #'cpm/search-in-input-dir ; search with directory input
  "sb" 'consult-multi-occur
  ;; "sb" 'helm-ag-buffers
  ;; "sf" 'helm-do-ag-this-file
  "sf" 'consult-line
  "sh" 'consult-org-heading
  "sj" 'cpm/forward-or-backward-sexp
  "sk" 'consult-yank-pop
  "sl" 'selectrum-repeat
  "sn" #'consult-notes-search-all
  ;; "sn" #'cpm/search-all-notes
  ;; "sk" 'helm-show-kill-ring
  ;; "sl" 'cpm/helm-list-search-buffers
  ;; "sm" 'swiper-mc
  ;; "so" #'ivy-occur
  ;; "so" 'helm-occur
  ;; "sp" 'swiper-thing-at-point
  "sp" #'consult-line-symbol-at-point
  "sr" #'vr/query-replace
  ;; "sR" 'helm-org-rifle
  ;; "ss" #'swiper
  "ss" 'consult-line
  ;; "ss" #'counsel-grep-or-swiper ;; search with swiper in file
  ;; "ss" 'helm-swoop-without-pre-input ;; search with swoop in file
  "sS" #'cpm/flyspell-ispell-goto-next-error ;; search for next spelling error
  "st" #'cpm/hydra-todo/body
  ;; "st" #'cpm/search-file-todo-markers ;; search for TODOs in file w/helm-ag
  ;; "sT" #'ivy-magit-todos  ;; search todos in git project
  ;; "sT" #'cpm/search-todo-markers ;; search todo markers in directory w/helm-ag
  )

;;;; Toggle Keybindings
(cpm-leader-def
  "t"  '(:ignore t :which-key "Toggles")
  "ta" 'company-mode
  "tb" 'buffer-line-mode
  "tB" 'beacon-mode
  "tc" 'centered-cursor-mode
  "tC" 'centered-window-mode
  ;; "td" 'cpm/osx-toggle-menubar-theme
  ;; "tf" 'toggle-serif
  "tF" 'toggle-frame-maximized
  "tg" 'git-gutter-mode
  "tG" 'golden-ratio-mode
  "th" 'hl-line-mode
  "tH" 'hidden-mode-line-mode
  "te" 'toggle-indicate-empty-lines
  "tE" 'eldoc-mode
  "tM" #'treemacs
  "tm" #'cpm/toggle-display-markup
  ;; "tn" 'nlinum-mode
  "tn" 'display-line-numbers-mode
  "tN" 'org-numbers-overlay-mode
  "to" #'imenu-list-smart-toggle
  ;; "to" 'org-toggle-link-display
  ;; "tO" 'outline-toc-mode
  "tp" 'smartparens-mode
  "tP" 'show-paren-mode
  "tr" 'rainbow-identifiers-mode
  "ts" 'flyspell-mode
  "tS" 'ispell-buffer
  "tt" 'toggle-dark-light-theme
  "tT" 'cpm/load-theme
  ;; "tT" 'helm-themes
  "tv" 'vterm-toggle-cd
  "tw" 'writeroom-mode
  "tz" 'zone
  )

;;;; User Keybindings
(cpm-leader-def
  "u"  '(:ignore t :which-key "User")
  "uA" '(:ignore t :which-key "Agenda Files")
  "uAa" #'cpm/goto-reading.org
  "uAc" #'cpm/goto-conferences.org
  "uAf" #'cpm/goto-org-files
  "uAi" #'cpm/goto-inbox.org
  "uAr" #'cpm/goto-reference.org
  "uAR" #'cpm/goto-referee-reports.org
  ;; "uAp" #'cpm/goto-projects.org
  "uAs" #'cpm/goto-someday.org
  "uAt" #'cpm/goto-todo.org
  "uAT" #'cpm/goto-teaching.org
  "uAw" #'cpm/goto-writing.org
  "ua"  '(:ignore t :which-key "Agenda")
  "uaa" 'cpm/jump-to-org-super-agenda
  "uaw" 'cpm/jump-to-week-agenda
  "um" 'cpm/org-to-markdown
  ;; "uc" 'cpm/pandoc-convert-to-pdf
  "ub" '(:ignore t :which-key "Beamer functions")
  "ubp" #'cpm/org-export-beamer-presentation
  "ubh" #'cpm/org-export-beamer-handout
  "uc"  '(:ignore t :whichkey "Citations")
  "uci" #'org-cite-insert
  "ucn" #'citar-open-notes
  "uce" #'citar-open-entry
  ;; "uC" 'cpm/pandoc-command-line-convert-to-pdf
  "ug" 'org-mac-grab-link
  "uh" #'cpm/org-export-to-buffer-html-as-body
  "ui" 'cpm/org-goto-inbox
  "uk" 'kill-compilation
  "ul" 'desktop-read
  "uo" 'cpm/markdown-to-org
  "up" 'run-pandoc
  "uP" 'cpm/pandoc-pdf-open
  "us" 'sb-expand-current-file
  "uS" 'just-one-space
  ;; "ut" 'cpm/org-goto-todo
  "ut" 'cpm/jump-to-org-agenda-all-todos
  "ud" 'distraction-free
  "uD" 'my-desktop
  "uj" 'cpm/goto-journal
  ;; "op" 'pandoc-convert-to-pdf
  "uu" #'cpm/straight-update-packages-asynchronously
  "uw" 'count-words
  "uW" 'osx-dictionary-search-input
  )

;;;; Version Control (Git) Keybindings
(cpm-leader-def
  "g"  '(:ignore t :which-key "Git")
  "gb" 'magit-blame
  "gc" 'magit-commit
  "gd" 'magit-diff
  "gh" #'hydra-git-gutter/body
  "gl" 'magit-log
  "gL" 'magit-log-buffer-file ;; show history of selected region
  "gn" 'git-gutter:next-hunk
  "gp" 'git-gutter:previous-hunk
  "gr" 'magit-reflog
  "gs" 'magit-status
  )

;;;;  Window Keybindings
(defun cpm/other-window ()
  (interactive)
  (other-window 1))
(general-def :keymaps 'override
  "C-c C-o" 'cpm/other-window)

(cpm-leader-def
  "0" 'winum-select-window-0
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5

  "w"  '(:ignore t :which-key "Windows")
  "wa" 'ace-window
  "wf" 'cpm/toggle-window-split
  "wc" 'delete-window
  "wd" 'delete-window
  "wh" 'split-window-horizontally
  "wm" 'delete-other-windows
  "wr" 'cpm/rotate-windows
  "wR" 'cpm/rotate-windows-backward
  "wt" 'tear-off-window
  "wu" 'winner-undo
  "wU" 'winner-redo
  "wv" 'cpm/split-window-right-and-focus
  "wV" 'split-window-vertically
  "wx" 'cpm/window-exchange-buffer
  "w-" 'split-window-below
  "w_" 'cpm/split-window-below-and-focus
  )

;;;; Zettelkasten/Notes/Wiki
;; (cpm-leader-def
;;   "n"    '(:ignore t :which-key "Notes")
;;   "n c"  #'org-roam-capture
;;   "n i"  #'org-roam-node-insert
;;   "n f"  #'org-roam-node-find
;;   "n g"  #'org-roam-graph
;;   "n n"  #'consult-notes
;;   "n N"  #'org-roam--new-file-named
;;   "n r"  #'cpm/find-note-relation
;;   "n s"  #'consult-notes-search-all
;;   "n t"  #'org-roam-buffer-toggle)

;;; Markdown Keybindings
;; (cpm-leader-def
;;   ""   '(nil :which-key "Local Leader")
;;   "c"  '(:ignore t :which-key "command")
;;   "h"  '(:ignore t :which-key "insert")
;;   "i"  '(:ignore t :which-key "lists")
;;   "t"  '(:ignore t :which-key "text")

;;   ;; Movement
;;   "{"   'markdown-backward-paragraph
;;   "}"   'markdown-forward-paragraph

;;   ;; Completion, and Cycling
;;   "]"   'markdown-complete

;;   ;; Indentation
;;   ">"   'markdown-indent-region
;;   "<"   'markdown-exdent-region

;;   ;; Buffer-wide commands
;;   "c]"  'markdown-complete-buffer
;;   "cb"  'cpm/clone-buffer-and-narrow
;;   "cc"  'multi-compile-run
;;   "cl"  'markdown-live-preview-mode
;;   "cm"  'markdown-other-window
;;   "cn"  'markdown-cleanup-list-numbers
;;   "co"  'markdown-open
;;   "cp"  'markdown-preview
;;   "cr"  'markdown-check-refs
;;   "cv"  'markdown-export-and-preview
;;   "cw"  'markdown-kill-ring-save

;;   ;; headings
;;   "hi"  'markdown-insert-header-dwim
;;   "hI"  'markdown-insert-header-setext-dwim
;;   "h1"  'markdown-insert-header-atx-1
;;   "h2"  'markdown-insert-header-atx-2
;;   "h3"  'markdown-insert-header-atx-3
;;   "h4"  'markdown-insert-header-atx-4
;;   "h5"  'markdown-insert-header-atx-5
;;   "h6"  'markdown-insert-header-atx-6
;;   "h!"  'markdown-insert-header-setext-1
;;   "h@"  'markdown-insert-header-setext-2

;;   ;; Insertion of common elements
;;   "-"   'markdown-insert-hr
;;   "if"  'markdown-insert-footnote
;;   "ii"  'markdown-insert-image
;;   "ik"  'spacemacs/insert-keybinding-markdown
;;   "iI"  'markdown-insert-reference-image
;;   "il"  'markdown-insert-link
;;   "iL"  'markdown-insert-reference-link-dwim
;;   "iw"  'markdown-insert-wiki-link
;;   "iu"  'markdown-insert-uri

;;   ;; Element removal
;;   "k"   'markdown-kill-thing-at-point

;;   ;; Numbering
;;   "n"   #'markdown-cleanup-list-numbers
;;   ;; List editing
;;   "li"  'markdown-insert-list-item

;;   ;; region manipulation
;;   "tb"  'markdown-insert-bold
;;   "ti"  'markdown-insert-italic
;;   "tc"  'markdown-insert-code
;;   "tC"  'markdown-insert-gfm-code-block
;;   "tq"  'markdown-insert-blockquote
;;   "tQ"  'markdown-blockquote-region
;;   "tp"  'markdown-insert-pre
;;   "tP"  'markdown-pre-region
;;   "tn"  'cpm/narrow-or-widen-dwim

;;   ;; Following and Jumping
;;   "N"   'markdown-next-link
;;   "f"   'markdown-follow-thing-at-point
;;   "P"   'markdown-previous-link
;;   "<RET>" 'markdown-do

;;   "gj"    #'markdown-next-visible-heading
;;   "gk"    #'markdown-previous-visible-heading
;;   ;; Assumes you have a markdown renderer plugin in chrome
;;   "M-r"   #'browse-url-of-file
;;   "h]"    #'markdown-next-visible-heading
;;   "h["    #'markdown-previous-visible-heading
;;   "p["    #'markdown-promote
;;   "p]"    #'markdown-demote
;;   "l["    #'markdown-next-link
;;   "l]"    #'markdown-previous-link
;;   )

;; (general-define-key
;;  :states '(emacs)
;;  :keymaps 'markdown-mode-map

;;  "s-*"      #'markdown-insert-list-item
;;  "s-b"      #'markdown-insert-bold
;;  "s-i"      #'markdown-insert-italic

;;  "M--"      #'markdown-insert-hr
;;  "M-RET"    #'markdown-insert-header
;;  )

;; (general-define-key
;;  :states '(emacs)
;;  :keymaps 'markdown-mode-map

;;  "RET"    #'markdown-follow-thing-at-point)

;; ;; Show which-key top-level bindings
;; (global-set-key (kbd "H-k") 'which-key-show-top-level)
;; ;; override evil insert for kill line
;; (general-define-key :states '(insert) "C-k" 'kill-line)

;; ;; ;;; Package Keybindings
;; ;; (cpm-leader-def
;; ;;   "P" '(:ignore t :which-key "Packages")
;; ;;   "Pl" 'paradox-list-packages
;; ;;   "Pu" 'paradox-upgrade-packages
;; ;;   "Pc" 'finder-commentary
;; ;;   )


;;; Org Keybindings
;; (general-define-key
;;  :states '(emacs)
;;  :keymaps 'org-mode-map
;;  :prefix "C-c C-o"
;;  ""    '(nil :which-key "Local Leader")
;;  "<tab>" #'org-cycle
;;  "RET" #'cpm/org-archive-done-tasks
;;  "SPC" #'org-toggle-checkbox
;;  "."   #'org-cycle-agenda-files
;;  "/"   #'org-sparse-tree
;;  "="   #'org-align-tags
;;  "?"   #'org-tags-view
;;  ":"   #'org-set-tags
;;  "a"   #'cpm/jump-to-org-super-agenda
;;  "A"   #'org-archive-subtree
;;  "b"   #'cpm/clone-buffer-and-narrow
;;  "B"   #'org-babel-tangle
;;  "c"   #'org-capture
;;  "d"   #'org-time-stamp
;;  "D"   #'org-deadline
;;  "e"   #'org-edit-special
;;  "f"   #'org-fill-paragraph
;;  "n"   #'cpm/narrow-or-widen-dwim
;;  "r"   #'org-refile
;;  "s"   #'org-schedule
;;  "t"   #'cpm/org-select-tags-completing-read
;;  "T"   #'org-todo
;;  "v"   #'variable-pitch-mode
;;  "l"   #'org-insert-link
;;  "L"   #'org-store-link
;;  "+"   #'org-timestamp-up-day
;;  "-"   #'org-timestamp-down-day
;;  "<"   #'org-metaleft
;;  ">"   #'org-metaright

;;  "i"  '(:ignore t :which-key "Insert...")
;;  "il" #'org-insert-link
;;  "if" #'org-footnote-new

;;  "R"  '(:ignore t :which-key "RevealJS..." )
;;  "Rr" #'cpm/reveal-to-html-open
;;  "Rs" #'cpm/narrowed-subtree-to-html
;;  "RS" #'org-reveal-export-current-subtree
;;  "Rp" #'cpm/reveal-to-pdf)

;; (general-define-key
;;  :states '(emacs)
;;  :keymaps 'org-agenda-mode-map
;;  :prefix "C-c C-a"
;;  "" nil
;;  "<escape>" #'org-agenda-Quit
;;  "E"   #'org-agenda-entry-text-mode
;;  "m"   #'org-agenda-month-view
;;  "C-j" #'org-agenda-next-item
;;  "C-k" #'org-agenda-previous-item
;;  "C-n" #'org-agenda-next-item
;;  "C-p" #'org-agenda-previous-item)

;; ;; (general-define-key :states '(normal) :keymaps 'org-mode-map
;; ;;   "RET" 'org-open-at-point     ;; Open with return in evil
;; ;;   "p"   'org-yank ;; better pasting behavior
;; ;;   "s-J" 'crux-top-join-line)

;; ;; ;;   normal, insert, visual shortcuts
;; ;; (general-define-key :states '(normal insert visual) :keymaps 'org-mode-map
;; ;;   "M-q" #'cpm/fill-or-unfill
;; ;;   "C-t" #'transpose-chars)

;; ;;   ;; normal & insert state shortcuts.
;; (general-define-key :states '(emacs) :keymaps 'org-mode-map
;;   ;; easily emphasize text
;;   ;; see https://emacs.stackexchange.com/questions/27645/unable-to-bind-emphasize-key-in-org-mode
;;   "s-b" (lambda () (interactive) (er/mark-word) (org-emphasize ?\*))
;;   "s-i" (lambda () (interactive) (er/mark-word) (org-emphasize ?\/))
;;   "s-l" (lambda () (interactive) (er/mark-word) (org-emphasize ?\=))
;;   ;; better pasting behavior in org-mode
;;   "s-v" 'org-yank)




;;; End keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-keybindings)
