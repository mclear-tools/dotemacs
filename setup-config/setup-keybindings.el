;; Keybindings for config

;;; General
(use-package general
  :demand t
  :config
  (general-create-definer cpm/leader-keys
	:states '(normal visual motion emacs insert)
	:keymaps 'override
	:prefix "SPC"
	:non-normal-prefix "C-SPC")
  (general-create-definer cpm/local-leader-keys
	:states '(normal visual)
	:keymaps 'override
	:prefix ","
	:non-normal-prefix "C-,")
  (general-override-mode))

;;; Which Key
(use-package which-key
  :after general
  :defer 1
  :diminish ""
  :config
  (setq which-key-special-keys nil)
  ;; Set the time delay (in seconds) for the which-key popup to appear.
  (setq which-key-idle-delay .6)
  ;; use minibuffer
  (which-key-setup-minibuffer)
  (which-key-mode))

;;; Namespaced Keybindings

;; set keybindings for use with evil
(with-eval-after-load 'evil

;;;; Application Keybindings

  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

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
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "b"  '(:ignore t :which-key "Buffers")
   "ba" 'consult-buffer
   "bb" 'cpm/persp-consult-buffer
   ;; "bb" 'persp-switch-to-buffer
   ;; "bb" 'helm-mini
   "bc" 'cpm/copy-whole-buffer-to-clipboard
   "bD" 'kill-buffer-and-window
   "bd" 'cpm/kill-this-buffer
   "be" 'erase-buffer
   ;; "bf" 'cpm/browse-file-directory
   "bf" 'reveal-in-osx-finder
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
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "c"   '(:ignore t :which-key "Commenting")
   "cb"  '(nil :wk "Block Wrap")
   "cbo" 'org-block-wrap
   "cby" 'cpm/yaml-wrap
   "cc"  'evil-commentary
   "cl"  'evil-commentary-line
   "cy"  'evil-commentary-yank-line
   )

;;;; Config Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

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
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

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
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "A" 'consult-apropos
   "?" 'consult-man
   ;; "A" 'helm-apropos
   ;; "B" #'cpm/dashboard
   ;; "?" 'counsel-descbinds
   ;; "?" 'helm-descbinds
   "<SPC>" 'execute-extended-command
   ;; "<SPC>" 'helm-M-x
   ;; "d" #'deer
   "c" #'company-complete
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
   ;; "l" 'uchronia-repeat
   "l" 'selectrum-repeat
   ;; "l" 'helm-resume
   ;; "N" 'research-notes
   "n" 'cpm/notebook
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
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "M"   '(:ignore t :which-key "Make/Compile")
   "Mm"  'compile
   "MM"  'multi-compile-run
   "Me"  'compile-goto-error
   "Mk"  'cpm/compile-next-makefile
   "MK"  'kill-compilation
   "Mr"  'recompile
   "Mv"  'cpm/make-move
   )

;;; Markdown Keybindings
  (use-package evil-markdown
    ;; :load-path "~/.emacs.d/.local/elisp/evil-markdown"
    :straight (:host github :repo "Somelauw/evil-markdown")
    :after markdown-mode evil
    :demand t)

  (general-define-key
   :states '(normal motion visual)
   :keymaps 'markdown-mode-map
   :prefix "SPC m"
   :non-normal-prefix "C-SPC m"

   ""    '(nil :which-key "Local Leader")
   "c"  '(:ignore t :which-key "command")
   "h"  '(:ignore t :which-key "insert")
   "i"  '(:ignore t :which-key "lists")
   "t"  '(:ignore t :which-key "text")

   ;; Movement
   "{"   'markdown-backward-paragraph
   "}"   'markdown-forward-paragraph

   ;; Completion, and Cycling
   "]"   'markdown-complete

   ;; Indentation
   ">"   'markdown-indent-region
   "<"   'markdown-exdent-region

   ;; Buffer-wide commands
   "c]"  'markdown-complete-buffer
   "cb"  'cpm/clone-buffer-and-narrow
   "cc"  'multi-compile-run
   "cl"  'markdown-live-preview-mode
   "cm"  'markdown-other-window
   "cn"  'markdown-cleanup-list-numbers
   "co"  'markdown-open
   "cp"  'markdown-preview
   "cr"  'markdown-check-refs
   "cv"  'markdown-export-and-preview
   "cw"  'markdown-kill-ring-save

   ;; headings
   "hi"  'markdown-insert-header-dwim
   "hI"  'markdown-insert-header-setext-dwim
   "h1"  'markdown-insert-header-atx-1
   "h2"  'markdown-insert-header-atx-2
   "h3"  'markdown-insert-header-atx-3
   "h4"  'markdown-insert-header-atx-4
   "h5"  'markdown-insert-header-atx-5
   "h6"  'markdown-insert-header-atx-6
   "h!"  'markdown-insert-header-setext-1
   "h@"  'markdown-insert-header-setext-2

   ;; Insertion of common elements
   "-"   'markdown-insert-hr
   "if"  'markdown-insert-footnote
   "ii"  'markdown-insert-image
   "ik"  'spacemacs/insert-keybinding-markdown
   "iI"  'markdown-insert-reference-image
   "il"  'markdown-insert-link
   "iL"  'markdown-insert-reference-link-dwim
   "iw"  'markdown-insert-wiki-link
   "iu"  'markdown-insert-uri

   ;; Element removal
   "k"   'markdown-kill-thing-at-point

   ;; Numbering
   "n"   #'markdown-cleanup-list-numbers
   ;; List editing
   "li"  'markdown-insert-list-item

   ;; region manipulation
   "tb"  'markdown-insert-bold
   "ti"  'markdown-insert-italic
   "tc"  'markdown-insert-code
   "tC"  'markdown-insert-gfm-code-block
   "tq"  'markdown-insert-blockquote
   "tQ"  'markdown-blockquote-region
   "tp"  'markdown-insert-pre
   "tP"  'markdown-pre-region
   "tn"  'cpm/narrow-or-widen-dwim

   ;; Following and Jumping
   "N"   'markdown-next-link
   "f"   'markdown-follow-thing-at-point
   "P"   'markdown-previous-link
   "<RET>" 'markdown-do

   "gj"    #'markdown-next-visible-heading
   "gk"    #'markdown-previous-visible-heading
   ;; Assumes you have a markdown renderer plugin in chrome
   "M-r"   #'browse-url-of-file
   "h]"    #'markdown-next-visible-heading
   "h["    #'markdown-previous-visible-heading
   "p["    #'markdown-promote
   "p]"    #'markdown-demote
   "l["    #'markdown-next-link
   "l]"    #'markdown-previous-link
   )

  (general-define-key
   :states '(normal motion insert)
   :keymaps 'markdown-mode-map

   "s-*"      #'markdown-insert-list-item
   "s-b"      #'markdown-insert-bold
   "s-i"      #'markdown-insert-italic

   "M--"      #'markdown-insert-hr
   "M-RET"    #'markdown-insert-header
   )

  (general-define-key
   :states '(normal motion)
   :keymaps 'markdown-mode-map

   "RET"    #'markdown-follow-thing-at-point)

  ;; Show which-key top-level bindings
  (global-set-key (kbd "H-k") 'which-key-show-top-level)
  ;; override evil insert for kill line
  (general-define-key :states '(insert) "C-k" 'kill-line)

;;; Package Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "P" '(:ignore t :which-key "Packages")
   "Pl" 'paradox-list-packages
   "Pu" 'paradox-upgrade-packages
   "Pc" 'finder-commentary
   )

;;; Project Keybindings
  (global-set-key (kbd "C-h C-c") 'finder-commentary)
  (general-define-key
   :states '(normal visual emacs motion)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

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

;;; Quit Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "q"  '(:ignore t :which-key "Quit")
   ;; "qq" 'cpm/save-desktop-save-buffers-kill-emacs
   "qd" 'cpm/kill-emacs-capture-daemon
   "qq" 'evil-quit-all
   "qQ" 'cpm/kill-all-emacsen
   "qr" 'restart-emacs
   )

;;; Search Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

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
   "sn" #'cpm/search-all-notes
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

;;; Toggle Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

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

;;; User Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

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
   "uC" 'cpm/pandoc-command-line-convert-to-pdf
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
   "uw" 'count-words
   "uW" 'osx-dictionary-search-input
   "ux" 'bibtex-actions-open
   )

;;; Version Control (Git) Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "g"  '(:ignore t :which-key "Git")
   "gb" 'magit-blame
   "gc" 'magit-commit
   "gd" 'magit-diff
   "gh" #'hydra-git-gutter/body
   "gl" 'magit-log
   "gn" 'git-gutter:next-hunk
   "gp" 'git-gutter:previous-hunk
   "gr" 'magit-reflog
   "gs" 'magit-status
   )

;;; Window Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

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
   "wm" 'delete-other-windows
   "wr" 'cpm/rotate-windows
   "wR" 'cpm/rotate-windows-backward
   "wt" 'tear-off-window
   "wu" 'winner-undo
   "wU" 'winner-redo
   "wv" 'cpm/split-window-right-and-focus
   "wV" 'evil-window-vsplit
   "wx" 'cpm/window-exchange-buffer
   "w-" 'evil-window-split
   "w_" 'cpm/split-window-below-and-focus
   )

;;; Org Keybindings
  (use-package evil-org
    :after org
    :hook (org-mode . evil-org-mode)
    :config
    (require 'evil-org-agenda)
    ;; write as sep hook so byte compile doesn't complain
    (add-hook 'evil-org-mode-hook
              (lambda () (evil-org-set-key-theme)))
    (setq evil-org-key-theme '(navigation insert textobjects additional shift))
    (evil-org-agenda-set-keys))



  (general-define-key
   :states '(normal visual)
   :keymaps 'org-mode-map
   :prefix "SPC m"
   :non-normal-prefix "C-SPC m"
   ""    '(nil :which-key "Local Leader")
   "<tab>" #'org-cycle
   "RET" #'cpm/org-archive-done-tasks
   "SPC" #'org-toggle-checkbox
   "."   #'org-cycle-agenda-files
   "/"   #'org-sparse-tree
   "="   #'org-align-tags
   "?"   #'org-tags-view
   ":"   #'org-set-tags
   "a"   #'cpm/jump-to-org-super-agenda
   "A"   #'org-archive-subtree
   "b"   #'cpm/clone-buffer-and-narrow
   "B"   #'org-babel-tangle
   "c"   #'org-capture
   "d"   #'org-time-stamp
   "D"   #'org-deadline
   "e"   #'org-edit-special
   "f"   #'org-fill-paragraph
   "n"   #'cpm/narrow-or-widen-dwim
   "r"   #'org-refile
   "s"   #'org-schedule
   "t"   #'cpm/org-select-tags-completing-read
   "T"   #'org-todo
   "v"   #'variable-pitch-mode
   "l"   #'org-insert-link
   "L"   #'org-store-link
   "+"   #'org-timestamp-up-day
   "-"   #'org-timestamp-down-day
   "<"   #'org-metaleft
   ">"   #'org-metaright

   "i"  '(:ignore t :which-key "Insert...")
   "il" #'org-insert-link
   "if" #'org-footnote-new

   "R"  '(:ignore t :which-key "RevealJS..." )
   "Rr" #'cpm/reveal-to-html-open
   "Rs" #'cpm/narrowed-subtree-to-html
   "RS" #'org-reveal-export-current-subtree
   "Rp" #'cpm/reveal-to-pdf)

  (general-define-key
   :states '(normal motion emacs)
   :keymaps 'org-agenda-mode-map
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "" nil
   "<escape>" #'org-agenda-Quit
   "m"   #'org-agenda-month-view
   "C-j" #'org-agenda-next-item
   "C-k" #'org-agenda-previous-item
   "C-n" #'org-agenda-next-item
   "C-p" #'org-agenda-previous-item)

  (general-define-key :states '(normal) :keymaps 'org-mode-map
    "RET" 'org-open-at-point     ;; Open with return in evil
    "p"   'org-yank ;; better pasting behavior
    "s-J" 'crux-top-join-line)

  ;;   normal, insert, visual shortcuts
  (general-define-key :states '(normal insert visual) :keymaps 'org-mode-map
    "M-q" #'cpm/fill-or-unfill
    "C-t" #'transpose-chars)

  ;;   ;; normal & insert state shortcuts.
  (general-define-key :states '(normal insert) :keymaps 'org-mode-map
    ;; easily emphasize text
    ;; see https://emacs.stackexchange.com/questions/27645/unable-to-bind-emphasize-key-in-org-mode
    "s-b" (lambda () (interactive) (er/mark-word) (org-emphasize ?\*))
    "s-i" (lambda () (interactive) (er/mark-word) (org-emphasize ?\/))
    "s-l" (lambda () (interactive) (er/mark-word) (org-emphasize ?\=))
    ;; better pasting behavior in org-mode
    "s-v" 'org-yank)


  ;; end evil keybindings
  )

;;; End keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-keybindings)
