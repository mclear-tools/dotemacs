;; Keybindings for config
;;; General
(use-package general
  :demand t
  :config (general-override-mode))

(with-eval-after-load 'evil
;;; Namespaced Keybindings
;;;;  Which Key
 (use-package which-key
   :defer 1
   :diminish ""
   :config
   (setq which-key-special-keys nil)
   ;; Set the time delay (in seconds) for the which-key popup to appear.
   (setq which-key-idle-delay .3)
   (which-key-mode))

;;;;  Application Keybindings
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
  "as" 'sane-term
  "aw" 'wttrin
  )

;;;;  Buffer Keybindings
  (general-define-key
    :states '(normal motion visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC"

    "b"  '(:ignore t :which-key "Buffers")
    "bb" 'helm-mini
    "bc" 'cpm/copy-whole-buffer-to-clipboard
    "bD" 'kill-buffer-and-window
    "bd" 'cpm/kill-this-buffer
    "be" 'erase-buffer
    ;; "bf" 'cpm/browse-file-directory
    "bf" 'reveal-in-osx-finder
    "bj" 'cpm/jump-in-buffer
    "bk" 'evil-delete-buffer
    "bK" 'crux-kill-other-buffers
    "bn" 'evil-buffer-new
    "bN" 'cpm/new-buffer-new-frame
    "br" 'revert-buffer
    "bR" 'crux-rename-buffer-and-file
    "bt" 'cpm/open-dir-in-iterm
    )

;;;;  Comment Keybindings
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

;;;;  Config Keybindings
(general-define-key
  :states '(normal motion visual insert emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "C-SPC"

  "C"  '(:ignore t :which-key "Config")
  "Cc" 'goto-custom.el
  "Cd" 'goto-dotfiles.org
  "CD" 'goto-emacs-dir
  "Ce" 'goto-early-init.el
  "Cf" 'cpm/find-files-setup-config-directory
  "Ck" 'cpm/compile-dotemacs
  "CK" 'cpm/delete-byte-compiled-files
  "Cl" 'load-config
  "Ci" 'goto-init.el
  "CI" 'cpm/load-init-file
  "Co" 'goto-org-files
  "Cp" 'goto-pandoc-config
  "Cs" 'cpm/search-setup-config-files
  )

;;;;  File Keybindings
(general-define-key
  :states '(normal motion visual insert emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "C-SPC"

    "f"  '(:ignore t :which-key "Files")
    "ff" 'helm-find-files
    "fl" 'helm-locate
    "fo" 'crux-open-with
    "fs" 'save-buffer
    "fr" 'helm-recentf
    "fy" 'cpm/show-and-copy-buffer-filename
    )

;;;;  General Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

     "A" 'helm-apropos
     "B" #'cpm/dashboard
     "?" 'helm-descbinds
     "<SPC>" 'helm-M-x
     ;; "d" #'deer
     "d" #'dired-jump
     "D" #'dired-jump-other-window
     ;; "D" #'cpm/deer-split-window
     "E" 'cpm/call-emacs
     "e" 'server-edit
     "G" 'general-describe-keybindings
     "j" 'avy-goto-char
     "k" 'helm-show-kill-ring
     "l" 'helm-resume
     ;; "N" 'research-notes
     "n" 'big-notes
     "r" 'cpm/resume-last-jump
     "S" 'hydra-spelling/body
     ;; "W" 'woman
     "#" 'universal-argument
     "`" 'beacon-blink
     "'" 'shell-pop
     "." 'quick-commit
     ";" 'evil-commentary-line
     "[" 'cpm/previous-user-buffer
     "]" 'cpm/next-user-buffer
     "TAB" 'switch-to-previous-buffer
     )

;;;;  Compile Keybindings
(general-define-key
  :states '(normal motion visual insert emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "C-SPC"

  "M"   '(:ignore t :which-key "Make/Compile")
  "Mm"  'compile
  "MM"  'multi-compile-run
  "Me"  'compile-goto-error
  "Mk"  'kill-compilation
  "Mr"  'recompile
  "Mv"  'cpm/make-move
)

;;; Markdown Keybindings
  (use-package evil-markdown
    :ensure nil
    :load-path "~/.emacs.d/.local/elisp/evil-markdown"
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
    "x"  '(:ignore t :which-key "text")

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
    "cc"  'multi-compile-run
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
    "xb"  'markdown-insert-bold
    "xi"  'markdown-insert-italic
    "xc"  'markdown-insert-code
    "xC"  'markdown-insert-gfm-code-block
    "xq"  'markdown-insert-blockquote
    "xQ"  'markdown-blockquote-region
    "xp"  'markdown-insert-pre
    "xP"  'markdown-pre-region

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

;;;;; Package Keybindings
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

;;;;; Project Keybindings
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
      "pb"  'helm-projectile-switch-to-buffer
      "pc"  'projectile-compile-project
      "pC"  'desktop+-create
      "pd"  'helm-projectile-find-dir
      "pD"  'cpm/hydra-desktop
      ;; "pD"  'projectile-dired
      "pf"  'helm-projectile-find-file
      "pF"  #'cpm/helm-projectile-find-file-other-window
      "pg"  'cpm/goto-projects
      "ph"  'helm-projectile
      "pJ"  'bmkp-desktop-jump
      "pG"  'projectile-regenerate-tags
      "pI"  'projectile-invalidate-cache
      "pk"  'projectile-kill-buffers
      ;; "pl"  'desktop+-load
      "po"  'projectile-multi-occur
      "pp"  'helm-projectile-switch-project
      "pP"  'projectile-persp-switch-project
      ;; "pp"  'helm-persp-projectile-switch-project
      "pr"  'helm-projectile-recentf
      "pR"  'projectile-replace
      "pS"  'persp-switch
      "ps"  '(:ignore t :which-key "Frames")
      "pss"  'nameframe-switch-frame
      "ps1" #'cpm/load-phil101
      "ps2" #'cpm/load-phil232
      "ps5" #'cpm/load-phil105
      "ps8" #'cpm/load-phil871
      "psa" #'cpm/load-kant-apperception-substance
      "psb" #'(:ignore t :which-key "Books")
      "psba" #'cpm/load-kant-agency-book
      "psbr" #'cpm/load-kant-rationality-book
      "psc" #'cpm/load-emacs-config
      "psf" #'cpm/load-kant-free-thought
      "psr" #'cpm/load-kant-reflection
      "pst" #'cpm/load-org-agenda-todo
      "psw" #'cpm/load-website
      "psz" #'cpm/load-zettelkasten
      "pt"  #'org-projectile-helm-template-or-project
      "pT"  'projectile-find-test-file
      "pv"  'hydra-persp/body
      "pV"  'projectile-vc
      "py"  'projectile-find-tag
    )

;;;;; Quit Keybindings
 (general-define-key
  :states '(normal motion visual insert emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "C-SPC"

    "q"  '(:ignore t :which-key "Quit")
    "qq" 'cpm/save-desktop-save-buffers-kill-emacs
    "qQ" 'evil-quit-all
    "qr" 'restart-emacs
    )

;;;;; Search Keybindings
 (general-define-key
  :states '(normal motion visual insert emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "C-SPC"

    "s" '(:ignore t :which-key "Search")
    "sa" 'helm-org-rifle-agenda-files
    "sd" 'cpm/helm-files-search-current-directory ; search current buffer's directory
    "sD" 'cpm/helm-files-do-ag ; search with directory input
    "sb" 'helm-ag-buffers
    "sf" 'helm-do-ag-this-file
    "sj" 'cpm/forward-or-backward-sexp
    "sk" 'helm-show-kill-ring
    "sl" 'cpm/helm-list-search-buffers
    "so" 'helm-occur
    "sp" 'helm-ag-project-root
    "sr" #'vr/query-replace
    "sR" 'helm-org-rifle
    ;; "ss" 'helm-swoop-without-pre-input ;; search with swoop in file
    "ss" #'cpm/flyspell-ispell-goto-next-error ;; search for next spelling error
    "st" #'cpm/search-file-todo-markers ;; search for TODOs in file w/helm-ag
    "sT" #'cpm/search-todo-markers ;; search todo markers in directory w/helm-ag
    "/"  'cpm/helm-files-search-current-directory   ;; search in directory with word prompt
     )

;;;;; Toggle Keybindings
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
      "tT" 'helm-themes
      "tw" 'writeroom-mode
      "tz" 'zone
      ;; "tt" 'counsel-load-theme
    )

;;;;; User Keybindings
  (general-define-key
    :states '(normal motion visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC"

      "u"  '(:ignore t :which-key "User")
      "uA" '(:ignore t :which-key "Agenda Files")
      "uAa" #'cpm/goto-reading.org
      "uAc" #'cpm/goto-classes.org
      "uAf" #'cpm/goto-org-files
      "uAi" #'cpm/goto-inbox.org
      "uAl" #'cpm/goto-links.org
      "uAr" #'cpm/goto-reference.org
      "uAp" #'cpm/goto-projects.org
      "uAs" #'cpm/goto-someday.org
      "uAt" #'cpm/goto-todo.org
      "ua"  '(:ignore t :which-key "Agenda")
      "uaa" 'cpm/jump-to-org-super-agenda
      "uaw" 'cpm/jump-to-week-agenda
      "um" 'cpm/org-to-markdown
      "uc" 'cpm/pandoc-convert-to-pdf
      "uC" 'cpm/pandoc-command-line-convert-to-pdf
      "ug" 'org-mac-grab-link
      "ui" 'cpm/org-goto-inbox
      "uk" 'kill-compilation
      "ul" 'desktop-read
      "uo" 'cpm/markdown-to-org
      "up" 'run-pandoc
      "uP" 'cpm/pandoc-pdf-open
      "ur" 'remember-notes
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
      "ux" 'helm-bibtex
      )

;;;;; Version Control (Git) Keybindings
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

;;;;; Window Keybindings
(general-define-key
  :states '(normal motion visual insert emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "C-SPC"

  "0" 'select-window-0
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  "5" 'select-window-5

  "w"  '(:ignore t :which-key "Windows")
  "wa" 'ace-window
  "wf" 'cpm/toggle-window-split
  "wc" 'delete-window
  "wd" 'delete-window
  "wm" 'delete-other-windows
  "wr" 'cpm/rotate-windows
  "wR" 'cpm/rotate-windows-backward
  "wu" 'winner-undo
  "wU" 'winner-redo
  "wv" 'cpm/split-window-right-and-focus
  "wV" 'evil-window-vsplit
  "wx" 'cpm/window-exchange
  "w-" 'evil-window-split
  "w_" 'cpm/split-window-below-and-focus
  )

;;;;; WIki Keybindings
  (general-define-key
    :states '(normal motion visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC"

      "W" '(:ignore t :which-key "Wiki")
      ;; Keys in visualize mode
      "Wp" 'org-brain-add-parent
      "WP" 'org-brain-remove-parent
      "Wc" 'org-brain-add-child
      "WC" 'org-brain-remove-child
      "Wh" 'org-brain-new-child
      "Wn" 'org-brain-pin
      "Wt" 'org-brain-set-title
      "Wj" 'forward-button
      "Wk" 'backward-button
      "Wo" 'org-brain-goto-current
      "WO" 'org-brain-goto
      "Wv" 'org-brain-visualize
      "Wf" 'org-brain-add-friendship
      "WF" 'org-brain-remove-friendship
      "Wd" 'org-brain-delete-entry
      "Wl" 'org-brain-add-resource
      "Wa" 'org-brain-visualize-attach
      "WA" 'org-brain-archive
      "Wb" 'org-brain-visualize-back
      "W\C-y" 'org-brain-visualize-paste-resource
      "WT" 'org-brain-set-tags
      "Wq" 'org-brain-visualize-quit
      "Wr" 'org-brain-visualize-random
      "WR" 'org-brain-visualize-wander
      "Wm" 'org-brain-visualize-mind-map
      "W+" 'org-brain-visualize-add-grandchild
      "W-" 'org-brain-visualize-remove-grandchild
      "Wz" 'org-brain-visualize-add-grandparent
      "WZ" 'org-brain-visualize-remove-grandparent)


    ;;   "Wc" 'org-wiki-close
    ;;   "Wd" 'org-wiki-dired-all
    ;;   "Wk" 'org-wiki-close
    ;;   "Wh" 'org-wiki-helm
    ;;   "WH" 'org-wiki-help
    ;;   "WI" 'org-wiki-index
    ;;   "Wi" 'org-wiki-insert
    ;;   "Wl" 'org-wiki-link
    ;;   "Wm" 'org-wiki-make-page
    ;;   "Wv" 'org-wiki-server-toggle
    ;;   "We" 'org-wiki-export-html
    ;;   "Wp" 'org-wiki-panel
    ;;   "Ws" 'org-wiki-search
    ;;   "Wt" 'org-wiki-header
    ;; )

;;;;; Zettel Keybindings (Notes)
(general-define-key
  :prefix "SPC"
  :non-normal-prefix "C-SPC"
  :states '(normal visual insert motion emacs)
  :keymaps 'override
  "z"  '(nil :wk "Zettelkasten")
  "zc" '(zd-search-current-id :wk "search current id")
  "zd" '(cpm/zettel-dired :wk "zettel dired view")
  "zf" '(zd-avy-file-search :wk "avy file search")
  "zF" '(zd-avy-file-search-ace-window :wk "avy file other window")
  "zI" '(zd-find-file-id-insert :wk "insert id")
  "zi" '(zd-find-file-full-title-insert :wk "insert full title")
  "zl" '(zd-avy-link-search :wk "avy link search")
  "zn" '(zd-new-file :wk "new file")
  "zN" '(zd-new-file-and-link :wk "new file & link")
  "zo" '(zd-find-file :wk "find file")
  "zr" '(zd-file-rename :wk "rename")
  "zR" '(deft-refresh :wk "refresh")
  "zs" '(zd-deft-new-search :wk "new search")
  "zS" '(zd-search-at-point :wk "search at point")
  "zt" '(zd-avy-tag-search :wk "avy tag search")
  "zT" '(zd-tag-buffer :wk "tag list")
  )

;;; Org Keybindings
(use-package evil-org
  :ensure t
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . evil-org-set-key-theme))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-org-set-key-theme '(textobjects insert navigation additional shift)))


(general-define-key
:states '(normal visual)
:keymaps 'org-mode-map
:prefix "SPC m"
:non-normal-prefix "C-SPC m"
 ""    '(nil :which-key "Local Leader")
 "RET" #'cpm/org-archive-done-tasks
 "SPC" #'org-toggle-checkbox
 "."   #'org-cycle-agenda-files
 "/"   #'org-sparse-tree
 "="   #'org-align-all-tags
 "?"   #'org-tags-view
 ":"   #'org-set-tags
 "a"   #'super-jump-to-org-agenda
 "A"   #'org-archive-subtree
 "b"   #'org-tree-to-indirect-buffer
 "B"   #'org-babel-tangle
 "c"   #'org-capture
 "d"   #'org-time-stamp
 "D"   #'org-deadline
 "e"   #'org-edit-special
 "n"   #'cpm/narrow-or-widen-dwim
 "r"   #'org-refile
 "s"   #'org-schedule
 "t"   #'counsel-org-tag
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
     "s-b" (lambda () (interactive) (org-emphasize ?\*))
     "s-i" (lambda () (interactive) (org-emphasize ?\/))
     "s-l" (lambda () (interactive) (org-emphasize ?\=))
      ;; better pasting behavior in org-mode
     "s-v" 'org-yank)


  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'setup-keybindings)
