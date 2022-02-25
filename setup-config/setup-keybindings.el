;; Keybindings for config

;; I put (nearly) all keybindings here. This has its disadvantages (e.g. separating
;; functions or packages from keybindings) but it also makes it the place to go
;; to deal with all keybindings.

;;; Bind Key
;; Note that bind-key comes with use-package
(use-package bind-key
  :straight nil
  :config
  (setq bind-key-describe-special-forms t))

;;; Personal Keybindings Prefix
(defvar cpm-prefix "C-c C-SPC"
  "Prefix for all personal keybinds")

;;; Personal Keybindings by Group
;;;; Buffer Keys
(bind-keys :prefix-map cpm+buffer-keys
           :prefix (concat cpm-prefix " b")
           ("a" . ibuffer)
           ("b" . consult-buffer)
           ("c" . cpm/copy-whole-buffer-to-clipboard )
           ("d" . kill-buffer-and-window             )
           ("e" . cpm/email-save-and-kill            )
           ("E" . erase-buffer                       )
           ("f" . reveal-in-osx-finder               )
           ("i" . ibuffer-jump                       )
           ("j" . cpm/jump-in-buffer                 )
           ("k" . cpm/kill-this-buffer               )
           ("K" . crux-kill-other-buffers            )
           ("m" . consult-global-mark                )
           ("n" . cpm/create-new-buffer              )
           ("N" . cpm/new-buffer-new-frame           )
           ("p" . consult-project-buffer             )
           ("r" . revert-buffer                      )
           ("R" . crux-rename-buffer-and-file        )
           ("s" . consult-buffer-other-window        )
           ("t" . cpm/open-dir-in-iterm              )
           ("[" . cpm/previous-user-buffer           )
           ("]" . cpm/next-user-buffer               )
           ("{" . tab-bar-switch-to-prev-tab         )
           ("}" . tab-bar-switch-to-next-tab         )
           ("<backtab>" . crux-switch-to-previous-buffer)
           ("TAB" . cpm/tab-bar-select-tab-dwim      )
           ("C-M-t" . tab-bar-new-tab                )
           )

;;;; Comment Keybindings
(bind-keys :prefix-map cpm+comment-wrap-keys
           :prefix (concat cpm-prefix " c")
           ("c" . comment-dwim)
           ("d" . crux-duplicate-and-comment-current-line-or-region)
           ("l" . comment-line)
           ("o" . org-block-wrap)
           ("y" . cpm/yaml-wrap))

;;;; Config Keybindings
(bind-keys :prefix-map cpm+config-keys
           :prefix (concat cpm-prefix " C")
           ("c" . goto-custom.el                        )
           ("d" . goto-dotfiles.org                     )
           ("D" . goto-emacs-dir                        )
           ("E" . goto-cpm-elisp-dir                    )
           ("e" . goto-early-init.el                    )
           ("f" . cpm/find-files-setup-config-directory )
           ("k" . cpm/byte-compile-dotemacs             )
           ("K" . cpm/delete-byte-compiled-files        )
           ("l" . load-config                           )
           ("i" . goto-init.el                          )
           ("I" . cpm/load-init-file                    )
           ("o" . goto-org-files                        )
           ("p" . goto-pandoc-config                    )
           ("s" . cpm/search-setup-config-files         ))

;;;; Compile Keybindings
(bind-keys :prefix-map cpm+compile-keys
           :prefix (concat cpm-prefix " M")
           ("m"  . compile                  )
           ("M"  . multi-compile-run        )
           ("e"  . compile-goto-error       )
           ("k"  . cpm/compile-next-makefile)
           ("K"  . kill-compilation         )
           ("r"  . recompile                )
           ("v"  . cpm/make-move            ))

;;;; Eval Keybindings
(bind-keys :prefix-map cpm+eval-keys
           :prefix (concat cpm-prefix " e")
           ("b"  . eval-buffer )
           ("c"  . cpm/eval-current-form)
           ("e"  . eval-last-sexp)
           ("f"  . eval-defun)
           ("r"  . eval-last-region))

;;;; File Keybindings
(bind-keys :prefix-map cpm+file-keys
           :prefix (concat cpm-prefix " f")
           ("b" . consult-bookmark                 )
           ("f" . find-file                        )
           ("l" . consult-locate                   )
           ("o" . crux-open-with                   )
           ("s" . save-buffer                      )
           ("r" . consult-recent-file              )
           ("y" . cpm/show-and-copy-buffer-filename))

;;;; Quit Keybindings
(bind-keys :prefix-map cpm+quit-keys
           :prefix (concat cpm-prefix " q")
           ("d" . cpm/kill-emacs-capture-daemon)
           ("q" . save-buffers-kill-emacs      )
           ("Q" . cpm/kill-all-emacsen         )
           ("r" . restart-emacs               ))

;;;; Search Keybindings
(bind-keys :prefix-map cpm+search-keys
           :prefix (concat cpm-prefix " s")
           ("a" . consult-org-agenda           )
           ;; search current buffer's directory
           ("d" . consult-ripgrep              )
           ;; search with directory input
           ("D" . cpm/search-in-input-dir      )
           ("b" . consult-multi-occur          )
           ("f" . consult-line                 )
           ("h" . consult-org-heading          )
           ("j" . cpm/forward-or-backward-sexp )
           ("k" . consult-yank-pop             )
           ("l" . selectrum-repeat             )
           ("n" . consult-notes-search-all     )
           ("p" . consult-line-symbol-at-point )
           ("r" . vr/query-replace             )
           ("s" . consult-line                 )
           ;; search for next spelling error
           ("S" . cpm/flyspell-ispell-goto-next-error)
           ("t" . cpm/hydra-todo/body))

;;;; Toggle Keybindings
(bind-keys :prefix-map cpm+toggle-keys
           :prefix (concat cpm-prefix " t")
           ("a" . company-mode                )
           ("b" . buffer-line-mode            )
           ("g" . git-gutter-mode             )
           ("h" . hl-line-mode                )
           ("H" . hidden-mode-line-mode       )
           ("e" . toggle-indicate-empty-lines )
           ("E" . eldoc-mode                  )
           ("m" . cpm/toggle-display-markup   )
           ("n" . display-line-numbers-mode   )
           ("N" . org-numbers-overlay-mode    )
           ("o" . imenu-list-smart-toggle     )
           ("p" . puni-global-mode            )
           ("P" . show-paren-mode             )
           ("r" . rainbow-identifiers-mode    )
           ("s" . flyspell-mode               )
           ("S" . ispell-buffer               )
           ("t" . toggle-dark-light-theme     )
           ("T" . cpm/load-theme              )
           ("w" . writeroom-mode              )
           ("z" . zone                        ))

;;;; User Keybindings
(bind-keys :prefix-map cpm+user-keys
           :prefix (concat cpm-prefix " u")
           ("a" .  cpm/jump-to-org-super-agenda                 )
           ("c" . cpm/find-files-setup-config-directory         )
           ("C" . cpm/search-setup-config-files                 )
           ("d" .  osx-dictionary-search-input                  )
           ("m" .  cpm/org-to-markdown                          )
           ("g" .  org-mac-grab-link                            )
           ("h" .  cpm/org-export-to-buffer-html-as-body        )
           ("i" .  cpm/org-goto-inbox                           )
           ("k" .  kill-compilation                             )
           ("l" .  desktop-read                                 )
           ("o" .  cpm/markdown-to-org                          )
           ("O" .  cpm/goto-org-files                           )
           ("p" .  run-pandoc                                   )
           ("P" .  cpm/pandoc-pdf-open                          )
           ("s" .  sb-expand-current-file                       )
           ("S" .  just-one-space                               )
           ("t" .  cpm/jump-to-org-agenda-all-todos             )
           ("j" .  cpm/goto-journal                             )
           ("u" .  cpm/straight-update-packages-asynchronously  )
           ("w" .  count-words                                  )
           ("W" .  cpm/jump-to-week-agenda                      ))

;;;; Version Control (Git) Keybindings
(bind-keys :prefix-map  cpm+vc-keys
           :prefix (concat cpm-prefix " g")
           ("b" .  magit-blame                 )
           ("c" .  magit-commit                )
           ("d" .  magit-diff                  )
           ("h" .  hydra-git-gutter/body       )
           ("l" .  magit-log                   )
           ;; show history of selected region
           ("L" .  magit-log-buffer-file       )
           ("n" .  git-gutter:next-hunk        )
           ("p" .  git-gutter:previous-hunk    )
           ;; quick commit file
           ("q" .  vc-next-action              )
           ("r" .  magit-reflog                )
           ("s" .  magit-status                ))

;;;; Window Keybindings
(bind-keys :prefix-map cpm+window-keys
           :prefix (concat cpm-prefix " w")
           ("0" .  winum-select-window-0           )
           ("1" .  winum-select-window-1           )
           ("2" .  winum-select-window-2           )
           ("3" .  winum-select-window-3           )
           ("4" .  winum-select-window-4           )
           ("a" .  ace-window                      )
           ("f" .  cpm/toggle-window-split         )
           ("c" .  delete-window                   )
           ("d" .  delete-window                   )
           ("h" .  cpm/split-window-below-and-focus)
           ("H" .  split-window-below              )
           ("m" .  delete-other-windows            )
           ("o" .  cpm/other-window                )
           ("r" .  cpm/rotate-windows              )
           ("R" .  cpm/rotate-windows-backward     )
           ("t" .  tear-off-window                 )
           ("u" .  winner-undo                     )
           ("U" .  winner-redo                     )
           ("v" .  cpm/split-window-right-and-focus)
           ("V" .  split-window-right              )
           ("w" .  cpm/other-window                )
           ("x" .  cpm/window-exchange-buffer      )
           ("-" .  split-window-below              )
           ("_" .  cpm/split-window-below-and-focus))

;;;; Zettelkasten/Notes/Wiki
(bind-keys :prefix-map cpm+notes-keys
           :prefix (concat cpm-prefix " n")
           ("c"  .  org-roam-capture        )
           ("C"  .  citar-open-notes        )
           ("i"  .  org-roam-node-insert    )
           ("f"  .  org-roam-node-find      )
           ("g"  .  org-roam-graph          )
           ("n"  .  consult-notes           )
           ("N"  .  org-roam--new-file-named)
           ("r"  .  cpm/find-note-relation  )
           ("s"  .  consult-notes-search-all)
           ("t"  .  org-roam-buffer-toggle))



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
;;   ;; normal & insert state shortcuts.


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




;;; End keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-keybindings)
