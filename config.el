;;; config.el --- summary -*- lexical-binding: t -*-

;; Author: Colin McLear
;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Personal config file
;; This file contains all settings that aren't an official part of 𝛌-Emacs.

;;; Code:
;;;; Personal Information

;; Give Emacs some personal info
(setq user-full-name "Colin McLear"
      user-mail-address "mclear@fastmail.com")

;;;; Private File
;; where to store private or "secret" info
(let ((private (expand-file-name "private.el" lem-user-dir)))
  (if (file-exists-p private)
	  (load-file private)))

;;;; User Vars

;;;;; Tab workspaces

(with-eval-after-load 'tabspaces
  (setopt tabspaces-session-mode t
          tabspaces-session-file (concat lem-cache-dir "tabsession.el")))

;;;;; Fullscreen Frame
  ;; Need to do this after startup to avoid flashing the screen for some reason
  (push '(fullscreen . maximized) initial-frame-alist))

;;;;; User Paths
  ;; Set exec-path-from-shell
  (setopt exec-path-from-shell-variables
          '("PATH" "MANPATH" "XDG_CONFIG_HOME" "BEETSDIR"))

;;;;; Shell
(setq-default shell-file-name "/opt/homebrew/bin/zsh")
(setq explicit-shell-file-name "/opt/homebrew/bin/zsh")

;;;;; Set User Elisp Dir
(setq lem-user-elisp-dir "~/bin/lisp-projects/")

;;;;; Citations
(setq lem-bibliography (concat (getenv "HOME") "/Dropbox/Work/bibfile.bib"))

(setq lem-bib-notes (concat (getenv "HOME") "/Dropbox/Work/projects/notebook/content-org/ref-notes"))

(setq lem-citar-note  "${author-or-editor} (${year}): ${title}\n#+ROAM_KEY: [cite:@${=key=}]\n#+SETUPFILE: ../hugo-notebook-setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n\n#+BEGIN_SRC emacs-lisp :exports none\n(insert \"#+BEGIN_SRC bibtex\")\n(newline)\n(citar--insert-bibtex \"${=key=}\")\n(insert \"#+END_SRC\")\n#+END_SRC\n")

;; Set citar library path
(with-eval-after-load 'citar
  (setq citar-library-paths '("~/Library/Mobile Documents/iCloud~com~sonnysoftware~bot/Documents/be-library")))

;;;;; Notes
;; I use hugo so define a setup file variable
(defvar hugo-notebook-setup-file "~/Dropbox/Work/projects/notebook/content-org/hugo-notebook-setup.org"
  "Variable for notebook setup using hugo.")

;; Denote settings
(setopt lem-notes-dir (concat (getenv "HOME") "/Documents/notes/")
        denote-directory (concat lem-notes-dir "denotes/")
        denote-known-keywords '("emacs" "teaching" "unl" "workbook")
        denote-prompts '(title keywords subdirectory)
        consult-notes-denote-display-id t
        citar-denote-subdir t)

;; Provide nicer spacing for note front matter
(setq denote-org-front-matter
      "#+title:     %s
#+date:    %s
#+filetags:    %s
#+identifier: %s
\n")

;; Consult Notes Setup
(with-eval-after-load 'consult-notes

  (setopt consult-notes-file-dir-sources
          `(("Agenda Files"    ?a ,(car org-agenda-files))
            ("Refile Notes"    ?r ,(concat lem-notes-dir "refile-notes/"))))

  (setopt consult-notes-org-headings-files '("~/Dropbox/org-files/inbox.org"
                                             "~/Dropbox/org-files/reading.org"
                                             "~/Dropbox/org-files/writing.org"
                                             "~/Dropbox/org-files/reference.org"
                                             "~/Dropbox/org-files/music.org"))

  (defun cpm-consult-notes--file-dir-annotate (name dir cand)
    "Annotate file CAND with its directory DIR, size, and modification time."
    (let* ((file  (concat dir cand))
           (attrs (file-attributes file))
           (fsize (file-size-human-readable (file-attribute-size attrs)))
           (ftime (consult-notes--time (file-attribute-modification-time attrs))))
      (put-text-property 0 (length dir)  'face 'consult-notes-name dir)
      (put-text-property 0 (length fsize) 'face 'consult-notes-size fsize)
      (put-text-property 0 (length ftime) 'face 'consult-notes-time ftime)
      (format "%8s %12s" fsize ftime)))

  (setopt consult-notes-file-dir-annotate-function #'cpm-consult-notes--file-dir-annotate)
  (consult-notes-denote-mode)
  (consult-notes-org-headings-mode))

;; Old sources
;; ("Zettel"          ?z ,(concat lem-notes-dir "zettel/"))
;; ("Lecture Notes"   ?l ,(concat lem-notes-dir "lecture-notes/"))
;; ("Workbook"        ?w ,(concat lem-notes-dir "workbook/"))
;; ("Reference Notes" ?r ,(concat lem-notes-dir "ref-notes/"))
;; ("Refile"          ?R ,(concat lem-notes-dir "refile-notes/"))

;; ;; Org-Roam Notes
;; (require 'lem-setup-org-roam)
;; (setq org-roam-directory lem-notes-dir)

;;;;; Org Directories
(setopt org-directory "~/Dropbox/org-files/"
        org-default-notes-file (concat org-directory "inbox.org")
        org-agenda-files (list org-directory))

;;;;; Straight Package Manager
(with-eval-after-load 'straight
  ;; Don't walk straight repos
  (push "straight" vc-directory-exclusion-list)
  ;; Delete .DS_Store before prune
  (advice-add 'straight-prune-build :before #'(lambda () (move-file-to-trash (concat lem-var-dir "straight/build/.DS_Store")))))

;;;;; Set Splash Footer
(setq lem-splash-footer  "Aus so krummem Holze, als woraus der Mensch gemacht ist, kann nichts ganz Gerades gezimmert werden")

;;;;; Markdown Command
(setq markdown-command
      (concat
       "/usr/local/bin/pandoc"
       " --from=markdown --to=html"
       " --standalone --mathjax --highlight-style=pygments"
       " --css=~/.pandoc/pandoc.css"
       " --quiet"
       " --number-sections"
       " --lua-filter=~/dotfiles/pandoc/cutsection.lua"
       " --lua-filter=~/dotfiles/pandoc/cuthead.lua"
       " --lua-filter=~/dotfiles/pandoc/date.lua"
       ;; " --metadata-file=~/dotfiles/pandoc/metadata.yml"
       " --metadata=reference-section-title:References"
       " --citeproc"
       " --bibliography=~/Dropbox/Work/bibfile.bib"))

;;;; Load Modules
;; Load modules in stages for a shorter init time. We load core modules first,
;; then more expensive modules after init, with the rest loaded after startup
;; has completed.

;;;;; Load Base Modules
(message "
;; ======================================================
;; *Loading 𝛌-Emacs Base Modules*
;; ======================================================")
(measure-time
 (cl-dolist (mod (list
                  ;; Base modules
                  'lem-setup-libraries
                  'lem-setup-settings
                  'lem-setup-functions
                  'lem-setup-macros
                  'lem-setup-scratch

                  ;; Basic UI modules
                  'lem-setup-frames
                  'lem-setup-windows
                  'lem-setup-buffers
                  'lem-setup-fonts
                  'lem-setup-faces
                  'lem-setup-theme))
   (require mod)))

;;;;; Load After-Init Modules
(defun lem-user-config-after-init ()
  "Modules loaded after init."
  (message "
  ;; ======================================================
  ;; *Loading 𝛌-Emacs after-init Modules*
  ;; ======================================================")
  (measure-time (cl-dolist (mod (list
                                 ;; Splash
                                 'lem-setup-splash

                                 ;; Completion & Keybinds
                                 'lem-setup-completion
                                 'lem-setup-keybindings

                                 ;; Navigation & Search modules
                                 'lem-setup-navigation
                                 'lem-setup-dired
                                 'lem-setup-search

                                 ;; Modal
                                 'cpm-setup-meow

                                 ;; Project & Tab/Workspace modules
                                 'lem-setup-vc
                                 'lem-setup-projects
                                 'lem-setup-tabs
                                 'cpm-setup-workspaces))
                  (require mod))))
(add-hook 'after-init-hook #'lem-user-config-after-init)

;;;;; Load After-Startup Modules
(defun lem-user-config-after-startup ()

  "Modules loaded after Emacs startup."
  (message "
  ;; ======================================================
  ;; *Loading 𝛌-Emacs after-startup Modules*
  ;; ======================================================")
  (measure-time (cl-dolist (mod (list
                                 ;; Other UI/UX
                                 'lem-setup-help
                                 'lem-setup-colors
                                 'cpm-setup-modeline

                                 ;; Server
                                 'lem-setup-server

                                 ;; Writing modules
                                 'lem-setup-writing
                                 'lem-setup-notes
                                 'lem-setup-citation

                                 ;; Programming modules
                                 'lem-setup-programming
                                 'lem-setup-debug
                                 'lem-setup-skeleton

                                 ;; Shell & Terminal
                                 'lem-setup-shell
                                 'lem-setup-eshell

                                 ;; Org modules
                                 'lem-setup-org-base
                                 'lem-setup-org-settings
                                 'lem-setup-org-extensions

                                 ;; Productivity
                                 'lem-setup-pdf
                                 'lem-setup-elfeed

                                 ;; OS settings
                                 ;; loads only if on macos
                                 (when sys-mac
                                   'lem-setup-macos)

                                 ;; Personal modules
                                 'cpm-setup-email
                                 'cpm-setup-org
                                 'cpm-setup-calendars
                                 'cpm-setup-multi-compile
                                 'cpm-setup-teaching))
                  (require mod))))
(add-hook 'emacs-startup-hook #'lem-user-config-after-startup)

;;;;; Scratch Directory
(with-eval-after-load 'lem-setup-scratch
  (setopt lem-scratch-default-dir lem-scratch-save-dir))

;;;; User Keybindings
(setopt lem-prefix "C-c C-SPC")

;; Make sure to load these after general keybindings
(with-eval-after-load 'lem-setup-keybindings
  (bind-key (concat lem-prefix " \\")  #'lem-toggle-eshell)
  (bind-keys :prefix-map lem+user-keys
             :prefix (concat lem-prefix " u"               )
             ;; Workspaces
             ("a" . cpm-open-agenda-in-workspace           )
             ("c" . cpm-open-emacsd-in-workspace           )
             ("e" . cpm-open-new-eshell-and-workspace      )
             ("h" . cpm-go-home                            )
             ("f" . cpm-open-elfeed-in-workspace           )
             ("m" . cpm-open-email-in-workspace            )
             ("n" . cpm-open-notes-in-workspace            )
             ("t" . cpm-open-new-terminal-and-workspace    )
             ;; Citation
             ("x" .  citar-insert-citation                 ))

  ;; Package management
  (bind-keys :prefix-map lem+packages
             :prefix (concat lem-prefix " u p")
             ("p" . package-list-packages)
             ("u" . package-list-upgradable-packages))

  (bind-keys :prefix-map lem+jump-to
             :prefix (concat lem-prefix " u j"             )
             ;; Jump to
             ("a" .  lem-jump-to-org-dashboard             )
             ("W" .  lem-jump-to-week-agenda               )
             ("t" .  lem-jump-to-org-agenda-all-todos      )
             ("j" .  lem-goto-journal                      )
             ("i" .  lem-org-goto-inbox                    )
             ("O" .  lem-goto-org-files                    ))

  (bind-keys :prefix-map lem+text
             :prefix (concat lem-prefix " u T"             )
             ;; Text manipulation
             ("S" .  just-one-space                        )
             ("h" .  lem-org-export-to-buffer-html-as-body )
             ("d" .  osx-dictionary-search-input           )
             ("w" .  count-words                           )
             ("o" .  lem-markdown-to-org                   )
             ("m" .  lem-org-to-markdown                   )))

;;;; User Packages

;; Here's a list of packages that I may want to install...
;; macrostep
;; tldr

;;;;; Programming Modes

;;;;;; Applescript
(use-package applescript-mode
  :mode (("\\.scpt\\'" . applescript-mode)
         ("\\.applescript\\'"       . applescript-mode))
  :commands (applescript-mode))

;;;;;; Haskell
(use-package haskell-mode
  :commands haskell-mode)

;;;;;; HTML
(use-package web-mode
  :commands (web-mode)
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t))

;;;;;; Lua
(use-package lua-mode
  :commands lua-mode
  :init
  (dolist (pattern '("\\.lua\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'lua-mode))))

;;;;;; PHP
(use-package php-mode
  :commands php-mode
  :init
  (dolist (pattern '("\\.php\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'php-mode))))

;;;;;; YAML
(use-package yaml-mode
  :commands yaml-mode
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

;;;;;; Plist
(use-package plist-mode
  ;; :straight nil
  :load-path "~/bin/lisp-projects/plist-mode"
  :commands (plist-mode))

;;;;;; Vim
(use-package vimrc-mode
  :commands vimrc-mode)

;;;;; Documentation
(use-package tldr
  :commands (tldr tldr-update-docs)
  :init
  (with-eval-after-load 'evil
    (evil-set-initial-state 'tldr-mode 'emacs))
  :config
  (setq tldr-directory-path (expand-file-name "tldr/" lem-etc-dir)))

;;;;; Popper Shells

(with-eval-after-load 'popper
  ;; Match eshell, shell, term and/or vterm buffers
  (setopt popper-reference-buffers
          (append popper-reference-buffers
                  '(;; make all shell/terminals popups
                    "^\\*eshell.*\\*$" eshell-mode
                    "^\\*shell.*\\*$"  shell-mode
                    "^\\*term.*\\*$"   term-mode
                    "^\\*vterm.*\\*$"  vterm-mode))))


;;;;; Elfeed

;; Set elfeed feeds
(with-eval-after-load 'elfeed
  (setopt elfeed-feeds '("http://nullprogram.com/feed/"
                         ("https://planet.emacslife.com/atom.xml" emacs)
                         ("https://tilde.town/~ramin_hal9001/atom.xml" emacs)
                         ("https://www.mail-archive.com/emacs-devel@gnu.org/maillist.xml" emacs)
                         ("https://karthinks.com/index.xml" emacs))))

;;;;; Eshell Aliases
(with-eval-after-load 'eshell
  (lem-set-eshell-alias
   "pg" "lem-goto-projects"
   "pd" "cd ~/Dropbox/Work/projects"))


;;;;; Zotero Org Zotxt Inferface
(use-package zotxt
  :commands (org-zotxt-insert-reference-link
             org-zotxt-open-attachment
             rg-zotxt-update-reference-link-at-point)
  :init
  (unless (package-installed-p 'zotxt-emacs)
    (package-vc-install "https://github.com/egh/zotxt-emacs.git"))
  (add-hook 'org-mode #'org-zotxt-mode))

;;;;; Capf-bibtex

(use-package capf-bibtex
  :hook ((org-mode markdown-mode tex-mode latex-mode reftex-mode) . capf-bibtex-mode)
  :custom
  (capf-bibtex-bibliography
   '("~/Dropbox/Work/bibfile.bib"))
  :init
  (unless (package-installed-p 'capf-bibtex)
    (package-vc-install "https://github.com/mclear-tools/capf-bibtex.git")))

;;;;; Pulsing Cursor

(use-package pulsing-cursor
  :defer 1
  :custom-face
  (pulsing-cursor-overlay-face1 ((t (:inherit match))))
  :custom
  (pulsing-cursor-delay 1.0)
  (pulsing-cursor-interval .5)
  (pulsing-cursor-blinks 5)
  :init
  (unless (package-installed-p 'pulsing-cursor)
    (package-vc-install "https://github.com/jasonjckn/pulsing-cursor"))
  :config
  (pulsing-cursor-mode +1))

;;;;; Org Modern Indent
;; Make org-modern work better with org-indent
(use-package org-modern-indent
  :hook (org-indent-mode . org-modern-indent-mode)
  :custom-face
  (org-modern-indent-line ((t (:height 1.0 :inherit lem-ui-default-font :inherit lambda-meek))))
  :init
  (unless (package-installed-p 'org-modern-indent)
    (package-vc-install "https://github.com/jdtsmith/org-modern-indent.git")))

;;;;; Org Devonthink Integration
(use-package org-devonthink
  :when sys-mac
  :commands (org-insert-dtp-link org-dtp-store-link)
  :init
  (unless (package-installed-p 'org-devonthink)
    (package-vc-install "https://github.com/lasvice/org-devonthink")))

;;;;; Command log mode
(use-package command-log-mode
  :commands (command-log-mode)
  :init
  (unless (package-installed-p 'command-log-mode)
    (package-vc-install "https://github.com/lewang/command-log-mode.git")))

;;;; User Functions

;;;;; Tab Numbers In Tab-Bar
;; Tab bar numbers
(defface lem-tab-bar-numbers
  '((t
     :inherit lambda-strong
     :height 1.1))
  "Face for tab numbers in both active and inactive tabs.")

(defvar lem-box-numbers-alist
  '((1 . "􀃊")
    (2 . "􀃌")
    (3 . "􀃎")
    (4 . "􀘙")
    (5 . "􀃒")
    (6 . "􀑵")
    (7 . "􀃖")
    (8 . "􀃘")
    (9 . "􀑷")
    (0 . "􀃈"))
  "Alist of integers to strings of SF Symbols with numbers in boxes.")

(defvar lem-filled-box-numbers-alist
  '((1 . "􀃋")
    (2 . "􀃍")
    (3 . "􀃏")
    (4 . "􀘚")
    (5 . "􀃓")
    (6 . "􀑶")
    (7 . "􀃗")
    (8 . "􀃙")
    (9 . "􀑸")
    (0 . "􀃉"))
  "Alist of integers to strings of SF Symbols with numbers in filled boxes.")

(defun lem-tab-bar-tab-name-format (tab i)
  (let* ((current-p (eq (car tab) 'current-tab))
         (tab-num (when (and tab-bar-tab-hints (< i 9))
                    (alist-get i (if current-p lem-filled-box-numbers-alist lem-box-numbers-alist)))))
    (concat
     " "
     (if current-p (propertize tab-num 'face 'lem-tab-bar-numbers) tab-num)
     " "
     (if current-p (propertize (alist-get 'name tab) 'face '(:inherit lambda-fg :underline t))
       (alist-get 'name tab))
     (or (and tab-bar-close-button-show
              (not (eq tab-bar-close-button-show
                       (if current-p 'non-selected 'selected)))
              tab-bar-close-button)
         ""))))

(setopt tab-bar-tab-name-format-function #'lem-tab-bar-tab-name-format)

;;;;; Kill Process
;; https://xenodium.com/emacs-quick-kill-process/
(use-package proced
  :functions cpm-proced--hook-fun
  :commands (proced cpm-quick-kill-process)
  :hook (proced-mode . cpm-proced--hook-fun)
  :config
  (defun cpm-proced--hook-fun ()
    (setq proced-auto-update-flag t))

  (require 'map)
  (require 'proced)
  (require 'seq)

  (defun cpm-quick-kill-process ()
    (interactive)
    (let* ((pid-width 5)
           (comm-width 25)
           (user-width 10)
           (processes (proced-process-attributes))
           (candidates
            (mapcar (lambda (attributes)
                      (let* ((process (cdr attributes))
                             (pid (format (format "%%%ds" pid-width) (map-elt process 'pid)))
                             (user (format (format "%%-%ds" user-width)
                                           (truncate-string-to-width
                                            (map-elt process 'user) user-width nil nil t)))
                             (comm (format (format "%%-%ds" comm-width)
                                           (truncate-string-to-width
                                            (map-elt process 'comm) comm-width nil nil t)))
                             (args-width (- (window-width) (+ pid-width user-width comm-width 3)))
                             (args (map-elt process 'args)))
                        (cons (if args
                                  (format "%s %s %s %s" pid user comm (truncate-string-to-width args args-width nil nil t))
                                (format "%s %s %s" pid user comm))
                              process)))
                    processes))
           (selection (map-elt candidates
                               (completing-read "kill process: "
                                                (seq-sort
                                                 (lambda (p1 p2)
                                                   (string-lessp (nth 2 (split-string (string-trim (car p1))))
                                                                 (nth 2 (split-string (string-trim (car p2))))))
                                                 candidates) nil t)))
           (prompt-title (format "%s %s %s"
                                 (map-elt selection 'pid)
                                 (map-elt selection 'user)
                                 (map-elt selection 'comm))))
      (when (y-or-n-p (format "Kill? %s" prompt-title))
        (if (eq (signal-process (map-elt selection 'pid) 9) 0)
            (message "killed: %s" prompt-title)
          (message "error: could not kill %s" prompt-title))))))

;;;;; Recenter Buffer
;; https://www.n16f.net/blog/eye-level-window-centering-in-emacs/
;; NOTE: As value approaches 1 "center" goes to the bottom of the screen
;; A value between.15 and .25 seems best
(defcustom lem-recenter-buffer-eye-level 0.25  "The relative position of the line considered as eye level in the
current window, as a ratio between 0 and 1.")

(defun lem-recenter-buffer ()
  "Scroll the window so that the current line is at eye level."
  (interactive)
  (let ((line (round (* (window-height) lem-recenter-buffer-eye-level))))
    (recenter line)
    (pulse-line)))

(global-set-key (kbd "C-l") 'lem-recenter-buffer)
(global-set-key (kbd "C-S-l") 'recenter-top-bottom)

;;;;; User Goto Functions
(defun goto-dotfiles.org ()
  "Open dotfiles.org file."
  (interactive)
  (find-file "~/dotfiles/dotfiles.org"))

(defun goto-pandoc-config ()
  "Open pandoc metadata file."
  (interactive)
  (find-file "~/.pandoc/metadata.yml"))

;;;;; Save Buffer & Exit Emacsclient
(defun lem-save-and-kill-emacsclient ()
  "Save buffer and exit emacsclient."
  (interactive)
  (save-buffer)
  (server-edit))

;; Disable emacs vc for git; just use magit!
;; (setq vc-handled-backends (delq 'Git vc-handled-backends))

;;;;; Set Fonts
;; Set the default face. The default face is the basis for most other
;; faces used in Emacs. A "face" is a configuration including font,
;; font size, foreground and background colors and other attributes.
;; The fixed-pitch and fixed-pitch-serif faces are monospace faces
;; generally used as the default face for code. The variable-pitch
;; face is used when `variable-pitch-mode' is turned on, generally
;; whenever a non-monospace face is preferred.
(defun lem-user-fonts ()
  "Set user fonts."
  (cond ((and (lem-font-available-p "SF Mono")
              (lem-font-available-p "SF Pro Text"))
         (set-face-attribute 'default           nil :font "SF Mono-13")
         (set-face-attribute 'fixed-pitch       nil :inherit 'default)
         (set-face-attribute 'fixed-pitch-serif nil :inherit 'default)
         (set-face-attribute 'variable-pitch    nil :font "SF Pro Text-14"))))
(with-eval-after-load 'lem-setup-functions
  (add-hook 'emacs-startup-hook #'lem-user-fonts))

;;;;; SHR Rendering
(setopt shr-use-fonts nil)

;;;;; Davmail & Mu4e
(defun cpm-start-davmail ()
  "Start a headless Davmail process in iterm."
  (interactive)
  (cond ((not (string> (shell-command-to-string "jps -v | grep davmail") ""))
         (do-applescript "
tell application \"iTerm\"
	delay 0.35
	if (count of windows) = 0 then
		create window with default profile
	else
		tell current window
			create tab with default profile
		end tell
	end if
	tell current session of current tab of current window
		write text \"davmail\"
	end tell
end tell")
         (do-applescript "tell application \"Emacs\"
activate
end tell")
         ;; (async-shell-command-no-window "open -a iterm")
         ;; (shell-command "sleep .5")
         ;; (iterm-send-string "davmail")

         ;; (let ((tname (cdr (assoc 'name (tab-bar--current-tab)))))
         ;;   (cond ((not (get-buffer "*davmail*"))
         ;;          ;; need to use vterm otherwise output speed is too slow
         ;;          (lem-run-in-vterm "davmail")
         ;;          (with-current-buffer "*davmail*"
         ;;            ;; don't use goto address mode in davmail buffer
         ;;            (goto-address-mode -1))
         ;;          ;; (ansi-term "davmail" "davmail")
         ;;          (if (string= tname "Home")
         ;;              (switch-to-buffer "*splash*")
         ;;            (lem-previous-user-buffer))
         (message "Davmail started."))
        (t
         (message "Davmail process is already running!"))))

(defun cpm-stop-davmail ()
  "Kill davmail headless server."
  (interactive)
  (cond ((string> (shell-command-to-string "jps -v | grep davmail") "")
         ;; (shell-command "osascript -e 'quit app \"iterm2\"'")
         (shell-command "jps -v | grep davmail | awk {'print $1'} | xargs kill")
         ;; (get-buffer "*davmail*")
         ;; (kill-buffer "*davmail*")
         (do-applescript "tell application \"Emacs\"
activate
end tell")
         (message "Davmail stopped."))
        (t
         (message "There is no Davmail process!"))))

(defun cpm-restart-davmail ()
  (interactive)
  (progn
    (cpm-stop-davmail)
    (cpm-start-davmail)
    ;; (get-buffer "*davmail*")
    ;; (kill-buffer "*davmail*")
    ;; (lem-run-in-vterm "davmail")
    ;; (with-current-buffer "*davmail*"
    ;;   (goto-address-mode -1))
    (message "Davmail restarted.")))

;; Startup & quit hooks
(with-eval-after-load 'mu4e
  (progn
    ;; initiate davmail process
    (cpm-start-davmail)
    (do-applescript "tell application \"Emacs\"
activate
end tell")
    ;; add to popper buffer
    ;; (add-to-list 'popper-reference-buffers '("\\*davmail\\*" . hide))
    ))
;; Kill davmail on quit
(add-hook 'kill-emacs-hook #'cpm-stop-davmail)


;;;;; Package.el Helper Functions

;; Show Packages Ready for Updating
;; See https://emacs.stackexchange.com/questions/38206/upgrading-packages-automatically
(defun package-list-upgradable-packages ()
  "Refresh and list upgradable packages."
  (interactive)
  (save-window-excursion
    (let (package-menu-async)
      (package-list-packages)))
  (pop-to-buffer "*Packages*")
  (delete-other-windows)
  (package-menu-filter-upgradable))

;; Show Package vc log
(defun package-browse-vc-log (desc)
  "Open a magit log buffer in popper window for package under point.
DESC must be a `package-desc' object and must have a link to a recognized repo host."
  (interactive (list (package--query-desc))
               package-menu-mode)
  (require 'popper)
  (unless desc
    (user-error "No package here"))
  (let* ((url (cdr (assoc :url (package-desc-extras desc))))
         (tmp "/tmp/")
         (tmpd (concat tmp "tmpdir/"))
         (vc-log-short-style '(file))
         (vc-git-root-log-format '("%ad: %d%h - %s" "\\(?1:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\): \\(?2: ([^)]+)\\)?\\(?3:[0-9a-z]+\\)"
                                   ((1 'change-log-date)
                                    (2 'change-log-list nil lax)
                                    (3 'log-view-message)))))
    ;; checks
    (cond ((not url) ;; check that there is a link
           (user-error "No website for %s" (package-desc-name desc)))
          ;; check that link is to a recognized repo
          ((not (and url (alist-get url package-vc-heuristic-alist
                                    nil nil #'string-match-p)))
           (user-error "No repository available for %s" (package-desc-name desc)))
          ;; proceed to clone repo
          (t
           (shell-command (concat "rm -rf " tmpd))
           (shell-command (concat "cd " tmp " && git clone --filter=blob:none --no-checkout " url " tmpdir && cd tmpdir"))
           (when-let ((default-directory tmpd))
             (vc-print-log nil 15))
           ;; move buffer window to popper (optional)
           (popper-toggle-type "*vc-change-log*")))))
(bind-key "l" #'package-browse-vc-log 'package-menu-mode-map)

;;; Provide

(provide 'config)
;;; config.el ends here
