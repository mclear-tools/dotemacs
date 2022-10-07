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

;;;;; Private File
;; where to store private or "secret" info
(let ((private (expand-file-name "private.el" lem-user-dir)))
  (if (file-exists-p private)
	  (load-file private)))

;;;; User Vars

;;;;; Fullscreen Frame
;; Need to do this after startup to avoid flashing the screen for some reason
(push '(fullscreen . maximized) initial-frame-alist)

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
  (setq citar-library-paths '("/Users/roambot/Library/Mobile Documents/iCloud~com~sonnysoftware~bot/Documents/be-library")))

;;;;; Notes
;; I use hugo so define a setup file variable
(defvar hugo-notebook-setup-file "~/Dropbox/Work/projects/notebook/content-org/hugo-notebook-setup.org"
  "Variable for notebook setup using hugo.")

;; Denote settings
(customize-set-variable 'lem-notes-dir (concat (getenv "HOME") "/Documents/notes/"))
(customize-set-variable 'denote-directory lem-notes-dir)
(customize-set-variable 'denote-known-keywords '("emacs" "teaching" "unl" "workbook"))
(customize-set-variable 'denote-prompts '(title keywords subdirectory))


;; Provide nicer spacing for note front matter
(setq denote-org-front-matter
      "#+title:     %s
#+date:    %s
#+filetags:    %s
#+identifier: %s
\n")

(setq consult-notes-sources
      `(("Zettel"          ?z ,(concat lem-notes-dir "zettel/"))
        ("Lecture Notes"   ?l ,(concat lem-notes-dir "lecture-notes/"))
        ("Reference Notes" ?r ,(concat lem-notes-dir "ref-notes/"))
        ("Org"             ?o "~/Dropbox/org-files/")
        ("Workbook"        ?w ,(concat lem-notes-dir "workbook/"))
        ("Refile"          ?R ,(concat lem-notes-dir "refile-notes/"))))

;; Org-Roam Notes
;; (require 'lem-setup-org-roam)
;; (setq org-roam-directory lem-notes-dir)


;; Old sources
;; ("Zettel"          ?z "~/Dropbox/Work/projects/notebook/content-org/")
;; ("Lecture Notes"   ?l "~/Dropbox/Work/projects/notebook/content-org/lectures/")
;; ("Reference Notes" ?r "~/Dropbox/Work/projects/notebook/content-org/ref-notes/")

;;;;; Org Directories
(setq org-directory "~/Dropbox/org-files/"
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
       " --css=/Users/roambot/.pandoc/pandoc.css"
       " --quiet"
       " --number-sections"
       " --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua"
       " --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua"
       " --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua"
       ;; " --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml"
       " --metadata=reference-section-title:References"
       " --citeproc"
       " --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib"))

;;;; Load Modules
;; Load modules in stages for a shorter init time. We load core modules first,
;; then more expensive modules after init, with the rest loaded after startup
;; has completed.

;;;;; Load base modules
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
                  'lem-setup-theme
                  ))
   (require mod)))

;;;;; Load After-Init Modules
(defun lem-user-config-after-init ()
  "Modules loaded after init."
  (message "
  ;; ======================================================
  ;; *Loading 𝛌-Emacs after-init Modules*
  ;; ======================================================")
  (measure-time (cl-dolist (mod (list
                                 ;; Splash/Dashboard
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
                                 ;; 'lem-setup-org-base
                                 ;; 'lem-setup-org-settings
                                 ;; 'lem-setup-org-extensions

                                 ;; Productivity
                                 'lem-setup-pdf
                                 'lem-setup-elfeed

                                 ;; OS settings
                                 ;; load only if on macos
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
(customize-set-variable 'lem-scratch-default-dir lem-scratch-save-dir)

;;;; User Keybindings
(customize-set-variable 'lem-prefix "C-c C-SPC")

;; Make sure to load these after general keybindings
(with-eval-after-load 'lem-setup-keybindings
  (bind-key (concat lem-prefix " \\")  #'lem-call-eshell)
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

             ;; Pandoc
             ("p" .  run-pandoc                            )
             ("P" .  lem-pandoc-pdf-open                   )

             ;; Citation
             ("x" .  citar-insert-citation                 ))

  (bind-keys :prefix-map lem+jump-to
             :prefix (concat lem-prefix " u" " j"          )
             ;; Jump to
             ("a" .  lem-jump-to-org-dashboard             )
             ("W" .  lem-jump-to-week-agenda               )
             ("t" .  lem-jump-to-org-agenda-all-todos      )
             ("j" .  lem-goto-journal                      )
             ("i" .  lem-org-goto-inbox                    )
             ("O" .  lem-goto-org-files                    ))

  (bind-keys :prefix-map lem+text
             :prefix (concat lem-prefix " u" " T"          )
             ;; Text manipulation
             ("S" .  just-one-space                        )
             ("h" .  lem-org-export-to-buffer-html-as-body )
             ("d" .  osx-dictionary-search-input           )
             ("w" .  count-words                           )
             ("o" .  lem-markdown-to-org                   )
             ("m" .  lem-org-to-markdown                   )))

;;;; User Packages

(with-eval-after-load 'popper
  ;; Match eshell, shell, term and/or vterm buffers
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '(;; make all shell/terminals popups
                  "^\\*eshell.*\\*$" eshell-mode
                  "^\\*shell.*\\*$"  shell-mode
                  "^\\*term.*\\*$"   term-mode
                  "^\\*vterm.*\\*$"  vterm-mode))))

;;;;; Homebrew
(use-package homebrew
  :disabled
  :when sys-mac
  ;; :straight (homebrew :host github :repo "jdormit/homebrew.el")
  :commands
  (homebrew-install homebrew-upgrade homebrew-update homebrew-edit homebrew-info homebrew-package-info))


;;;;; Dictionary
(use-package sdcv-mode
  :disabled
  ;; :straight (:type git :host github :repo "gucong/emacs-sdcv")
  :bind (:map lem+search-keys
         ("w" . sdcv-search)))

;;;;; Capf-bibtex
(use-package capf-bibtex
  ;; :straight (:type git :host github :repo "mclear-tools/capf-bibtex")
  :disabled
  :hook ((org-mode markdown-mode tex-mode latex-mode reftex-mode) . capf-bibtex-mode)
  :custom
  (capf-bibtex-bibliography
   '("/Users/roambot/Dropbox/Work/bibfile.bib")))

;;;;; Word Repetition Finder
;; Via https://irreal.org/blog/?p=10235
(use-package repetition_error_finder
  ;; :straight (:type git :host github :repo "ioah86/repetition_error_finder")
  :disabled
  :commands (find-reperr-whole-buffer find-reperr-from-point))

;;;;; Bookmark+
(use-package bookmark+
  :disabled
  :commands (bmkp-switch-bookmark-file-create bmkp-set-desktop-bookmark)
  :config
  (setq bmkp-last-as-first-bookmark-file (concat lem-cache-dir "bookmarks")))
;;;;; Highlight Lines
;; Highlight lines. You can toggle this off
(use-package hl-line+
  :disabled
  :hook
  ;; https://tech.toryanderson.com/2021/09/24/replacing-beacon.el-with-hl-line-flash/
  (window-scroll-functions . hl-line-flash)
  (focus-in . hl-line-flash)
  (post-command . hl-line-flash)
  :custom-face
  ;; subtle highlighting
  (hl-line ((t (:inherit highlight))))
  :custom
  (global-hl-line-mode nil)
  (hl-line-flash-show-period 0.5)
  ;; (hl-line-inhibit-highlighting-for-modes '(dired-mode))
  ;; (hl-line-overlay-priority -100) ;; sadly, seems not observed by diredfl
  (hl-line-when-idle-interval 5)
  (hl-line-inhibit-highlighting-for-modes '(eshell-mode lem-splash-mode))
  :config
  (toggle-hl-line-when-idle 1 t))

;;;;; Crosshair Highlighting
;; Highlight cursor vertically and horizontally
(use-package crosshairs
  :disabled
  :commands (crosshairs-highlight
             crosshairs-mode
             flash-crosshairs)
  :bind (:map lem+toggle-keys
         ("c" . crosshairs-mode))
  :custom-face
  (col-highlight ((t (:inherit hl-line))))
  :config
  ;; same colors for both hlines
  (setq col-highlight-vline-face-flag t))


;;;;; Pulsing Cursor
(use-package pulsing-cursor
  :load-path (lambda () (concat lem-user-elisp-dir "pulsing-cursor/"))
  ;; :straight (:type git :host github :repo "jasonjckn/pulsing-cursor")
  :defer 1
  :custom-face
  (pulsing-cursor-overlay-face1 ((t (:inherit match))))
  :custom
  (pulsing-cursor-delay 1.0)
  (pulsing-cursor-interval .5)
  (pulsing-cursor-blinks 5)
  :config (pulsing-cursor-mode +1))


;;;;; Command log mode
(use-package command-log-mode
  ;; :straight (:type git :host github :repo "lewang/command-log-mode")
  :commands (command-log-mode))

;;;;; Elfeed

;; Set elfeed feeds
(with-eval-after-load 'elfeed
  (customize-set-variable 'elfeed-feeds '("http://nullprogram.com/feed/"
                                          ("https://planet.emacslife.com/atom.xml" emacs)
                                          ("https://tilde.town/~ramin_hal9001/atom.xml" emacs))))

;;;;; Eshell Aliases
(with-eval-after-load 'eshell
  (lem-set-eshell-alias
   "pg" "goto-projects"
   "pd" "cd ~/projects"))

;;;; User Functions

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
(add-hook 'emacs-startup-hook
          (lambda ()
            (custom-set-faces
             `(default ((t (:font "SF Mono 13"))))
             `(fixed-pitch ((t (:inherit (default)))))
             `(fixed-pitch-serif ((t (:inherit (default)))))
             `(variable-pitch ((t (:font "Avenir Next 14")))))))

;;;;; Davmail & Mu4e
(defun cpm-start-davmail ()
  "Start a headless Davmail process in verm."
  (interactive)
  (let ((tname (cdr (assoc 'name (tab-bar--current-tab)))))
    (cond ((not (get-buffer "*davmail*"))
           ;; need to use vterm otherwise output speed is too slow
           (lem-run-in-vterm "davmail")
           (switch-to-buffer "*davmail*")
           ;; don't use goto address mode in davmail buffer
           (goto-address-mode -1)
           ;; (ansi-term "davmail" "davmail")
           (if (string= tname "Home")
               (switch-to-buffer "*splash*")
             (lem-previous-user-buffer))
           (message "Davmail started."))
          (t
           (message "Davmail process is already running!")))))

(defun cpm-stop-davmail ()
  "Kill davmail headless server."
  (interactive)
  (cond ((get-buffer "*davmail*")
         (kill-buffer "*davmail*")
         (message "Davmail stopped."))
        (t
         (message "There is no Davmail process!"))))

;; ;; Startup & quit hooks
(with-eval-after-load 'mu4e
  (progn
    ;; initiate davmail process
    (cpm-start-davmail)
    ;; add to popper buffer
    (add-to-list 'popper-reference-buffers '("\\*davmail\\*" . hide))
    ))
;; Kill davmail on quit
(add-hook 'kill-emacs-hook #'cpm-stop-davmail)

;;; Provide
(provide 'config)
;;; config.el ends here
