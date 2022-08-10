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

;;;;; Shell
(setq-default shell-file-name "/opt/homebrew/bin/zsh")
(setq explicit-shell-file-name "/opt/homebrew/bin/zsh")

;;;;; Set Fonts
(custom-set-variables
 '(lem-ui-default-font
   '(:font "SF Mono" :height 130)))

(custom-set-variables
 '(lem-ui-variable-width-font
   '(:font "Avenir Next" :height 140)))

;;;;; Set User Elisp Dir
(setq lem-user-elisp-dir "~/bin/lisp-projects/")

;;;;; Compilation
;; Multi-compile settings

(customize-set-variable 'multi-compile-alist '((org-mode .
                                                         (("pandoc-docx & Open" . "pandoc -s -C -f org+smart --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx   --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua -o %file-sans.docx %file-name && open %file-sans.docx")
                                                          ("pandoc-pdf & Open" . "pandoc -s -C -f org+smart --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                          ("pandoc-beamer-compile-presentation" . "pandoc -C -i --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && pandoc --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name")
                                                          ("pandoc-beamer & Open" . "pandoc -C -i --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && open %file-sans.pdf")
                                                          ("pandoc-beamer-handout & Open" . "pandoc -C --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name && open %file-sans-handout.pdf")))

                                               ;; commands for pandoc
                                               (markdown-mode .
                                                              (("pandoc-normalize" . "pandoc -f markdown -t markdown -s --id-prefix=%file-sans: --atx-headers --columns=85 --wrap=auto --reference-location=block -o %file-name %file-name")
                                                               ("pandoc-sep-html & Open" . "pandoc -f markdown -t html4 -s --base-header-level=1 --number-sections --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/sep.html4 --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name && open %file-sans.html")
                                                               ("pandoc-pdf & Open" . "pandoc -s -C --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                               ("pandoc-beamer-compile-presentation" . "pandoc -C -i --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && pandoc --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name")
                                                               ("pandoc-beamer & Open" . "pandoc -C -i --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && open %file-sans.pdf")
                                                               ("pandoc-beamer-handout & Open" . "pandoc -C --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name && open %file-sans-handout.pdf")
                                                               ("pandoc-handout & Open" . "pandoc -s -C --pdf-engine=xelatex  --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --template=/Users/Roambot/.pandoc/pandoc-templates/tufte.tex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                               ("pandoc-docx & Open" . "pandoc -s -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.docx %file-name && open %file-sans.docx")
                                                               ("pandoc-html & Open" . "pandoc -C -f markdown -t html5 -s --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/default.html5 --css=/Users/roambot/.pandoc/pandoc.css --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name && open %file-sans.html")
                                                               ("pandoc-pdf" . "pandoc -s -C --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name")
                                                               ("pandoc-docx" . "pandoc -s -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.docx %file-name")
                                                               ("pandoc-html" . "pandoc -f markdown -t html5 -s -C --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/default.html5 --css=/Users/roambot/.pandoc/pandoc.css --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name")
                                                               ("pandoc-handout" . "pandoc -s --pdf-engine=xelatex -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --template=/Users/Roambot/.pandoc/pandoc-templates/tufte.tex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name")
                                                               ("test pdf" . "pandoc -s --pdf-engine=xelatex -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                               ("pandoc-letter-pdf & Open" . "pandoc -s --pdf-engine=xelatex -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --template=/Users/Roambot/dotfiles/pandoc/pandoc-templates/letter.tex -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                               ("pandoc-book-chapter-pdf & Open" . "pandoc -s -N --pdf-engine=xelatex --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --citeproc --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/promote-headers.lua --filter pandoc-latex-color --metadata=reference-section-title:'References' --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                               ((string/starts-with buffer-file-name "/Users/roambot/Dropbox/Work/projects/Book-Projects/rationality-book/") . (("compile rationality book" . "cd %make-dir && make -k && open %make-dirbuild/pdf/kant-rationality-book.pdf")))))))

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
        ("Refile"          ?R ,(concat lem-notes-dir "refile-notes/"))
        ))

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
;; Don't walk straight repos
(push "straight" vc-directory-exclusion-list)
;; Delete .DS_Store before prune
(advice-add 'straight-prune-build :before #'(lambda () (move-file-to-trash "/Users/roambot/.emacs.d/.local/straight/build/.DS_Store")))

;;;;; Set Splash Footer
(setq lem-splash-footer  "                              Aus so krummem Holze, als woraus der Mensch gemacht ist, kann nichts ganz Gerades gezimmert werden")

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
       " --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib"
       ))

;;;; User Keybindings
(customize-set-variable 'lem-prefix "C-c C-SPC")
(bind-keys :prefix-map lem+user-keys
           :prefix (concat lem-prefix " u")
           ("a" .  lem-jump-to-org-dashboard                    )
           ("c" .  lem-find-files-setup-config-directory        )
           ("C" .  lem-search-setup-config-files                )
           ("d" .  osx-dictionary-search-input                  )
           ("m" .  lem-org-to-markdown                          )
           ("g" .  org-mac-grab-link                            )
           ("h" .  lem-org-export-to-buffer-html-as-body        )
           ("i" .  lem-org-goto-inbox                           )
           ("k" .  kill-compilation                             )
           ("l" .  desktop-read                                 )
           ("o" .  lem-markdown-to-org                          )
           ("O" .  lem-goto-org-files                           )
           ("p" .  run-pandoc                                   )
           ("P" .  lem-pandoc-pdf-open                          )
           ("s" .  sb-expand-current-file                       )
           ("S" .  just-one-space                               )
           ("t" .  lem-jump-to-org-agenda-all-todos             )
           ("j" .  lem-goto-journal                             )
           ("u" .  lem-straight-update-packages-asynchronously  )
           ("w" .  count-words                                  )
           ("W" .  lem-jump-to-week-agenda                      )
           ("x" .  citar-insert-citation                        ))

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

;;;; Load Modules

;; Load modules
(message "*Loading 𝛌-Emacs User Modules*")
(measure-time
 (cl-dolist (mod (list
                  ;; Core modules
                  'lem-setup-libraries
                  'lem-setup-settings
                  'lem-setup-functions
                  'lem-setup-macros
                  'lem-setup-server
                  'lem-setup-scratch

                  ;; UI modules
                  'lem-setup-frames
                  'lem-setup-windows
                  'lem-setup-buffers
                  'lem-setup-fonts
                  'lem-setup-faces
                  'lem-setup-colors
                  'lem-setup-completion
                  'lem-setup-keybindings
                  'lem-setup-help
                  'lem-setup-modeline
                  'lem-setup-theme
                  'lem-setup-splash

                  ;; Navigation & Search modules
                  'lem-setup-navigation
                  'lem-setup-dired
                  'lem-setup-search

                  ;; Project & Tab/Workspace modules
                  'lem-setup-vc
                  'lem-setup-projects
                  'lem-setup-tabs

                  ;; Writing modules
                  'lem-setup-writing
                  'lem-setup-notes
                  'lem-setup-citation

                  ;; Programming modules
                  'lem-setup-programming
                  'lem-setup-debug
                  'lem-setup-shell
                  'lem-setup-skeleton

                  ;; Org modules
                  'lem-setup-org-base
                  'lem-setup-org-settings
                  'lem-setup-org-extensions

                  ;; Productivity
                  'lem-setup-pdf
                  'lem-setup-elfeed

                  ;; Personal modules
                  'cpm-setup-email
                  'cpm-setup-meow
                  'cpm-setup-org
                  'cpm-setup-workspaces
                  'cpm-setup-calendars
                  'cpm-setup-teaching))
   (require mod)))

;; MacOS settings - defer load until after init.
(when sys-mac
  (message "*Load MacOS settings...*")
  (measure-time
   (run-with-idle-timer 1 nil
                        (function require)
                        'lem-setup-macos nil t)))

;;;; User Packages

;;;; Elfeed

;; Set elfeed feeds
(customize-set-variable 'elfeed-feeds '("http://nullprogram.com/feed/"
                                        ("https://planet.emacslife.com/atom.xml" emacs)))

;;; Provide
(provide 'config)
;;; config.el ends here
