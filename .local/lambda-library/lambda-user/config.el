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

                  ;; Org modules
                  'lem-setup-org-base

                  ;; Productivity
                  'lem-setup-pdf

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

;;;; User Vars

;;;;; Set Fonts
(custom-set-variables
 '(lem-ui-default-font
   '(:font "SF Mono" :weight normal :height 130)))

(custom-set-variables
 '(lem-ui-variable-width-font
   '(:font "Avenir Next" :weight normal :height 140)))

;;;;; Set User Elisp Dir
(setq lem-user-elisp-dir "~/bin/lisp-projects/")

;;;;; Citations
(setq lem-bibliography (concat (getenv "HOME") "/Dropbox/Work/bibfile.bib"))

(setq lem-bib-notes (concat (getenv "HOME") "/Dropbox/Work/projects/notebook/content-org/ref-notes"))

(setq lem-citar-note  "${author-or-editor} (${year}): ${title}\n#+ROAM_KEY: cite:${=key=}\n#+SETUPFILE: ../hugo-notebook-setup.org\n#+HUGO_SECTION: reading-notes\n\n- Tags :: \n- Bookends link :: bookends://sonnysoftware.com/${beref}\n- PDF :: [[${file}][PDF Link]]\n\n\n#+BEGIN_SRC emacs-lisp :exports none\n(insert \"#+BEGIN_SRC bibtex\")\n(newline)\n(citar--insert-bibtex \"${=key=}\")\n(insert \"#+END_SRC\")\n#+END_SRC\n")

;; Set citar library path
(with-eval-after-load 'citar
  (setq citar-library-paths '("/Users/roambot/Library/Mobile Documents/iCloud~com~sonnysoftware~bot/Documents/be-library")))

;;;;; Notes
;; I use hugo so define a setup file variable
(defvar hugo-notebook-setup-file "~/Dropbox/Work/projects/notebook/content-org/hugo-notebook-setup.org"
  "Variable for notebook setup using hugo.")

(setq lem-notes-dir "~/Dropbox/Work/projects/notes-all/")

(setq org-roam-directory "~/Dropbox/Work/projects/notebook/content-org/")
(setq lem-zettelkasten  "~/Dropbox/Work/projects/notebook/content-org")
(setq consult-notes-data-dirs
      '(("Org"             ?o "~/Dropbox/org-files/")
        ("Org Refile"      ?R "~/Dropbox/Work/projects/notebook/org-refile/")))


(with-eval-after-load 'consult-notes
  (defun consult-notes-org-roam-cited (reference)
    "Return a list of notes that cite the REFERENCE."
    (interactive (list (citar-select-ref
                        :rebuild-cache current-prefix-arg
                        :filter (citar-has-note))))
    (let* ((ids
            (org-roam-db-query [:select * :from citations
                                :where (= cite-key $s1)]
                               (car reference)))
           (anodes
            (mapcar (lambda (id)
                      (org-roam-node-from-id (car id)))
                    ids))
           (template
            (org-roam-node--process-display-format org-roam-node-display-template))
           (bnodes
            (mapcar (lambda (node)
                      (org-roam-node-read--to-candidate node template)) anodes))
           (node (completing-read
                  "Node: "
                  (lambda (string pred action)
                    (if (eq action 'metadata)
                        `(metadata
                          ;; get title using annotation function
                          (annotation-function
                           . ,(lambda (title)
                                (funcall org-roam-node-annotation-function
                                         (get-text-property 0 'node title))))
                          (category . org-roam-node))
                      (complete-with-action action bnodes string pred)))))
           (fnode
            (cdr (assoc node bnodes))))
      (if ids
          ;; Open node in other window
          (org-roam-node-open fnode)
        (message "No notes cite this reference.")))))

;; Old sources
;; ("Zettel"          ?z "~/Dropbox/Work/projects/notebook/content-org/")
;; ("Lecture Notes"   ?l "~/Dropbox/Work/projects/notebook/content-org/lectures/")
;; ("Reference Notes" ?r "~/Dropbox/Work/projects/notebook/content-org/ref-notes/")

(setq consult-notes-all-notes "~/Dropbox/Work/projects/notes-all/")
;;;;; Org Directories
(with-eval-after-load 'org
  (setq org-directory "~/Dropbox/org-files/"
        org-default-notes-file (concat org-directory "inbox.org")
        org-agenda-files (list org-directory)))

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

;;;;; Quick Commits
;; Make a quick commit without opening magit. This is a version of a
;; workflow I used to use in Sublime Text. Perfect for short commit messages.
;; FIXME: is there a way to make this work without evil?
(defun quick-commit ()
  "Quickly commit the current file-visiting buffer from the mini-buffer."
  (interactive)
  (shell-command (concat "Git add " (buffer-file-name) " && Git commit -m '" (read-string "Enter commit message: ") "'")))

;;;;; Save Buffer & Exit Emacsclient
(defun lem-email-save-and-kill ()
  "Save buffer and exit emacsclient."
  (interactive)
  (save-buffer)
  (server-edit))

;;;; User Keybindings
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

;;;; User Packages
;;;;; Delve (Collections of Notes in Org-Roam)
(use-package delve
  :disabled
  :straight (:repo "publicimageltd/delve"
             :host github
             :type git)
  :after org-roam
  :bind
  ;; the main entry point, offering a list of all stored collections
  ;; and of all open Delve buffers:
  (("<f12>" . delve))
  :config
  ;; set meaningful tag names for the dashboard query
  (setq delve-dashboard-tags '("german-idealism" "kant" "hegel"))
  (setq delve-store-directory (concat lem-cache-dir "delve-store"))
  ;; turn on delve-minor-mode when org roam file is opened:
  (delve-global-minor-mode))

;;; Provide
(provide 'config)
;;; config.el ends here
