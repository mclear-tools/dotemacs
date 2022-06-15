;;; lem-setup-citation.el --- summary -*- lexical-binding: t -*-

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

;; Packages for academic citation

;;; Code:

;;;; Citation Variables
(defcustom lem-bibliography nil "User bibliography for citation."
  :group 'lambda-emacs
  :tag "Lambda-Emacs User Bibliography")
(defcustom lem-bib-notes nil "User citation notes directory."
  :group 'lambda-emacs
  :tag "Lambda-Emacs Citation Notes Directory")
(defcustom lem-citar-note nil "Template for use with citar notes."
  :group 'lambda-emacs
  :tag "Lambda-Emacs Citar Notes Template")

;;;; Org-Cite
;; Eventually this should be a full replacement for org-ref
(use-package oc
  :straight nil
  :after org
  :config
  (setq org-cite-global-bibliography `(,lem-bibliography))
  (setq org-cite-export-processors
        '((beamer csl)
          (latex csl)
          (t csl))))

;; Currently only using csl
(use-package oc-csl
  :straight nil
  :after oc
  :init
  ;; make sure to download csl
  ;; https://citationstyles.org
  ;; https://github.com/citation-style-language
  ;; repos for styles & locales
  (setq org-cite-csl-styles-dir "~/.local/share/csl/styles")
  (setq org-cite-csl-locales-dir "~/.local/share/csl/locales"))

;;;; Citeproc
(use-package citeproc
  :straight (:host github :repo "andras-simonyi/citeproc-el")
  :after (oc oc-csl))

;;;; Citar
(use-package citar
  ;; :straight (:host github :repo "bdarcus/citar")
  :straight (:local-repo "/Users/roambot/bin/lisp-projects/citar" :files ("*.el"))
  :commands (citar-open-beref
             citar-open-notes
             citar-insert-citation)
  :bind (:map citar-map
         ("b" .  #'citar-open-beref))
  :custom
  (citar-bibliography `(,lem-bibliography))
  (org-cite-global-bibliography `(,lem-bibliography))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :config
  ;; use consult-completing-read for enhanced interface
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  ;; use embark with at-point
  (setq citar-at-point-function 'embark-act)
  (setq citar-default-action 'citar-open-beref)
  ;; add beref entry for bookends
  (setq citar-additional-fields '("doi" "url" "beref"))
  (setq citar-templates
        `((main . " ${=key= id:15} ${title:48}")
          (suffix . "${author editor:30}  ${=type=:12}  ${=beref=:12} ${tags keywords:*}")
          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
          (note . ,lem-citar-note)))
  (setq citar-symbols
        `((file ,(all-the-icons-octicon "file-pdf"      :face 'error) . " ")
          (note ,(all-the-icons-octicon "file-text"     :face 'warning) . " ")
          (link ,(all-the-icons-octicon "link-external" :face 'org-link) . " ")))
  ;; edit notes
  (setq citar-notes-paths `(,lem-bib-notes))

  ;; Citar & Bookends
  (defun citar-get-beref (entry)
    (let* ((field (citar-has-a-value '(beref) entry))
           (base-url (pcase field
                       ('beref "bookends://sonnysoftware.com/"))))
      (when field
        (concat base-url (citar-get-value field entry)))))

  (defun citar-open-beref (keys-entries)
    "Open bookends link associated with the KEYS-ENTRIES in bookends.

With prefix, rebuild the cache before offering candidates."
    (interactive (list (citar-select-refs
                        :rebuild-cache current-prefix-arg)))
    (dolist (key-entry keys-entries)
      (let ((link (citar-get-beref (cdr key-entry))))
        (if link
            (browse-url-default-browser link)
          (message "No ref found for %s" key-entry))))))

;;;; Citar-Capf
;; (use-package citar-capf
;;   :straight (:local-repo "/Users/roambot/bin/lisp-projects/citar-capf")
;;   :hook ((org-mode markdown-mode tex-mode latex-mode reftex-mode) . citar-capf-mode))

;; Add hooks
(defun lem--add-citation-hooks (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(defun lem--citar-capf-hooks ()
  (add-hook 'completion-at-point-functions #'citar-capf -90 t)
  (add-to-list 'completion-at-point-functions #'citar-capf))

(lem--add-citation-hooks
 'lem--citar-capf-hooks
 '(markdown-mode-hook
   org-mode-hook
   LaTeX-mode-hook
   latex-mode-hook
   tex-mode-hook))


(defun capf-citar-test ()
  (interactive)
  (progn
    (splash-screen-kill)
    ;; (require 'org)
    ;; (require 'citar-capf)
    (switch-to-buffer "*scratch*")
    (markdown-mode)
    (require 'citar)
    ;; (call-interactively #'citar-open)
    ;; (minibuffer-keyboard-quit)
    ;; (keyboard-quit)
    ))

(provide 'lem-setup-citation)
;;; lem-setup-citation.el ends here