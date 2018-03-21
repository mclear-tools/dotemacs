;;; nubox-tty-theme.el --- Nubox color theme (tty version)

;; Author: Martijn Terpstra <bigmartijn@gmail.com>
;; Package-Version: 1.0.0
;; Keywords: faces

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;;; Code:

(deftheme nubox-tty "Nubox (tty version)")
(let ((black "#000000")
      (black2 "#888888")
      (red "#a80000")
      (red2 "#fc5454")
      (green "#00a800")
      (green2"#54fc54")
      (yellow "#a85400")
      (yellow2"#fcfc54")
      (blue "#0000a8")
      (blue2"#5454fc")
      (magenta "#a800a8")
      (magenta2"#fc54fc")
      (cyan "#00a8a8")
      (cyan2"#54fcfc")
      (white "#cccccc")
      (white2"#fcfcfc"))
  (custom-theme-set-faces
   'nubox-tty
   `(default ((t (:foreground ,white2 :background ,black))))
   `(cursor ((t (:background ,white2))))
   `(escape-glyph ((t (:foreground ,cyan2))))
   `(dired-async-failures ((t (:foreground ,red))))
   `(dired-async-message ((t (:weight bold :foreground ,white2))))
   `(dired-async-mode-message ((t (:weight bold :foreground ,white2))))
   `(dired-directory ((t (:weight bold :foreground ,blue2))))
   `(dired-flagged ((t (:weight bold :foreground ,red2))))
   `(dired-header ((t (:foreground ,green2))))
   `(dired-ignored ((t (:foreground ,white))))
   `(dired-mark ((t (:foreground ,magenta2))))
   `(dired-marked ((t (:weight bold :foreground ,yellow2))))
   `(dired-perm-write ((t (:foreground ,magenta))))
   `(dired-symlink ((t (:weight bold :foreground ,cyan2))))
   `(dired-warning ((t (:weight bold :foreground ,red2))))
   `(eshell-ls-archive ((t (:foreground ,white))))
   `(eshell-ls-backup ((t (:foreground ,white))))
   `(eshell-ls-clutter ((t (:foreground ,white))))
   `(eshell-ls-directory ((t (:weight bold :foreground ,blue2))))
   `(eshell-ls-executable ((t (:weight bold :foreground ,green2))))
   `(eshell-ls-missing ((t (:foreground ,red2))))
   `(eshell-ls-product ((t (:foreground ,white))))
   `(eshell-ls-readonly ((t (:foreground ,white2))))
   `(eshell-ls-special ((t (:foreground ,magenta2))))
   `(eshell-ls-symlink ((t (:weight bold :foreground ,cyan2))))
   `(eshell-ls-unreadable ((t (:foreground ,white2))))
   `(eshell-prompt ((t (:foreground ,green2))))
   `(fixed-pitch ((t nil)))
   `(flyspell-duplicate ((t (:underline ,yellow))))
   `(flyspell-incorrect ((t (:underline ,red))))
   `(ido-first-match ((t (:weight bold))))
   `(ido-incomplete-regexp ((t (:inherit font-lock-warning-face))))
   `(ido-only-match ((t (:foreground ,yellow2))))
   `(ido-subdir ((t (:foreground ,blue2))))
   `(minibuffer-prompt ((t (:weight bold :foreground ,green :background ,black))))
   `(highlight ((t (:foreground ,black :background ,black2 ))))
   `(region ((t (:foreground ,black :background ,blue))))
   `(shadow ((t (:foreground ,black2))))
   `(secondary-selection ((t (:foreground ,white2 :background ,blue))))
   `(trailing-whitespace ((((class color) (background light)) (:background ,red)) (((class color) (background dark)) (:background ,red)) (t (:inverse-video t))))
   `(font-lock-builtin-face ((t (:foreground ,cyan2))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-comment-face ((t (:foreground ,black2))))
   `(font-lock-constant-face ((t (:foreground ,magenta))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,yellow2))))
   `(font-lock-keyword-face ((t (:foreground ,red2))))
   `(font-lock-negation-char-face ((t nil)))
   `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   `(font-lock-string-face ((t (:foreground ,green2))))
   `(font-lock-type-face ((t (:foreground ,magenta2))))
   `(font-lock-variable-name-face ((t (:foreground ,blue2))))
   `(font-lock-warning-face ((t (:weight bold :foreground ,red2))))
   `(org-document-info  ((t (:foreground ,blue2))))
   `(org-document-title  ((t (:weight bold :foreground ,white2))))
   `(org-footnote  ((t (:underline t :foreground ,blue2))))
   `(org-table  ((t (:foreground ,blue2))))
   `(org-latex-and-related  ((t (:foreground ,white))))
   `(org-todo  ((t (:weight bold :foreground ,red2))))
   `(org-special-keyword  ((t (:weight bold :foreground ,black2))))
   `(org-done  ((t (:weight bold :foreground ,green2))))
   `(outline-1  ((t (:weight bold :foreground ,blue2))))
   `(outline-2  ((t (:weight bold :foreground ,cyan2))))
   `(outline-3  ((t (:weight bold :foreground ,green2))))
   `(outline-4  ((t (:weight bold :foreground ,yellow2))))
   `(outline-5  ((t (:weight bold :foreground ,red2))))
   `(outline-6  ((t (:weight bold :foreground ,magenta2))))
   `(outline-7  ((t (:weight bold :foreground ,black2))))
   `(outline-8  ((t (:weight bold :foreground ,white))))
   `(button ((t (:inherit (link)))))
   `(link ((t (:underline (:color foreground-color :style line) :foreground ,blue))))
   `(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground ,magenta)) (((class color) (background dark)) (:foreground ,magenta))))
   `(fringe ((t (:background ,black))))
   `(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground ,black2 :background ,white)) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground ,black :background ,white2)) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground ,white2 :background ,black))))
   `(mode-line ((t (:box nil :foreground ,black :background ,white))))
   `(mode-line-inactive ((t (:box nil :foreground ,black :background ,black2))))
   `(mu4e-header-key-face ((t (:weight bold :foreground ,green2))))
   `(mu4e-unread-face ((t (:weight bold :foreground ,blue2))))
   `(mu4e-highlight-face ((t (:inherit (font-lock-doc-face)))))
   `(isearch ((t (:foreground ,black :background ,green2))))
   `(isearch-lazy-highlight-face ((t (:foreground ,black :background ,blue2))))
   `(isearch-fail ((t (:foreground ,white2 :background ,red2))))
   `(highlight ((t (:foreground ,blue2))))
   `(match ((t (:foreground ,black :background ,blue))))
   `(next-error ((t (:inherit (region)))))
   `(query-replace ((t (:inherit (isearch)))))
   `(ac-completion-face ((t (:inherit (font-lock-comment-face)))))
   `(artbollocks-face ((t (:underline (:color ,magenta :style line)))))
   `(artbollocks-lexical-illusions-face ((t (:underline (:color ,magenta :style line)))))
   `(artbollocks-passive-voice-face ((t (:underline (:color ,green :style line)))))
   `(artbollocks-weasel-words-face ((t (:underline (:color ,red2 :style line)))))
   `(org-ref-cite-face ((t (:inherit (org-link)))))
   `(pdf-view-region ((t (:inherit (region)))))
   `(popup-face ((t (:background ,black2 :foreground ,white2))))
   `(popup-menu-mouse-face ((t (:background ,white :foreground ,black))))
   `(popup-menu-selection-face ((t (:background ,white2 :foreground ,black))))
   `(popup-scroll-bar-background-face ((t (:background ,black2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,white))))
   `(popup-tip-face ((t (:background ,black2 :foreground ,white))))
   `(bookmark-menu-bookmark ((t nil)))
   `(term-color-black ((t (:foreground ,black :background ,black))))
   `(term-color-blue ((t (:foreground ,blue2 :background ,blue))))
   `(term-color-green ((t (:foreground ,green2 :background ,green))))
   `(term-color-cyan ((t (:foreground ,cyan2 :background ,cyan))))
   `(term-color-red ((t (:foreground ,red2 :background ,red))))
   `(term-color-magenta ((t (:foreground ,magenta2 :background ,magenta))))
   `(term-color-white ((t (:foreground ,white2 :background ,white))))
   `(term-color-yellow ((t (:foreground ,yellow2 :background ,yellow))))
   `(vertical-border ((t (:foreground ,black2))))
   `(writegood-duplicates-face ((t (:underline (:color ,cyan :style line)))))
   `(writegood-passive-voice-face ((t (:underline (:color ,green :style line)))))
   `(writegood-weasels-face ((t (:underline (:color ,red :style line)))))
   `(show-paren-match ((t (:weight bold :foreground ,white :background ,black2))))
   `(show-paren-mismatch ((t (:weight bold :foreground ,black :background ,red))))))
(provide-theme 'nubox-tty)

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'nubox-tty-theme)

;;; nubox-tty-theme.el ends here
