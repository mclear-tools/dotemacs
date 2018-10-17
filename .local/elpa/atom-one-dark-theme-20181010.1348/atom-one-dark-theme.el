;;; atom-one-dark-theme.el --- Atom One Dark color theme

;; Copyright 2015-2017 Jonathan Chu

;; Author: Jonathan Chu <me@jonathanchu.is>
;; URL: https://github.com/jonathanchu/atom-one-dark-theme
;; Package-Version: 20181010.1348
;; Version: 0.4.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An Emacs port of the Atom One Dark theme from Atom.io.

;;; Code:

(deftheme atom-one-dark
  "Atom One Dark - An Emacs port of the Atom One Dark theme from Atom.io.")

(defvar atom-one-dark-colors-alist
  (let* ((256color  (eq (display-color-cells (selected-frame)) 256))
         (colors `(("atom-one-dark-accent"   . "#528BFF")
                   ("atom-one-dark-fg"       . (if ,256color "color-248" "#ABB2BF"))
                   ("atom-one-dark-bg"       . (if ,256color "color-235" "#282C34"))
                   ("atom-one-dark-bg-1"     . (if ,256color "color-234" "#121417"))
                   ("atom-one-dark-bg-hl"    . (if ,256color "color-236" "#2C323C"))
                   ("atom-one-dark-gutter"   . (if ,256color "color-239" "#4B5363"))
                   ("atom-one-dark-mono-1"   . (if ,256color "color-248" "#ABB2BF"))
                   ("atom-one-dark-mono-2"   . (if ,256color "color-244" "#828997"))
                   ("atom-one-dark-mono-3"   . (if ,256color "color-240" "#5C6370"))
                   ("atom-one-dark-cyan"     . "#56B6C2")
                   ("atom-one-dark-blue"     . "#61AFEF")
                   ("atom-one-dark-purple"   . "#C678DD")
                   ("atom-one-dark-green"    . "#98C379")
                   ("atom-one-dark-red-1"    . "#E06C75")
                   ("atom-one-dark-red-2"    . "#BE5046")
                   ("atom-one-dark-orange-1" . "#D19A66")
                   ("atom-one-dark-orange-2" . "#E5C07B")
                   ("atom-one-dark-gray"     . (if ,256color "color-237" "#3E4451"))
                   ("atom-one-dark-silver"   . (if ,256color "color-247" "#9DA5B4"))
                   ("atom-one-dark-black"    . (if ,256color "color-233" "#21252B"))
                   ("atom-one-dark-border"   . (if ,256color "color-232" "#181A1F")))))
    colors)
  "List of Atom One Dark colors.")

(defmacro atom-one-dark-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    atom-one-dark-colors-alist))
     ,@body))

(atom-one-dark-with-color-variables
  (custom-theme-set-faces
   'atom-one-dark

   `(default ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg))))
   `(success ((t (:foreground ,atom-one-dark-green))))
   `(warning ((t (:foreground ,atom-one-dark-orange-2))))
   `(error ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
   `(link ((t (:foreground ,atom-one-dark-blue :underline t :weight bold))))
   `(link-visited ((t (:foreground ,atom-one-dark-blue :underline t :weight normal))))
   `(cursor ((t (:background ,atom-one-dark-accent))))
   `(fringe ((t (:background ,atom-one-dark-bg))))
   `(region ((t (:background ,atom-one-dark-gray))))
   `(highlight ((t (:background ,atom-one-dark-gray))))
   `(hl-line ((t (:background ,atom-one-dark-bg-hl))))
   `(vertical-border ((t (:background ,atom-one-dark-border :foreground ,atom-one-dark-border))))
   `(secondary-selection ((t (:background ,atom-one-dark-bg-1))))
   `(query-replace ((t (:inherit (isearch)))))
   `(minibuffer-prompt ((t (:foreground ,atom-one-dark-silver))))

   `(font-lock-builtin-face ((t (:foreground ,atom-one-dark-cyan))))
   `(font-lock-comment-face ((t (:foreground ,atom-one-dark-mono-3))))
   `(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
   `(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((t (:foreground ,atom-one-dark-blue))))
   `(font-lock-keyword-face ((t (:foreground ,atom-one-dark-purple :weight normal))))
   `(font-lock-preprocessor-face ((t (:foreground ,atom-one-dark-mono-2))))
   `(font-lock-string-face ((t (:foreground ,atom-one-dark-green))))
   `(font-lock-type-face ((t (:foreground ,atom-one-dark-orange-2))))
   `(font-lock-constant-face ((t (:foreground ,atom-one-dark-cyan))))
   `(font-lock-variable-name-face ((t (:foreground ,atom-one-dark-red-1))))
   `(font-lock-warning-face ((t (:foreground ,atom-one-dark-mono-3 :bold t))))

   ;; mode-line
   `(mode-line ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-silver :box (:color ,atom-one-dark-border :line-width 1)))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background ,atom-one-dark-border :foreground ,atom-one-dark-gray :box (:color ,atom-one-dark-border :line-width 1)))))

   ;; window-divider
   `(window-divider ((t (:foreground ,atom-one-dark-border))))
   `(window-divider-first-pixel ((t (:foreground ,atom-one-dark-border))))
   `(window-divider-last-pixel ((t (:foreground ,atom-one-dark-border))))

   ;; ido
   `(ido-first-match ((t (:foreground ,atom-one-dark-purple :weight bold))))
   `(ido-only-match ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
   `(ido-subdir ((t (:foreground ,atom-one-dark-blue))))
   `(ido-virtual ((t (:foreground ,atom-one-dark-mono-3))))

   ;; ace-jump
   `(ace-jump-face-background ((t (:foreground ,atom-one-dark-mono-3 :background ,atom-one-dark-bg-1 :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-bg-1 :inverse-video nil))))

   ;; company-mode
   `(company-tooltip ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg-1))))
   `(company-tooltip-annotation ((t (:foreground ,atom-one-dark-mono-2 :background ,atom-one-dark-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-gray))))
   `(company-tooltip-mouse ((t (:background ,atom-one-dark-gray))))
   `(company-tooltip-common ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg-1))))
   `(company-tooltip-common-selection ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-gray))))
   `(company-preview ((t (:background ,atom-one-dark-bg))))
   `(company-preview-common ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg))))
   `(company-scrollbar-fg ((t (:background ,atom-one-dark-mono-1))))
   `(company-scrollbar-bg ((t (:background ,atom-one-dark-bg-1))))

   ;; flymake
   `(flymake-error ((t (:underline (:color ,atom-one-dark-red-1 :style wave)))))
   `(flymake-note ((t (:underline (:color ,atom-one-dark-green :style wave)))))
   `(flymake-warning ((t (:underline (:color ,atom-one-dark-orange-1 :style wave)))))

   ;; flycheck
   `(flycheck-error ((t (:underline (:color ,atom-one-dark-red-1 :style wave)))))
   `(flycheck-info ((t (:underline (:color ,atom-one-dark-green :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,atom-one-dark-orange-1 :style wave)))))

   ;; compilation
   `(compilation-face ((t (:foreground ,atom-one-dark-fg))))
   `(compilation-line-number ((t (:foreground ,atom-one-dark-mono-2))))
   `(compilation-column-number ((t (:foreground ,atom-one-dark-mono-2))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info :weight bold))))
   `(compilation-mode-line-fail ((t (:inherit compilation-error :weight bold))))

   ;; isearch
   `(isearch ((t (:foreground ,atom-one-dark-bg :background ,atom-one-dark-purple))))
   `(isearch-fail ((t (:foreground ,atom-one-dark-red-2 :background nil))))
   `(lazy-highlight ((t (:foreground ,atom-one-dark-purple :background ,atom-one-dark-bg-1 :underline ,atom-one-dark-purple))))

   ;; diff-hl (https://github.com/dgutov/diff-hl)
   '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
   '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
   '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

   ;; dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink ((t (:foreground "#FD5FF1"))))

   ;; helm
   `(helm-header ((t (:foreground ,atom-one-dark-mono-2
                      :background ,atom-one-dark-bg
                      :underline nil
                      :box (:line-width 6 :color ,atom-one-dark-bg)))))
   `(helm-source-header ((t (:foreground ,atom-one-dark-orange-2
                             :background ,atom-one-dark-bg
                             :underline nil
                             :weight bold
                             :box (:line-width 6 :color ,atom-one-dark-bg)))))
   `(helm-selection ((t (:background ,atom-one-dark-gray))))
   `(helm-selection-line ((t (:background ,atom-one-dark-gray))))
   `(helm-visible-mark ((t (:foreground ,atom-one-dark-bg :foreground ,atom-one-dark-orange-2))))
   `(helm-candidate-number ((t (:foreground ,atom-one-dark-green :background ,atom-one-dark-bg-1))))
   `(helm-separator ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-red-1))))
   `(helm-M-x-key ((t (:foreground ,atom-one-dark-orange-1))))
   `(helm-bookmark-addressbook ((t (:foreground ,atom-one-dark-orange-1))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,atom-one-dark-purple))))
   `(helm-bookmark-info ((t (:foreground ,atom-one-dark-green))))
   `(helm-bookmark-man ((t (:foreground ,atom-one-dark-orange-2))))
   `(helm-bookmark-w3m ((t (:foreground ,atom-one-dark-purple))))
   `(helm-match ((t (:foreground ,atom-one-dark-orange-2))))
   `(helm-ff-directory ((t (:foreground ,atom-one-dark-cyan :background ,atom-one-dark-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,atom-one-dark-green :background ,atom-one-dark-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,atom-one-dark-bg :background ,atom-one-dark-orange-2 :weight normal))))
   `(helm-buffer-not-saved ((t (:foreground ,atom-one-dark-red-1))))
   `(helm-buffer-process ((t (:foreground ,atom-one-dark-mono-2))))
   `(helm-buffer-saved-out ((t (:foreground ,atom-one-dark-fg))))
   `(helm-buffer-size ((t (:foreground ,atom-one-dark-mono-2))))
   `(helm-buffer-directory ((t (:foreground ,atom-one-dark-purple))))
   `(helm-grep-cmd-line ((t (:foreground ,atom-one-dark-cyan))))
   `(helm-grep-file ((t (:foreground ,atom-one-dark-fg))))
   `(helm-grep-finish ((t (:foreground ,atom-one-dark-green))))
   `(helm-grep-lineno ((t (:foreground ,atom-one-dark-mono-2))))
   `(helm-grep-finish ((t (:foreground ,atom-one-dark-red-1))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-swoop-target-line-block-face ((t (:background ,atom-one-dark-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-line-face ((t (:background ,atom-one-dark-mono-3 :foreground "#222222"))))
   `(helm-swoop-target-word-face ((t (:background ,atom-one-dark-purple :foreground "#ffffff"))))
   `(helm-locate-finish ((t (:foreground ,atom-one-dark-green))))
   `(info-menu-star ((t (:foreground ,atom-one-dark-red-1))))

   ;; ivy
   `(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground ,atom-one-dark-green))))
   `(ivy-current-match ((t (:background ,atom-one-dark-gray :weight normal))))
   `(ivy-highlight-face ((t (:inherit font-lock-builtin-face))))
   `(ivy-match-required-face ((t (:inherit minibuffer-prompt :foreground ,atom-one-dark-red-1))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,atom-one-dark-bg-hl))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1 :background ,atom-one-dark-black :foreground ,atom-one-dark-purple :weight semi-bold))))
   `(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2 :background ,atom-one-dark-black :foreground ,atom-one-dark-green :weight semi-bold))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-2 :background ,atom-one-dark-black :foreground ,atom-one-dark-orange-2 :weight semi-bold))))
   `(ivy-minibuffer-match-highlight ((t (:foreground ,atom-one-dark-blue))))
   `(ivy-modified-buffer ((t (:inherit default :foreground ,atom-one-dark-orange-1))))
   `(ivy-virtual ((t (:inherit font-lock-builtin-face :slant italic))))

   ;; counsel
   `(counsel-key-binding ((t (:foreground ,atom-one-dark-orange-2 :weight bold))))

   ;; swiper
   `(swiper-match-face-1 ((t (:inherit ivy-minibuffer-match-face-1))))
   `(swiper-match-face-2 ((t (:inherit ivy-minibuffer-match-face-2))))
   `(swiper-match-face-3 ((t (:inherit ivy-minibuffer-match-face-3))))
   `(swiper-match-face-4 ((t (:inherit ivy-minibuffer-match-face-4))))

   ;; git-commit
   `(git-commit-comment-action  ((t (:foreground ,atom-one-dark-green :weight bold))))
   `(git-commit-comment-branch  ((t (:foreground ,atom-one-dark-blue :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,atom-one-dark-orange-2 :weight bold))))

   ;; git-gutter
   `(git-gutter:added ((t (:foreground ,atom-one-dark-green :weight bold))))
   `(git-gutter:deleted ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
   `(git-gutter:modified ((t (:foreground ,atom-one-dark-orange-1 :weight bold))))

   ;; man
   `(Man-overstrike ((t (:inherit font-lock-type-face :weight bold))))
   `(Man-underline ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; woman
   `(woman-bold ((t (:inherit font-lock-type-face :weight bold))))
   `(woman-italic ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; dictionary
   `(dictionary-button-face ((t (:inherit widget-button))))
   `(dictionary-reference-face ((t (:inherit font-lock-type-face :weight bold))))
   `(dictionary-word-entry-face ((t (:inherit font-lock-keyword-face :slant italic :weight bold))))

   ;; jabber
   `(jabber-roster-user-online ((t (:foreground ,atom-one-dark-green))))
   `(jabber-roster-user-away ((t (:foreground ,atom-one-dark-red-1))))
   `(jabber-roster-user-xa ((t (:foreground ,atom-one-dark-red-2))))
   `(jabber-roster-user-dnd ((t (:foreground ,atom-one-dark-purple))))
   `(jabber-roster-user-chatty ((t (:foreground ,atom-one-dark-orange-2))))
   `(jabber-roster-user-error ((t (:foreground ,atom-one-dark-red-1 :bold t))))
   `(jabber-roster-user-offline ((t (:foreground ,atom-one-dark-mono-3))))
   `(jabber-chat-prompt-local ((t (:foreground ,atom-one-dark-blue))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,atom-one-dark-orange-2))))
   `(jabber-chat-prompt-system ((t (:foreground ,atom-one-dark-mono-3))))
   `(jabber-chat-error ((t (:inherit jabber-roster-user-error))))
   `(jabber-rare-time-face ((t (:foreground ,atom-one-dark-cyan))))
   `(jabber-activity-face ((t (:inherit jabber-chat-prompt-foreign))))
   `(jabber-activity-personal-face ((t (:inherit jabber-chat-prompt-local))))

   ;; eww
   `(eww-form-checkbox ((t (:inherit eww-form-submit))))
   `(eww-form-file ((t (:inherit eww-form-submit))))
   `(eww-form-select ((t (:inherit eww-form-submit))))
   `(eww-form-submit ((t (:background ,atom-one-dark-gray :foreground ,atom-one-dark-fg :box (:line-width 2 :color ,atom-one-dark-border :style released-button)))))
   `(eww-form-text ((t (:inherit widget-field :box (:line-width 1 :color ,atom-one-dark-border)))))
   `(eww-form-textarea ((t (:inherit eww-form-text))))
   `(eww-invalid-certificate ((t (:foreground ,atom-one-dark-red-1))))
   `(eww-valid-certificate ((t (:foreground ,atom-one-dark-green))))

   ;; js2-mode
   `(js2-error ((t (:underline (:color ,atom-one-dark-red-1 :style wave)))))
   `(js2-external-variable ((t (:foreground ,atom-one-dark-cyan))))
   `(js2-warning ((t (:underline (:color ,atom-one-dark-orange-1 :style wave)))))
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,atom-one-dark-mono-1))))
   `(js2-jsdoc-tag ((t (:foreground ,atom-one-dark-purple))))
   `(js2-jsdoc-type ((t (:foreground ,atom-one-dark-orange-2))))
   `(js2-jsdoc-value((t (:foreground ,atom-one-dark-red-1))))
   `(js2-object-property ((t (:foreground ,atom-one-dark-red-1))))

   ;; magit
   `(magit-section-highlight ((t (:background ,atom-one-dark-bg-hl))))
   `(magit-section-heading ((t (:foreground ,atom-one-dark-orange-2 :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,atom-one-dark-fg :weight bold))))
   `(magit-diff-file-heading ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,atom-one-dark-gray :weight bold))))
   `(magit-diff-file-heading-selection ((t (:foreground ,atom-one-dark-orange-2 :background ,atom-one-dark-bg-hl :weight bold))))
   `(magit-diff-hunk-heading ((t (:foreground ,atom-one-dark-mono-2 :background ,atom-one-dark-gray))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,atom-one-dark-mono-1 :background ,atom-one-dark-mono-3))))
   `(magit-diff-hunk-heading-selection ((t (:foreground ,atom-one-dark-purple :background ,atom-one-dark-mono-3))))
   `(magit-diff-context ((t (:foreground ,atom-one-dark-fg))))
   `(magit-diff-context-highlight ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-fg))))
   `(magit-diffstat-added ((t (:foreground ,atom-one-dark-green))))
   `(magit-diffstat-removed ((t (:foreground ,atom-one-dark-red-1))))
   `(magit-process-ok ((t (:foreground ,atom-one-dark-green))))
   `(magit-process-ng ((t (:foreground ,atom-one-dark-red-1))))
   `(magit-log-author ((t (:foreground ,atom-one-dark-orange-2))))
   `(magit-log-date ((t (:foreground ,atom-one-dark-mono-2))))
   `(magit-log-graph ((t (:foreground ,atom-one-dark-silver))))
   `(magit-sequence-pick ((t (:foreground ,atom-one-dark-orange-2))))
   `(magit-sequence-stop ((t (:foreground ,atom-one-dark-green))))
   `(magit-sequence-part ((t (:foreground ,atom-one-dark-orange-1))))
   `(magit-sequence-head ((t (:foreground ,atom-one-dark-blue))))
   `(magit-sequence-drop ((t (:foreground ,atom-one-dark-red-1))))
   `(magit-sequence-done ((t (:foreground ,atom-one-dark-mono-2))))
   `(magit-sequence-onto ((t (:foreground ,atom-one-dark-mono-2))))
   `(magit-bisect-good ((t (:foreground ,atom-one-dark-green))))
   `(magit-bisect-skip ((t (:foreground ,atom-one-dark-orange-1))))
   `(magit-bisect-bad ((t (:foreground ,atom-one-dark-red-1))))
   `(magit-blame-heading ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-mono-2))))
   `(magit-blame-hash ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-purple))))
   `(magit-blame-name ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-orange-2))))
   `(magit-blame-date ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-mono-3))))
   `(magit-blame-summary ((t (:background ,atom-one-dark-bg-1 :foreground ,atom-one-dark-mono-2))))
   `(magit-dimmed ((t (:foreground ,atom-one-dark-mono-2))))
   `(magit-hash ((t (:foreground ,atom-one-dark-purple))))
   `(magit-tag  ((t (:foreground ,atom-one-dark-orange-1 :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,atom-one-dark-green :weight bold))))
   `(magit-branch-local   ((t (:foreground ,atom-one-dark-blue :weight bold))))
   `(magit-branch-current ((t (:foreground ,atom-one-dark-blue :weight bold :box t))))
   `(magit-head           ((t (:foreground ,atom-one-dark-blue :weight bold))))
   `(magit-refname        ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,atom-one-dark-bg :foreground ,atom-one-dark-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,atom-one-dark-green))))
   `(magit-signature-bad       ((t (:foreground ,atom-one-dark-red-1))))
   `(magit-signature-untrusted ((t (:foreground ,atom-one-dark-orange-1))))
   `(magit-cherry-unmatched    ((t (:foreground ,atom-one-dark-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,atom-one-dark-purple))))
   `(magit-reflog-commit       ((t (:foreground ,atom-one-dark-green))))
   `(magit-reflog-amend        ((t (:foreground ,atom-one-dark-purple))))
   `(magit-reflog-merge        ((t (:foreground ,atom-one-dark-green))))
   `(magit-reflog-checkout     ((t (:foreground ,atom-one-dark-blue))))
   `(magit-reflog-reset        ((t (:foreground ,atom-one-dark-red-1))))
   `(magit-reflog-rebase       ((t (:foreground ,atom-one-dark-purple))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,atom-one-dark-green))))
   `(magit-reflog-remote       ((t (:foreground ,atom-one-dark-cyan))))
   `(magit-reflog-other        ((t (:foreground ,atom-one-dark-cyan))))

   ;; message
   `(message-cited-text ((t (:foreground ,atom-one-dark-green))))
   `(message-header-cc ((t (:foreground ,atom-one-dark-orange-1 :weight bold))))
   `(message-header-name ((t (:foreground ,atom-one-dark-purple))))
   `(message-header-newsgroups ((t (:foreground ,atom-one-dark-orange-2 :weight bold :slant italic))))
   `(message-header-other ((t (:foreground ,atom-one-dark-red-1))))
   `(message-header-subject ((t (:foreground ,atom-one-dark-blue))))
   `(message-header-to ((t (:foreground ,atom-one-dark-orange-2 :weight bold))))
   `(message-header-xheader ((t (:foreground ,atom-one-dark-silver))))
   `(message-mml ((t (:foreground ,atom-one-dark-purple))))
   `(message-separator ((t (:foreground ,atom-one-dark-mono-3 :slant italic))))

   ;; notmuch
   `(notmuch-crypto-decryption ((t (:foreground ,atom-one-dark-purple :background ,atom-one-dark-black))))
   `(notmuch-crypto-signature-bad ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-black))))
   `(notmuch-crypto-signature-good ((t (:foreground ,atom-one-dark-green :background ,atom-one-dark-black))))
   `(notmuch-crypto-signature-good-key ((t (:foreground ,atom-one-dark-green :background ,atom-one-dark-black))))
   `(notmuch-crypto-signature-unknown ((t (:foreground ,atom-one-dark-orange-1 :background ,atom-one-dark-black))))
   `(notmuch-hello-logo-background ((t (:inherit default))))
   `(notmuch-message-summary-face ((t (:background ,atom-one-dark-black))))
   `(notmuch-search-count ((t (:inherit default :foreground ,atom-one-dark-silver))))
   `(notmuch-search-date ((t (:inherit default :foreground ,atom-one-dark-purple))))
   `(notmuch-search-matching-authors ((t (:inherit default :foreground ,atom-one-dark-orange-2))))
   `(notmuch-search-non-matching-authors ((t (:inherit font-lock-comment-face :slant italic))))
   `(notmuch-tag-added ((t (:underline ,atom-one-dark-green))))
   `(notmuch-tag-deleted ((t (:strike-through ,atom-one-dark-red-2))))
   `(notmuch-tag-face ((t (:foreground ,atom-one-dark-green))))
   `(notmuch-tag-unread ((t (:foreground ,atom-one-dark-red-1))))
   `(notmuch-tree-match-author-face ((t (:inherit notmuch-search-matching-authors))))
   `(notmuch-tree-match-date-face ((t (:inherit notmuch-search-date))))
   `(notmuch-tree-match-face ((t (:weight semi-bold))))
   `(notmuch-tree-match-tag-face ((t (:inherit notmuch-tag-face))))
   `(notmuch-tree-no-match-face ((t (:slant italic :weight light :inherit font-lock-comment-face))))

   ;; elfeed
   `(elfeed-log-debug-level-face ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-green))))
   `(elfeed-log-error-level-face ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-red-1))))
   `(elfeed-log-info-level-face ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-blue))))
   `(elfeed-log-warn-level-face ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-orange-1))))
   `(elfeed-search-date-face ((t (:foreground ,atom-one-dark-purple))))
   `(elfeed-search-feed-face ((t (:foreground ,atom-one-dark-orange-2))))
   `(elfeed-search-tag-face ((t (:foreground ,atom-one-dark-green))))
   `(elfeed-search-title-face ((t (:foreground ,atom-one-dark-mono-1))))
   `(elfeed-search-unread-count-face ((t (:foreground ,atom-one-dark-silver))))

   ;; perspective
   `(persp-selected-face ((t (:foreground ,atom-one-dark-blue))))

   ;; powerline
   `(powerline-active1 ((,class (:background ,atom-one-dark-bg-hl :foreground ,atom-one-dark-purple))))
   `(powerline-active2 ((,class (:background ,atom-one-dark-bg-hl :foreground ,atom-one-dark-purple))))
   `(powerline-inactive1 ((,class (:background ,atom-one-dark-bg :foreground ,atom-one-dark-fg))))
   `(powerline-inactive2 ((,class (:background ,atom-one-dark-bg :foreground ,atom-one-dark-fg))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,atom-one-dark-blue))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,atom-one-dark-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,atom-one-dark-orange-1))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,atom-one-dark-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,atom-one-dark-purple))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,atom-one-dark-orange-2))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,atom-one-dark-blue))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,atom-one-dark-green))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,atom-one-dark-orange-1))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,atom-one-dark-cyan))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,atom-one-dark-purple))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,atom-one-dark-orange-2))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,atom-one-dark-red-1 :weight bold))))

   ;; rbenv
   `(rbenv-active-ruby-face ((t (:foreground ,atom-one-dark-green))))

   ;; elixir
   `(elixir-atom-face ((t (:foreground ,atom-one-dark-cyan))))
   `(elixir-attribute-face ((t (:foreground ,atom-one-dark-red-1))))

   ;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-gray :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,atom-one-dark-gray :weight bold))))

   ;; spaceline
   `(spaceline-flycheck-error  ((,class (:foreground ,atom-one-dark-red-1))))
   `(spaceline-flycheck-info   ((,class (:foreground ,atom-one-dark-green))))
   `(spaceline-flycheck-warning((,class (:foreground ,atom-one-dark-orange-1))))
   `(spaceline-python-venv ((,class (:foreground ,atom-one-dark-purple))))

   ;; web-mode
   `(web-mode-doctype-face ((t (:inherit font-lock-comment-face))))
   `(web-mode-error-face ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-red-1))))
   `(web-mode-html-attr-equal-face ((t (:inherit default))))
   `(web-mode-html-attr-name-face ((t (:foreground ,atom-one-dark-orange-1))))
   `(web-mode-html-tag-bracket-face ((t (:inherit default))))
   `(web-mode-html-tag-face ((t (:foreground ,atom-one-dark-red-1))))
   `(web-mode-symbol-face ((t (:foreground ,atom-one-dark-orange-1))))

   ;; flx-ido
   `(flx-highlight-face ((t (:inherit (link) :weight bold))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face ((t (:foreground ,atom-one-dark-blue))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground "#FFFFFF" :background ,atom-one-dark-red-2))))
   `(rpm-spec-macro-face ((t (:foreground ,atom-one-dark-orange-2))))
   `(rpm-spec-var-face ((t (:foreground ,atom-one-dark-red-1))))
   `(rpm-spec-doc-face ((t (:foreground ,atom-one-dark-purple))))
   `(rpm-spec-dir-face ((t (:foreground ,atom-one-dark-cyan))))
   `(rpm-spec-package-face ((t (:foreground ,atom-one-dark-red-2))))
   `(rpm-spec-ghost-face ((t (:foreground ,atom-one-dark-red-2))))
   `(rpm-spec-section-face ((t (:foreground ,atom-one-dark-orange-2))))

   ;; guix
   `(guix-true ((t (:foreground ,atom-one-dark-green :weight bold))))

   ;; gomoku
   `(gomoku-O ((t (:foreground ,atom-one-dark-red-1 :weight bold))))
   `(gomoku-X ((t (:foreground ,atom-one-dark-green :weight bold))))

   ;; term
   `(term-color-black ((t :foreground ,atom-one-dark-mono-1)))
   `(term-color-blue ((t (:foreground ,atom-one-dark-blue))))
   `(term-color-cyan ((t :foreground ,atom-one-dark-cyan)))
   `(term-color-green ((t (:foreground ,atom-one-dark-green))))
   `(term-color-magenta ((t :foreground ,atom-one-dark-purple)))
   `(term-color-red ((t :foreground ,atom-one-dark-red-1)))
   `(term-color-white ((t :foreground ,atom-one-dark-fg)))
   `(term-color-yellow ((t (:foreground ,atom-one-dark-orange-1))))

   ;; linum
   `(linum ((t (:foreground ,atom-one-dark-gutter :background ,atom-one-dark-bg))))
   ;; hlinum
   `(linum-highlight-face ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg))))
   ;; native line numbers (emacs version >=26)
   `(line-number ((t (:foreground ,atom-one-dark-gutter :background ,atom-one-dark-bg))))
   `(line-number-current-line ((t (:foreground ,atom-one-dark-fg :background ,atom-one-dark-bg))))

   ;; regexp-builder
   `(reb-match-0 ((t (:background ,atom-one-dark-gray))))
   `(reb-match-1 ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-purple :weight semi-bold))))
   `(reb-match-2 ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-green :weight semi-bold))))
   `(reb-match-3 ((t (:background ,atom-one-dark-black :foreground ,atom-one-dark-orange-2 :weight semi-bold))))

   ;; desktop-entry
   `(desktop-entry-deprecated-keyword-face ((t (:inherit font-lock-warning-face))))
   `(desktop-entry-group-header-face ((t (:inherit font-lock-type-face))))
   `(desktop-entry-locale-face ((t (:inherit font-lock-string-face))))
   `(desktop-entry-unknown-keyword-face ((t (:underline (:color ,atom-one-dark-red-1 :style wave) :inherit font-lock-keyword-face))))
   `(desktop-entry-value-face ((t (:inherit default))))

   ;; latex-mode
   `(font-latex-sectioning-0-face ((t (:foreground ,atom-one-dark-blue :height 1.0))))
   `(font-latex-sectioning-1-face ((t (:foreground ,atom-one-dark-blue :height 1.0))))
   `(font-latex-sectioning-2-face ((t (:foreground ,atom-one-dark-blue :height 1.0))))
   `(font-latex-sectioning-3-face ((t (:foreground ,atom-one-dark-blue :height 1.0))))
   `(font-latex-sectioning-4-face ((t (:foreground ,atom-one-dark-blue :height 1.0))))
   `(font-latex-sectioning-5-face ((t (:foreground ,atom-one-dark-blue :height 1.0))))
   `(font-latex-bold-face ((t (:foreground ,atom-one-dark-green :weight bold))))
   `(font-latex-italic-face ((t (:foreground ,atom-one-dark-green))))
   `(font-latex-warning-face ((t (:foreground ,atom-one-dark-red-1))))
   `(font-latex-doctex-preprocessor-face ((t (:foreground ,atom-one-dark-cyan))))

   ;; org-mode
   `(org-date ((t (:foreground ,atom-one-dark-cyan))))
   `(org-footnote ((t (:foreground ,atom-one-dark-cyan))))
   `(org-sexp-date ((t (:foreground ,atom-one-dark-cyan))))

   ;; calendar
   `(diary ((t (:inherit warning))))
   `(holiday ((t (:foreground ,atom-one-dark-green))))

   ;; gud
   `(breakpoint-disabled ((t (:foreground ,atom-one-dark-orange-1))))
   `(breakpoint-enabled ((t (:foreground ,atom-one-dark-red-1 :weight bold))))

   ;; realgud
   `(realgud-overlay-arrow1        ((t (:foreground ,atom-one-dark-green))))
   `(realgud-overlay-arrow3        ((t (:foreground ,atom-one-dark-orange-1))   `(realgud-overlay-arrow2        ((t (:foreground ,atom-one-dark-orange-2))))
))
   '(realgud-bp-enabled-face       ((t (:inherit (error)))))
   `(realgud-bp-disabled-face      ((t (:inherit (secondary-selection)))))
   `(realgud-bp-line-enabled-face  ((t (:box (:color ,atom-one-dark-red-1)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color ,atom-one-dark-gray)))))
   `(realgud-line-number           ((t (:foreground ,atom-one-dark-mono-2))))
   `(realgud-backtrace-number      ((t (:inherit (secondary-selection)))))

   ;; undo-tree
   `(undo-tree-visualizer-current-face ((t (:foreground ,atom-one-dark-red-1))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,atom-one-dark-orange-1))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,atom-one-dark-cyan))))
   ))

(atom-one-dark-with-color-variables
  (custom-theme-set-variables
   'atom-one-dark
   ;; fill-column-indicator
   `(fci-rule-color ,atom-one-dark-gray)

   ;; tetris
   ;; | Tetromino | Color                    |
   ;; | O         | `atom-one-dark-orange-2' |
   ;; | J         | `atom-one-dark-blue'     |
   ;; | L         | `atom-one-dark-orange-1' |
   ;; | Z         | `atom-one-dark-red-1'    |
   ;; | S         | `atom-one-dark-green'    |
   ;; | T         | `atom-one-dark-purple'   |
   ;; | I         | `atom-one-dark-cyan'     |
   '(tetris-x-colors
     [[229 192 123] [97 175 239] [209 154 102] [224 108 117] [152 195 121] [198 120 221] [86 182 194]])

   ;; ansi-color
   `(ansi-color-names-vector
     [,atom-one-dark-black ,atom-one-dark-red-1 ,atom-one-dark-green ,atom-one-dark-orange-2
      ,atom-one-dark-blue ,atom-one-dark-purple ,atom-one-dark-cyan ,atom-one-dark-fg])
   ))

(defvar atom-one-dark-theme-force-faces-for-mode t
  "If t, atom-one-dark-theme will use Face Remapping to alter the theme faces for
the current buffer based on its mode in an attempt to mimick the Atom One Dark
Theme from Atom.io as best as possible.
The reason this is required is because some modes (html-mode, jyaml-mode, ...)
do not provide the necessary faces to do theming without conflicting with other
modes.
Current modes, and their faces, impacted by this variable:
* js2-mode: font-lock-constant-face, font-lock-doc-face, font-lock-variable-name-face
")

;; Many modes in Emacs do not define their own faces and instead use standard Emacs faces when it comes to theming.
;; That being said, to have a real "Atom One Dark Theme" for Emacs, we need to work around this so that these themes look
;; as much like "Atom One Dark Theme" as possible.  This means using per-buffer faces via "Face Remapping":
;;
;;   http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html
;;
;; Of course, this might be confusing to some when in one mode they see keywords highlighted in one face and in another
;; mode they see a different face.  That being said, you can set the `atom-one-dark-theme-force-faces-for-mode` variable to
;; `nil` to disable this feature.
(defun atom-one-dark-theme-change-faces-for-mode ()
  (interactive)
  (and (eq atom-one-dark-theme-force-faces-for-mode t)
       (cond
        ((member major-mode '(js2-mode))
         ;; atom-one-dark-orange-1
         (face-remap-add-relative 'font-lock-constant-face :foreground "#D19A66")
         (face-remap-add-relative 'font-lock-doc-face '(:inherit (font-lock-comment-face)))
         ;; atom-one-dark-mono-1
         (face-remap-add-relative 'font-lock-variable-name-face :foreground "#ABB2BF"))
        )))

(add-hook 'after-change-major-mode-hook 'atom-one-dark-theme-change-faces-for-mode)

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'atom-one-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; atom-one-dark-theme.el ends here
