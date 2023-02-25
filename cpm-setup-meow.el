;;; cpm-setup-meow.el --- summary -*- lexical-binding: t -*-

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

;; Setup and keybindings for modal interaction with Emacs. Currently I'm using Meow instead of evil/vim bindings.

;;; Code:

;;;; Meow Setup
(defun meow-setup ()
  "Setup meow bindings."
  (meow-motion-overwrite-define-key
   '("s-[" . lem-previous-user-buffer)
   '("s-]" . lem-next-user-buffer)
   '("s-{" . tab-bar-switch-to-prev-tab)
   '("s-}" . tab-bar-switch-to-next-tab)
   '("j" . meow-next)
   '("k" . meow-prev))

  ;; Set leader This isn't the sanctioned way to do this, but it seems to be the
  ;; only way to get `leader' to properly display keys from
  ;; `meow-leader-define-key' and my personal keymap in `lem+leader-map' I think
  ;; the preferred way is via (setq meow-keypad-leader-dispatch "...") but
  ;; that doesn't work as i want it to
  (add-to-list 'meow-keymap-alist (cons 'leader lem+leader-map))
  ;; Keypad prefixes hijack personal keybinds so disable them
  ;; See https://github.com/meow-edit/meow/issues/206
  (setq meow-keypad-meta-prefix nil
        meow-keypad-ctrl-meta-prefix nil
        meow-keypad-literal-prefix nil
        meow-keypad-start-keys nil)

  (meow-leader-define-key
   ;;  ;; here we create bindings for necessary, high frequency commands
   '("?" . consult-apropos)
   ;; high frequency keybindings
   '(")" . "C-)")
   '("}" . "C-}")
   '("." . "M-.")
   '("[" . lem-previous-user-buffer)
   '("]" . lem-next-user-buffer)
   '("{" . tab-bar-switch-to-prev-tab)
   '("}" . tab-bar-switch-to-next-tab)
   '("TAB" . lem-tab-bar-select-tab-dwim)
   '("SPC" . execute-extended-command)
   ;; high frequency commands
   '(";" . comment-line)
   '("/" . meow-keypad-describe-key)
   '("=" . hl-line-mode)
   '("a" . consult-org-agenda)
   '("b" . lem+buffer-keys)
   '("c" . lem+comment-wrap-keys)
   '("C" . lem+config-keys)
   '("d" . dired-jump)
   '("D" . dired-jump-other-window)
   '("e" . lem+eval-keys)
   '("E" . lem-call-emacs)
   '("f" . lem+file-keys)
   '("F" . lem+flymake-keys)
   '("i" . lem-find-lambda-file)
   '("I" . lem-search-lambda-files)
   '("j" . avy-goto-char-timer)
   '("J" . crux-top-join-line)
   '("k" . consult-yank-from-kill-ring)
   '("l" . vertico-repeat-last)
   '("L" . consult-locate)
   '("M" . lem+compile-keys)
   '("n" . lem+notes-keys)
   '("N" . consult-notes-search-all)
   `("p" . ,project-prefix-map)
   '("q" . lem+quit-keys)
   '("r" . consult-register)
   '("R" . consult-recent-file)
   '("s" . lem+search-keys)
   '("S" . lem+spelling-keys)
   '("t" . lem+toggle-keys)
   '("u" . lem+user-keys)
   '("v" . lem+vc-keys)
   '("V" . multi-vterm-dedicated-toggle)
   '("w" . lem+window-keys)
   '("W" . lem+workspace-keys)
   '("y" . yas-minor-mode-map)
   )


  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '(":" . meow-goto-line)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . beginning-of-buffer)
   '("G" . end-of-buffer)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . overwrite-mode)
   '("s" . meow-kill)
   '("S" . embrace-commander)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-swap-grab)
   '("y" . meow-clipboard-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("&" . meow-query-replace-regexp)
   '("%" . meow-query-replace)
   '("=" . meow-grab)
   '("s-[" . lem-previous-user-buffer)
   '("s-]" . lem-next-user-buffer)
   '("s-{" . tab-bar-switch-to-prev-tab)
   '("s-}" . tab-bar-switch-to-next-tab)
   '("<escape>" . meow-cancel-selection)))

;;;; Meow
;; Note load Meow before loading personal keybindings, otherwise some might get clobbered
(use-package meow
  :custom
  (meow-use-cursor-position-hack t)
  (meow-use-clipboard t)
  (meow-goto-line-function 'consult-goto-line)
  :config
  ;; set colors in theme
  (setq meow-use-dynamic-face-color nil)
  ;; Make sure delete char means delete char
  ;; see https://github.com/meow-edit/meow/issues/112
  (setq meow--kbd-delete-char "<deletechar>")
  (meow-thing-register 'angle '(regexp "<" ">") '(regexp "<" ">"))
  (add-to-list 'meow-char-thing-table '(?a . angle))
  (with-eval-after-load 'org
    ;; for use with meow movement
    (modify-syntax-entry ?@ "_" org-mode-syntax-table))
  ;; start vterm in insert
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  ;; start eshell in insert
  (add-to-list 'meow-mode-state-list '(eshell-mode . insert))
  ;; mu4e views in motion
  (add-to-list 'meow-mode-state-list '(mu4e-headers-mode . motion))
  (add-to-list 'meow-mode-state-list '(mu4e-view-mode . motion))
  ;; start helpful in normal
  ;; (add-to-list 'meow-mode-state-list '(helpful-mode . normal))
  (meow-global-mode 1)
  (meow-setup))

;; Cooperate with splash page
(defun meow-lem-splash ()
  ;; Set no cursor with meow
  (with-eval-after-load 'meow
    (meow-motion-mode 1)
    (setq-local meow-cursor-type-motion nil)
    (ignore-errors
      (revert-buffer))))
  (add-hook 'lem-splash-mode-hook #'meow-lem-splash)

(provide 'cpm-setup-meow)
;;; cpm-setup-meow.el ends here
