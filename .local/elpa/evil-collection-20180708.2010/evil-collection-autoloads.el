;;; evil-collection-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-collection" "evil-collection.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from evil-collection.el

(autoload 'evil-collection-translate-key "evil-collection" "\
Translate keys in the keymap(s) corresponding to STATES and KEYMAPS.
STATES should be the name of an evil state, a list of states, or nil. KEYMAPS
should be a symbol corresponding to the keymap to make the translations in or a
list of keymap symbols. Like `evil-define-key', when a keymap does not exist,
the keybindings will be deferred until the keymap is defined, so
`with-eval-after-load' is not neccessary. TRANSLATIONS corresponds to a list of
key replacement pairs. For example, specifying \"a\" \"b\" will bind \"a\" to
\"b\"'s definition in the keymap. Specifying nil as a replacement will unbind a
key. If DESTRUCTIVE is nil, a backup of the keymap will be stored on the initial
invocation, and future invocations will always look up keys in the backup
keymap. When no TRANSLATIONS are given, this function will only create the
backup keymap without making any translations. On the other hand, if DESTRUCTIVE
is non-nil, the keymap will be destructively altered without creating a backup.
For example, calling this function multiple times with \"a\" \"b\" \"b\" \"a\"
would continue to swap and unswap the definitions of these keys. This means that
when DESTRUCTIVE is non-nil, all related swaps/cycles should be done in the same
invocation.

\(fn STATES KEYMAPS &rest TRANSLATIONS &key DESTRUCTIVE &allow-other-keys)" nil nil)

(function-put 'evil-collection-translate-key 'lisp-indent-function 'defun)

(autoload 'evil-collection-swap-key "evil-collection" "\
Wrapper around `evil-collection-translate-key' for swapping keys.
STATES, KEYMAPS, and ARGS are passed to `evil-collection-translate-key'. ARGS
should consist of key swaps (e.g. \"a\" \"b\" is equivalent to \"a\" \"b\" \"b\"
\"a\" with `evil-collection-translate-key') and optionally keyword arguments for
`evil-collection-translate-key'.

\(fn STATES KEYMAPS &rest ARGS)" nil t)

(function-put 'evil-collection-swap-key 'lisp-indent-function 'defun)

(autoload 'evil-collection-init "evil-collection" "\
Register the Evil bindings for all modes in `evil-collection-mode-list'.

Alternatively, you may register select bindings manually, for
instance:

  (with-eval-after-load 'calendar
    (require 'evil-collection-calendar)
    (evil-collection-calendar-setup))

If MODES is specified (as either one mode or a list of modes), use those modes
instead of the modes in `evil-collection-mode-list'.

\(fn &optional MODES)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection" '("evil-collection-")))

;;;***

;;;### (autoloads nil "evil-collection-ace-jump-mode" "evil-collection-ace-jump-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ace-jump-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ace-jump-mode" '("evil-collection-ace-jump-mode-")))

;;;***

;;;### (autoloads nil "evil-collection-ag" "evil-collection-ag.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ag.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ag" '("evil-collection-ag-")))

;;;***

;;;### (autoloads nil "evil-collection-alchemist" "evil-collection-alchemist.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-alchemist.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-alchemist" '("evil-collection-alchemist-")))

;;;***

;;;### (autoloads nil "evil-collection-anaconda-mode" "evil-collection-anaconda-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-anaconda-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-anaconda-mode" '("evil-collection-anaconda-mode-")))

;;;***

;;;### (autoloads nil "evil-collection-arc-mode" "evil-collection-arc-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-arc-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-arc-mode" '("evil-collection-arc-mode-")))

;;;***

;;;### (autoloads nil "evil-collection-avy" "evil-collection-avy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-avy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-avy" '("evil-collection-avy-")))

;;;***

;;;### (autoloads nil "evil-collection-bookmark" "evil-collection-bookmark.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-bookmark.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-bookmark" '("evil-collection-bookmark-")))

;;;***

;;;### (autoloads nil "evil-collection-buff-menu" "evil-collection-buff-menu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-buff-menu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-buff-menu" '("evil-collection-buff-menu-")))

;;;***

;;;### (autoloads nil "evil-collection-calc" "evil-collection-calc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-calc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-calc" '("evil-collection-calc-")))

;;;***

;;;### (autoloads nil "evil-collection-calendar" "evil-collection-calendar.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-calendar.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-calendar" '("evil-collection-calendar-")))

;;;***

;;;### (autoloads nil "evil-collection-cider" "evil-collection-cider.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-cider.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-cider" '("evil-collection-cider-")))

;;;***

;;;### (autoloads nil "evil-collection-cmake-mode" "evil-collection-cmake-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-cmake-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-cmake-mode" '("evil-collection-cmake-mode-set")))

;;;***

;;;### (autoloads nil "evil-collection-comint" "evil-collection-comint.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-comint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-comint" '("evil-collection-comint-")))

;;;***

;;;### (autoloads nil "evil-collection-company" "evil-collection-company.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-company.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-company" '("evil-collection-company-")))

;;;***

;;;### (autoloads nil "evil-collection-compile" "evil-collection-compile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-compile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-compile" '("evil-collection-compile-")))

;;;***

;;;### (autoloads nil "evil-collection-cus-theme" "evil-collection-cus-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-cus-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-cus-theme" '("evil-collection-cus-theme-")))

;;;***

;;;### (autoloads nil "evil-collection-custom" "evil-collection-custom.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-custom.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-custom" '("evil-collection-custom-")))

;;;***

;;;### (autoloads nil "evil-collection-daemons" "evil-collection-daemons.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-daemons.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-daemons" '("evil-collection-daemons-")))

;;;***

;;;### (autoloads nil "evil-collection-debbugs" "evil-collection-debbugs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-debbugs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-debbugs" '("evil-collection-debbugs-")))

;;;***

;;;### (autoloads nil "evil-collection-debug" "evil-collection-debug.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-debug.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-debug" '("evil-collection-debug-")))

;;;***

;;;### (autoloads nil "evil-collection-diff-mode" "evil-collection-diff-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-diff-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-diff-mode" '("evil-collection-diff-")))

;;;***

;;;### (autoloads nil "evil-collection-dired" "evil-collection-dired.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-dired.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-dired" '("evil-collection-dired-")))

;;;***

;;;### (autoloads nil "evil-collection-doc-view" "evil-collection-doc-view.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-doc-view.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-doc-view" '("evil-collection-doc-view-")))

;;;***

;;;### (autoloads nil "evil-collection-edebug" "evil-collection-edebug.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-edebug.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-edebug" '("evil-collection-edebug-")))

;;;***

;;;### (autoloads nil "evil-collection-ediff" "evil-collection-ediff.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ediff.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ediff" '("evil-collection-ediff-")))

;;;***

;;;### (autoloads nil "evil-collection-eldoc" "evil-collection-eldoc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-eldoc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-eldoc" '("evil-collection-eldoc-setup")))

;;;***

;;;### (autoloads nil "evil-collection-elfeed" "evil-collection-elfeed.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-elfeed.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-elfeed" '("evil-collection-elfeed-")))

;;;***

;;;### (autoloads nil "evil-collection-elisp-mode" "evil-collection-elisp-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-elisp-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-elisp-mode" '("evil-collection-elisp-mode-")))

;;;***

;;;### (autoloads nil "evil-collection-elisp-refs" "evil-collection-elisp-refs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-elisp-refs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-elisp-refs" '("evil-collection-elisp-refs-")))

;;;***

;;;### (autoloads nil "evil-collection-emms" "evil-collection-emms.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-emms.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-emms" '("evil-collection-emms-")))

;;;***

;;;### (autoloads nil "evil-collection-epa" "evil-collection-epa.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-epa.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-epa" '("evil-collection-epa-")))

;;;***

;;;### (autoloads nil "evil-collection-ert" "evil-collection-ert.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ert.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ert" '("evil-collection-ert-")))

;;;***

;;;### (autoloads nil "evil-collection-eshell" "evil-collection-eshell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-eshell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-eshell" '("evil-collection-eshell-")))

;;;***

;;;### (autoloads nil "evil-collection-etags-select" "evil-collection-etags-select.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-etags-select.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-etags-select" '("evil-collection-etags-select-setup")))

;;;***

;;;### (autoloads nil "evil-collection-eval-sexp-fu" "evil-collection-eval-sexp-fu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-eval-sexp-fu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-eval-sexp-fu" '("evil-collection-eval-sexp-fu-")))

;;;***

;;;### (autoloads nil "evil-collection-eww" "evil-collection-eww.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-eww.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-eww" '("evil-collection-eww-")))

;;;***

;;;### (autoloads nil "evil-collection-flycheck" "evil-collection-flycheck.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-flycheck.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-flycheck" '("evil-collection-flycheck-")))

;;;***

;;;### (autoloads nil "evil-collection-free-keys" "evil-collection-free-keys.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-free-keys.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-free-keys" '("evil-collection-free-keys-")))

;;;***

;;;### (autoloads nil "evil-collection-geiser" "evil-collection-geiser.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-geiser.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-geiser" '("evil-collection-geiser-")))

;;;***

;;;### (autoloads nil "evil-collection-ggtags" "evil-collection-ggtags.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ggtags.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ggtags" '("evil-collection-ggtags-")))

;;;***

;;;### (autoloads nil "evil-collection-git-timemachine" "evil-collection-git-timemachine.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-git-timemachine.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-git-timemachine" '("evil-collection-git-timemachine-")))

;;;***

;;;### (autoloads nil "evil-collection-go-mode" "evil-collection-go-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-go-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-go-mode" '("evil-collection-go-mode-")))

;;;***

;;;### (autoloads nil "evil-collection-guix" "evil-collection-guix.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-guix.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-guix" '("evil-collection-guix-")))

;;;***

;;;### (autoloads nil "evil-collection-helm" "evil-collection-helm.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-helm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-helm" '("evil-collection-helm-")))

;;;***

;;;### (autoloads nil "evil-collection-help" "evil-collection-help.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-help.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-help" '("evil-collection-help-")))

;;;***

;;;### (autoloads nil "evil-collection-ibuffer" "evil-collection-ibuffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ibuffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ibuffer" '("evil-collection-ibuffer-")))

;;;***

;;;### (autoloads nil "evil-collection-image" "evil-collection-image.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-image.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-image" '("evil-collection-image-")))

;;;***

;;;### (autoloads nil "evil-collection-image+" "evil-collection-image+.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-image+.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-image+" '("evil-collection-image+-")))

;;;***

;;;### (autoloads nil "evil-collection-indium" "evil-collection-indium.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-indium.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-indium" '("evil-collection-indium-")))

;;;***

;;;### (autoloads nil "evil-collection-info" "evil-collection-info.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-info.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-info" '("evil-collection-info-")))

;;;***

;;;### (autoloads nil "evil-collection-ivy" "evil-collection-ivy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ivy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ivy" '("evil-collection-ivy-")))

;;;***

;;;### (autoloads nil "evil-collection-js2-mode" "evil-collection-js2-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-js2-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-js2-mode" '("evil-collection-js2-")))

;;;***

;;;### (autoloads nil "evil-collection-kotlin-mode" "evil-collection-kotlin-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-kotlin-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-kotlin-mode" '("evil-collection-kotlin-m")))

;;;***

;;;### (autoloads nil "evil-collection-log-view" "evil-collection-log-view.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-log-view.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-log-view" '("evil-collection-log-view-")))

;;;***

;;;### (autoloads nil "evil-collection-lsp-ui-imenu" "evil-collection-lsp-ui-imenu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-lsp-ui-imenu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-lsp-ui-imenu" '("evil-collection-lsp-ui-imenu-")))

;;;***

;;;### (autoloads nil "evil-collection-lua-mode" "evil-collection-lua-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-lua-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-lua-mode" '("evil-collection-lua-mode-")))

;;;***

;;;### (autoloads nil "evil-collection-macrostep" "evil-collection-macrostep.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-macrostep.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-macrostep" '("evil-collection-macrostep-")))

;;;***

;;;### (autoloads nil "evil-collection-magit" "evil-collection-magit.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-magit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-magit" '("evil-collection-magit-")))

;;;***

;;;### (autoloads nil "evil-collection-man" "evil-collection-man.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-man.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-man" '("evil-collection-man-")))

;;;***

;;;### (autoloads nil "evil-collection-minibuffer" "evil-collection-minibuffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-minibuffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-minibuffer" '("evil-collection-minibuffer-")))

;;;***

;;;### (autoloads nil "evil-collection-mu4e" "evil-collection-mu4e.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-mu4e.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-mu4e" '("evil-collection-mu4e-")))

;;;***

;;;### (autoloads nil "evil-collection-mu4e-conversation" "evil-collection-mu4e-conversation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-mu4e-conversation.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-mu4e-conversation" '("evil-collection-mu4e-conversation-")))

;;;***

;;;### (autoloads nil "evil-collection-neotree" "evil-collection-neotree.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-neotree.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-neotree" '("evil-collection-neotree-")))

;;;***

;;;### (autoloads nil "evil-collection-notmuch" "evil-collection-notmuch.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-notmuch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-notmuch" '("evil-collection-notmuch-")))

;;;***

;;;### (autoloads nil "evil-collection-nov" "evil-collection-nov.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-nov.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-nov" '("evil-collection-nov-")))

;;;***

;;;### (autoloads nil "evil-collection-occur" "evil-collection-occur.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-occur.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-occur" '("evil-collection-occur-")))

;;;***

;;;### (autoloads nil "evil-collection-outline" "evil-collection-outline.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-outline.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-outline" '("evil-collection-outline-")))

;;;***

;;;### (autoloads nil "evil-collection-p4" "evil-collection-p4.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-p4.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-p4" '("evil-collection-p4-")))

;;;***

;;;### (autoloads nil "evil-collection-package-menu" "evil-collection-package-menu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-package-menu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-package-menu" '("evil-collection-package-menu-")))

;;;***

;;;### (autoloads nil "evil-collection-paren" "evil-collection-paren.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-paren.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-paren" '("evil-collection-paren-s")))

;;;***

;;;### (autoloads nil "evil-collection-pass" "evil-collection-pass.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-pass.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-pass" '("evil-collection-pass-")))

;;;***

;;;### (autoloads nil "evil-collection-pdf" "evil-collection-pdf.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-pdf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-pdf" '("evil-collection-pdf-")))

;;;***

;;;### (autoloads nil "evil-collection-popup" "evil-collection-popup.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-popup.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-popup" '("evil-collection-popup-")))

;;;***

;;;### (autoloads nil "evil-collection-proced" "evil-collection-proced.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-proced.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-proced" '("evil-collection-proced-")))

;;;***

;;;### (autoloads nil "evil-collection-prodigy" "evil-collection-prodigy.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-prodigy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-prodigy" '("evil-collection-prodigy-")))

;;;***

;;;### (autoloads nil "evil-collection-profiler" "evil-collection-profiler.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-profiler.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-profiler" '("evil-collection-profiler-")))

;;;***

;;;### (autoloads nil "evil-collection-python" "evil-collection-python.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-python.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-python" '("evil-collection-python-")))

;;;***

;;;### (autoloads nil "evil-collection-quickrun" "evil-collection-quickrun.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-quickrun.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-quickrun" '("evil-collection-quickrun-")))

;;;***

;;;### (autoloads nil "evil-collection-racer" "evil-collection-racer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-racer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-racer" '("evil-collection-racer-")))

;;;***

;;;### (autoloads nil "evil-collection-realgud" "evil-collection-realgud.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-realgud.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-realgud" '("evil-collection-realgud-")))

;;;***

;;;### (autoloads nil "evil-collection-reftex" "evil-collection-reftex.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-reftex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-reftex" '("evil-collection-reftex-")))

;;;***

;;;### (autoloads nil "evil-collection-rjsx-mode" "evil-collection-rjsx-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-rjsx-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-rjsx-mode" '("evil-collection-rjsx-m")))

;;;***

;;;### (autoloads nil "evil-collection-robe" "evil-collection-robe.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-robe.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-robe" '("evil-collection-robe-")))

;;;***

;;;### (autoloads nil "evil-collection-rtags" "evil-collection-rtags.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-rtags.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-rtags" '("evil-collection-rtags-")))

;;;***

;;;### (autoloads nil "evil-collection-ruby-mode" "evil-collection-ruby-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ruby-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ruby-mode" '("evil-collection-ruby-mode-")))

;;;***

;;;### (autoloads nil "evil-collection-settings" "evil-collection-settings.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-settings.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-settings" '("evil-collection-settings-setup-debugger-keys")))

;;;***

;;;### (autoloads nil "evil-collection-simple" "evil-collection-simple.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-simple.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-simple" '("evil-collection-simple-")))

;;;***

;;;### (autoloads nil "evil-collection-slime" "evil-collection-slime.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-slime.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-slime" '("evil-collection-slime-")))

;;;***

;;;### (autoloads nil "evil-collection-term" "evil-collection-term.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-term.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-term" '("evil-collection-term-")))

;;;***

;;;### (autoloads nil "evil-collection-tide" "evil-collection-tide.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-tide.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-tide" '("evil-collection-tide-")))

;;;***

;;;### (autoloads nil "evil-collection-transmission" "evil-collection-transmission.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-transmission.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-transmission" '("evil-collection-transmission-")))

;;;***

;;;### (autoloads nil "evil-collection-typescript-mode" "evil-collection-typescript-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-typescript-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-typescript-mode" '("evil-collection-typescript-mode-set")))

;;;***

;;;### (autoloads nil "evil-collection-vc-annotate" "evil-collection-vc-annotate.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-vc-annotate.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-vc-annotate" '("evil-collection-vc-annotate-")))

;;;***

;;;### (autoloads nil "evil-collection-vdiff" "evil-collection-vdiff.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-vdiff.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-vdiff" '("evil-collection-vdiff-setup")))

;;;***

;;;### (autoloads nil "evil-collection-view" "evil-collection-view.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-view.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-view" '("evil-collection-view-")))

;;;***

;;;### (autoloads nil "evil-collection-vlf" "evil-collection-vlf.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-vlf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-vlf" '("evil-collection-vlf-")))

;;;***

;;;### (autoloads nil "evil-collection-wdired" "evil-collection-wdired.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-wdired.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-wdired" '("evil-collection-wdired-")))

;;;***

;;;### (autoloads nil "evil-collection-wgrep" "evil-collection-wgrep.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-wgrep.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-wgrep" '("evil-collection-wgrep-")))

;;;***

;;;### (autoloads nil "evil-collection-which-key" "evil-collection-which-key.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-which-key.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-which-key" '("evil-collection-which-key-")))

;;;***

;;;### (autoloads nil "evil-collection-woman" "evil-collection-woman.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-woman.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-woman" '("evil-collection-woman-")))

;;;***

;;;### (autoloads nil "evil-collection-xref" "evil-collection-xref.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-xref.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-xref" '("evil-collection-xref-")))

;;;***

;;;### (autoloads nil "evil-collection-ztree" "evil-collection-ztree.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-collection-ztree.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-collection-ztree" '("evil-collection-ztree-")))

;;;***

;;;### (autoloads nil nil ("evil-collection-integration.el" "evil-collection-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-collection-autoloads.el ends here
