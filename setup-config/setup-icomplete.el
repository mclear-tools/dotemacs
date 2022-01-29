;;; Icomplete
;; Use this when running emacs for testing

;;; Icomplete
(use-package icomplete-vertical
  :straight (:type built-in)
  :hook (after-init . icomplete-vertical-mode)
  :bind
  (:map icomplete-minibuffer-map
   ("C-v"   . icomplete-vertical-toggle)
   ("RET"   . icomplete-force-complete-and-exit)
   ("TAB"   . icomplete-force-complete-and-exit)
   ("C-M-i" . minibuffer-complete)
   ("M-RET" . exit-minibuffer)
   ("<down>". icomplete-forward-completions)
   ("C-j"   . icomplete-forward-completions)
   ("<up>"  . icomplete-backward-completions)
   ("C-k"   . icomplete-backward-completions))
  :custom
  (icomplete-show-matches-on-no-input t)
  (icomplete-hide-common-prefix nil)
  (icomplete-compute-delay 0.0)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (icomplete-scroll t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode))


;;; Provide
(provide 'setup-icomplete)
