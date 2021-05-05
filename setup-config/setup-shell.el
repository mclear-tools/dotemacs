;; Shell
;;; Compilation Buffer
;;  Whenever I run ~compile~, the buffer stays even after a successful compilation.
;;  Let's make it close automatically if the compilation is successful.
(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time "0.4 sec" nil
                           (lambda ()
                             (select-window (get-buffer-window (get-buffer-create "*compilation*")))
                             (delete-window))))
          (message "No Compilation Errors!"))))

;;; Completion Buffer
;; Remove completion buffer when done
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (let ((buffer "*Completions*"))
                (and (get-buffer buffer)
                     (kill-buffer buffer)))))

;;; Terminal
;;;; Settings
;; Sane settings for ansi-term
;;  Other useful shell settings
(setq explicit-shell-file-name "/usr/local/bin/zsh")
;; don't add newline in long lines
(setq-default term-suppress-hard-newline t)
;; kill process buffers without query
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
;; kill ansi-buffer on exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
         ad-do-it
         (kill-buffer buffer))
        ad-do-it))
      (ad-activate 'term-sentinel)

;; clickable links & no highlight of line
(defun my-term-hook ()
  (goto-address-mode) (hl-line-mode 0) (setq comint-buffer-maximum-size most-positive-fixnum))
(add-hook 'term-mode-hook 'my-term-hook)
(add-hook 'vterm-mode-hook 'my-term-hook)

;; paste and navigation
(defun term-send-tab ()
  "Send tab in term mode."
  (interactive)
  (term-send-raw-string "\t"))

;; Emacs doesn’t handle less well, so use cat instead for the shell pager
(setenv "PAGER" "cat")

;; hack to fix pasting issue, the paste micro-state won't work in term
;; (general-define-key :states '(normal motion) :keymaps 'term-raw-map
;;   "p" 'term-paste
;;   "C-k" 'term-send-up
;;   "C-j" 'term-send-down)

;; (general-define-key :states '(insert) :keymaps 'term-raw-map
;;   "C-c C-d" 'term-send-eof
;;   "C-c C-z" 'term-stop-subjob
;;   "<tab>"   'term-send-tab
;;   "s-v"     'term-paste
;;   "C-k"     'term-send-up
;;   "C-j"     'term-send-down)



;;;; Vterm
;; Better terminal function---way faster than ansi-term
(use-package vterm
  :commands (vterm vterm-other-window)
  :general
  (:states '(normal motion insert)
   :keymaps 'vterm-mode-map
   ;; fix issue with fzf
   ;; "C-c" #'vterm-send-C-c
   "C-g" #'vterm--self-insert
   "C-j" #'vterm-send-down
   "C-k" #'vterm-send-up
   "s-v" #'vterm-yank
   "C-v" #'vterm-yank
   ;; "<C-escape>" #'evil-collection-vterm-toggle-send-escape)
   "<C-escape>" #'cpm/vterm-escape-toggle)
  (:states '(normal)
   :keymaps 'vterm-mode-map
   "p" #'vterm-yank
   "P" #'vterm-yank)
  :custom (vterm-install t)
  :config
  (eval-when-compile
    (setq vterm-always-compile-module t))
  (setq vterm-keymap-exceptions nil)
  ;; set colors -- this is best with dark solarized right now
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 100000)
  (setq ansi-color-names-vector
        ["#002833" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
  (setq vterm-term-environment-variable "xterm-256color")
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local evil-insert-state-cursor '("chartreuse3" box))
              (evil-insert-state))))

;; Escape to vim mode in shell
(defun cpm/vterm-escape-toggle ()
  (interactive)
  (evil-collection-vterm-toggle-send-escape)
  (vterm-send-key "<escape>"))

;; directory tracking
(defun vterm--rename-buffer-as-title (title)
  (let ((dir (string-trim-left (concat (nth 1 (split-string title ":")) "/"))))
    (cd-absolute dir)
    (rename-buffer (format "term %s" title) t)))
(add-hook 'vterm-set-title-functions 'vterm--rename-buffer-as-title)

;; vterm frame
(defun vterm-frame ()
  "Open a new terminal frame."
  (interactive)
  (let ((frame (selected-frame)))
    (with-selected-frame frame
      (progn
        (vterm)
        (set-frame-parameter frame 'name "terminal")))))

;; vterm toggle
(use-package vterm-toggle
  :disabled
  :commands (vterm-toggle-forward vterm-toggle-backward vterm-toggle-cd vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  ;; toggle window in bottom side
  (add-to-list 'display-buffer-alist
               '("^v?term.*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.5))))


;;;; Multi-Vterm
(use-package multi-vterm
  :commands (multi-vterm multi-vterm-projectile multi-vterm-dedicated-toggle))

;;; Virtualenvwrapper
(use-package virtualenvwrapper
  :after (:any eshell sane-term ansi-term)
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support
  (setq venv-project-home
        (expand-file-name (or (getenv "PROJECT_HOME") "~/Dropbox/Work/projects/")))
  (setq venv-location "~/bin/virtualenvs")
  (add-hook 'venv-postactivate-hook (lambda () (workon-venv)))
  (defun workon-venv ()
    "change directory to project in eshell"
    (eshell/cd (concat venv-project-home venv-current-name))))

;;; Tramp
;; An easy way to ssh
(use-package tramp
  :straight nil
  :defer 1)
(use-package tramp-term :commands tramp-term)

;;; Eshell
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell Eshell is
;; an elisp shell. It has its own configuration parameters, distinct from those of
;; shell or ansi-terminal.
;;;; Eshell Settings
(use-package eshell
  :commands eshell
  :init
  (setq eshell-directory-name (concat cpm-local-dir "eshell/")
        eshell-history-file-name (concat cpm-local-dir "eshell/history")
        eshell-aliases-file (concat cpm-local-dir "eshell/alias")
        eshell-last-dir-ring-file-name (concat cpm-local-dir "eshell/lastdir")
        eshell-highlight-prompt nil
        eshell-buffer-shorthand t
        eshell-cmpl-ignore-case t
        eshell-cmpl-cycle-completions t
        eshell-destroy-buffer-when-process-dies t
        eshell-history-size 10000
        ;; auto truncate after 20k lines
        eshell-buffer-maximum-lines 20000
        eshell-hist-ignoredups t
        eshell-error-if-no-glob t
        eshell-glob-case-insensitive t
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-list-files-after-cd t
        eshell-banner-message ""
        ;; eshell-banner-message (message "Emacs initialized in %.2fs \n\n" (float-time (time-subtract (current-time) my-start-time)))
        ;; eshell-banner-message "What would you like to do?\n\n"
      )
      ;; Visual commands
  (setq eshell-visual-commands '("ranger" "vi" "screen" "top" "less" "more" "lynx"
                                     "ncftp" "pine" "tin" "trn" "elm" "vim"
                                     "nmtui" "alsamixer" "htop" "el" "elinks"
                                     ))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show"))))

(defun cpm/setup-eshell ()
 (interactive)
  ;; turn off semantic-mode in eshell buffers
  (semantic-mode -1)
  ;; turn off hl-line-mode
  (hl-line-mode -1))

;;;; Eshell helm
;; helm support
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell-cmpl-initialize)
            (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
            (define-key eshell-mode-map (kbd "M-l") 'helm-eshell-history)
            (cpm/setup-eshell)))

    (when (not (functionp 'eshell/rgrep))
      (defun eshell/rgrep (&rest args)
        "Use Emacs grep facility instead of calling external grep."
        (eshell-grep "rgrep" args t)))

;;;; Eshell Evil Histgory Navigation
;; History browsing. Note keybindings need to be buffer local as per
;; https://github.com/noctuid/general.el/issues/80
(add-hook 'eshell-mode-hook
(lambda ()
  (general-define-key :states  '(normal insert emacs) :keymaps 'eshell-mode-map
    "<down>" 'eshell-next-input
    "<up>"   'eshell-previous-input
    "C-k"    'eshell-next-input
    "C-j"    'eshell-previous-input)))

;;;; Eshell Prompt
;; A nicer eshell prompt https://gist.github.com/ekaschalk/f0ac91c406ad99e53bb97752683811a5
;; with some useful discussion of how it was put together http://www.modernemacs.com/post/custom-eshell/
;; I've made just a few tiny modifications.

(with-eval-after-load 'eshell
(require 'dash)
(require 's)

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME
         (lambda () (when ,FORM
                 (-> ,ICON
                    (concat esh-section-delim ,FORM)
                    (with-face ,@PROPS))))))

(defun esh-acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (concat acc esh-sep it))
    acc))

(defun esh-prompt-func ()
  "Build `eshell-prompt-function'"
  (concat esh-header
          (-reduce-from 'esh-acc "" eshell-funcs)
          "\n"
          eshell-prompt-string))

(esh-section esh-dir
             "\xf07c"  ;  (faicon folder)
             (abbreviate-file-name (eshell/pwd))
             '(:foreground "#268bd2" :underline t))

(esh-section esh-git
             "\xe907"  ;  (git icon)
             (with-eval-after-load 'magit
             (magit-get-current-branch))
             '(:foreground "#b58900"))

(esh-section esh-python
             "\xe928"  ;  (python icon)
             (with-eval-after-load "virtualenvwrapper"
             venv-current-name))

(esh-section esh-clock
             "\xf017"  ;  (clock icon)
             (format-time-string "%H:%M" (current-time))
             '(:foreground "forest green"))

;; Below I implement a "prompt number" section
(setq esh-prompt-num 0)
(add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
(advice-add 'eshell-send-input :before
            (lambda (&rest args) (setq esh-prompt-num (cl-incf esh-prompt-num))))

(esh-section esh-num
             "\xf0c9"  ;  (list icon)
             (number-to-string esh-prompt-num)
             '(:foreground "brown"))

;; Separator between esh-sections
(setq esh-sep " | ")  ; or "  "

;; Separator between an esh-section icon and form
(setq esh-section-delim " ")

;; Eshell prompt header
(setq esh-header "\n┌─")  ; or "\n "

;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
;; your login, these can be the same.
(setq eshell-prompt-regexp "^└─>> ") ;; note the '^' to get regex working right
(setq eshell-prompt-string "└─>> ")

;; Choose which eshell-funcs to enable
(setq eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num))

;; Enable the new eshell prompt
(setq eshell-prompt-function 'esh-prompt-func))

;;;; Clear Eshell
;; Make eshell act like a standard unix terminal.
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          #'(lambda()
              (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;;;; Eshell Magit
(defun eshell/magit ()
  "Function to open magit-status for the current directory"
  (interactive)
  (require 'magit)
  (magit-status-setup-buffer default-directory)
  nil)

;;;; Eshell Fringe Status
;; Show last status in fringe
;; https://github.com/ryuslash/eshell-fringe-status
(use-package eshell-fringe-status
  :defer t
  :config
  (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode))

;;;; Eshell Autosuggest
;; Fish-like history autosuggestions in eshell
;; https://github.com/dieggsy/esh-autosuggest/
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;;;; End Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-shell)
