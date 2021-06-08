;;; Testing


;;;; Pretty Ligatures
;; https://www.reddit.com/r/emacs/comments/icem4s/emacs_271_freezes_when_using_font_ligatures/g23795b?utm_source=share&utm_medium=web2x&context=3
;; (when (window-system)
;;   (set-frame-font cpm-font5))
;; (let ((alist '(
;;                (33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)") ;; Broken
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)") ;; Broken
;;                (48 . ".\\(?:x[a-zA-Z]\\)") ;; Broken?
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;                )
;;              ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))



;; (when cpm-ligatures
;;   (defun my-correct-symbol-bounds (pretty-alist)
;;     "Prepend a TAB character to each symbol in this alist,
;; this way compose-region called by prettify-symbols-mode
;; will use the correct width of the symbols
;; instead of the width measured by char-width."
;;     (mapcar (lambda (el)
;;               (setcdr el (string ?\t (cdr el)))
;;               el)
;;             pretty-alist))

;;   (defun my-ligature-list (ligatures codepoint-start)
;;     "Create an alist of strings to replace with
;; codepoints starting from codepoint-start."
;;     (require 'dash)
;;     (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
;;       (-zip-pair ligatures codepoints)))

;;                                         ; list can be found at https://github.com/i-tu/Hasklig/blob/master/GlyphOrderAndAliasDB#L1588
;;   (setq my-hasklig-ligatures
;;         (let* ((ligs '("&&" "***" "*>" "\\\\" "||" "|>" "::"
;;                        "==" "===" "==>" "=>" "=<<" "!!" ">>"
;;                        ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
;;                        "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
;;                        "<<" "<<<" "<+>" ".." "..." "++" "+++"
;;                        "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->")))
;;           (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

;; nice glyphs for haskell with hasklig
;; (defun my-set-hasklig-ligatures ()
;;   "Add hasklig ligatures for use with prettify-symbols-mode."
;;   (setq prettify-symbols-alist
;;         (append my-hasklig-ligatures prettify-symbols-alist))
;;   (prettify-symbols-mode))

;; (add-hook 'text-mode-hook 'my-set-hasklig-ligatures))

;;;; Bug Hunter
(use-package bug-hunter
  :defer 2)


;;;; Named Servers
;; from https://www.reddit.com/r/emacs/comments/9ih3fo/frameoriented_workflow/e6lkfpa?utm_source=share&utm_medium=web2x
;; set different server names for multiple processes
;; (setq server-name "org")
;; (setq server-name "dashboard")


;;;; Did You Mean
;; https://gitlab.com/kisaragi-hiu/didyoumean.el/
;; (eval-when-compile
;;   (quelpa '(didyoumean :fetcher gitlab :repo "kisaragi-hiu/didyoumean.el")))
(use-package didyoumean
  :defer 2)


;;;; Org export to doc

;; This setup is tested on Emacs 24.3 & Emacs 24.4 on Linux/OSX
;; org v7 bundled with Emacs 24.3
(setq org-export-odt-preferred-output-format "doc")
;; org v8 bundled with Emacs 24.4
(setq org-odt-preferred-output-format "doc")
;; BTW, you can assign "pdf" in above variables if you prefer PDF format

;; Only OSX need below setup
(defun my-setup-odt-org-convert-process ()
  (interactive)
  (let ((cmd "/Applications/LibreOffice.app/Contents/MacOS/soffice"))
    (when (and (eq system-type 'darwin) (file-exists-p cmd))
      ;; org v7
      (setq org-export-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i")))
      ;; org v8
      (setq org-odt-convert-processes '(("LibreOffice" "/Applications/LibreOffice.app/Contents/MacOS/soffice --headless --convert-to %f%x --outdir %d %i"))))
    ))
(my-setup-odt-org-convert-process)


;;;; Clone a Repo
;; http://xenodium.com/emacs-clone-git-repo-from-clipboard/
;; -*- lexical-binding: t -*-

(defun ar/git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finished."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir (expand-file-name "~/Downloads/"))
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

;;;; Symbol List
(use-package symbolist
  :straight (:host github :repo "lassik/emacs-symbolist")
  :commands (symbolist-prefix symbolist-regexp))

;;;; Org-Hugo Links
;; New link type for Org-Hugo internal links
(defun org-hugo-link-complete ()
  "Create link with Hugo ref shortcode"
  (concat "{{% ref " (file-relative-name (read-file-name "File: ")) " %}}"))

(defun org-hugo-follow (link)
  (find-file (expand-file-name link)))

(with-eval-after-load 'org
  (org-link-set-parameters "hugo"
                           :complete 'org-hugo-link-complete
                           :follow 'org-hugo-follow))

;;;; Mini-Pop-Up
(use-package mini-popup
  :disabled
  :straight (:host github :repo "minad/mini-popup")
  :config
  ;; Configure a height function (Example for Vertico)
  (defun mini-popup-height-resize ()
    (* (1+ (min vertico--total vertico-count)) (default-line-height)))
  (defun mini-popup-height-fixed ()
    (* (1+ (if vertico--input vertico-count 0)) (default-line-height)))
  (setq mini-popup--height-function #'mini-popup-height-fixed)

  ;; Ensure that the popup is updated after refresh (Consult-specific)
  (add-hook 'consult--completion-refresh-hook #'mini-popup--setup-hook 99)
  )
;;; End Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-testing)
