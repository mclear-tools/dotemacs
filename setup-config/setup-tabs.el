;; Tab-bar -- use tab-bar for window & buffer management -*- lexical-binding: t; -*-

;;; Tab Bar
;; Use tab-bar for window grouping and configuration within a project (replaces eyebrowse)
(use-package tab-bar
  :straight (:type built-in)
  :commands (tab-bar-new-tab
             tab-bar-switch-to-tab
             tab-bar-switch-to-next-tab
             tab-bar-switch-to-prev-tab)
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-show nil)
  :config
  ;; https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/
  (defun cpm/tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t))))))

  (setq tab-bar-tab-name-function #'cpm/name-tab-by-project-or-default)
  (setq tab-bar-mode t))

;; See https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
;; I've modified it to use project instead of projectile
(defun cpm/name-tab-by-project-or-default ()
  "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name."
  (let ((project-name (cpm--project-name)))
    (if (string= "-" project-name)
        (tab-bar-tab-name-current)
      (cpm--project-name))))

;; Get the current tab name for use in some other display
(defun cpm-current-tab-name ()
  (alist-get 'name (tab-bar--current-tab)))


;;;; Group Buffers By Tab
;; tab-bar version of separate buffer list filter
;; https://github.com/wamei/elscreen-separate-buffer-list/issues/8
;; https://github.com/kaz-yos/emacs/blob/master/init.d/200_tab-related.el#L74-L87

(defun cpm--tab-bar-buffer-name-filter (buffer-names)
  "Filter BUFFER-NAMES by the current tab's buffer list
It should be used to filter a list of buffer names created by
other functions, such as `helm-buffer-list'."
  (let ((buffer-names-to-keep
         ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-List.html
         (append (mapcar #'buffer-name (alist-get 'wc-bl (tab-bar--tab)))
                 (mapcar #'buffer-name (alist-get 'wc-bbl (tab-bar--tab))))))
    (seq-filter (lambda (elt)
                  (member elt buffer-names-to-keep))
                buffer-names)))

;;;; Tab Bar Echo
(use-package tab-bar-echo-area
  :straight (:type git :host github :repo "fritzgrabo/tab-bar-echo-area")
  :bind (:map tab-prefix-map
         ("c" . tab-bar-echo-area-display-tab-name)
         ("a" . tab-bar-echo-area-display-tab-names))
  :custom-face
  (tab-bar-echo-area-tab ((t (:underline t :weight bold))))
  (tab-bar-echo-area-tab-group-current ((t (:foreground ,bespoke-faded))))
  :config
  (tab-bar-echo-area-mode 1))

;; display all tabs when idle
(run-with-idle-timer 15 t (lambda () (message nil) (tab-bar-echo-area-display-tab-names)))

;;; Provide Tabs
(provide 'setup-tabs)
