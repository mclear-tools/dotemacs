;; Tab-bar -- use tab-bar for window & buffer management -*- lexical-binding: t; -*-

;;; Tab Bar
;; Use tab-bar for window grouping and configuration within a project (replaces eyebrowse)
(use-package tab-bar
  :straight (:type built-in)
  :after (project)
  :commands (tab-bar-new-tab
             tab-bar-switch-to-tab
             tab-bar-switch-to-next-tab
             tab-bar-switch-to-prev-tab)
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-close-tab-select 'recent)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
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

  ;; (setq tab-bar-tab-name-function #'cpm/name-tab-by-project-or-default)
  (setq tab-bar-mode t))

;; ;; See https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
;; ;; I've modified it to use project instead of projectile
;; (defun cpm/name-tab-by-project-or-default ()
;;   "Return project name if in a project, or default tab-bar name if not.
;; The default tab-bar name uses the buffer name."
;;   (let ((project-name (cpm--project-name)))
;;     (if (string= "-" project-name)
;;         (tab-bar-tab-name-current)
;;       (cpm--project-name))))

;; ;; Get the current tab name for use in some other display
;; (defun cpm--current-tab-name ()
;;   (alist-get 'name (tab-bar--current-tab)))

;;;; Tab Bar Echo
;; Use echo area for tab name display
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
(run-with-idle-timer 5 t (lambda () (message nil) (tab-bar-echo-area-display-tab-names)))

;;; Provide Tabs
(provide 'setup-tabs)