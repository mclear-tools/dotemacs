;;; Mu4e
(use-package mu4e
  :straight t
  :commands mu4e
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using mbsync every 5 minutes
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")
  (setq mu4e-completing-read-function 'completing-read)
  ;; Configure the function to use for sending mail
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-attachments-dir "~/Downloads")

  ;; Viewing
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M:%S"
        mu4e-headers-fields '((:date . 20)
			                  (:flags . 5)
			                  (:mailing-list . 10)
			                  (:from-or-to . 25)
			                  (:subject . nil)))
  (setq mu4e-speedbar-support t)
  (setq mu4e-use-fancy-chars t)
  ;; how to handle html-formatted emails
  ;; NOTE: superseded by xwidget support -- see mu4e-views below
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser) t)
  ;;; NOTE: need to add something about not loading remote images (mu4e-view-show-images ?)
  ;; Other options for rendering
  ;;; (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  ;;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

  ;; Composing Email
  (setq message-kill-buffer-on-exit t)
  ;;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)


  ;; NOTE: Only use this if you have set up a GPG key!
  ;; Automatically sign all outgoing mails
  ;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  ;; Move messages to trash
  ;; See https://rakhim.org/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/
  (fset 'cpm-email-move-to-trash "mTrash")
  (define-key mu4e-headers-mode-map (kbd "d") 'cpm-email-move-to-trash)
  (define-key mu4e-view-mode-map (kbd "d") 'cpm-email-move-to-trash)


  (setq mu4e-contexts
        (list
         ;; Work account
         (make-mu4e-context
          :name "UNL"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/UNL" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "mclear@unl.edu")
                  (user-full-name    . "Colin McLear")
                  ;; use Davmail for exchange handshake
                  (smtpmail-smtp-server  . "localhost")
                  ;; this address needs to be the original not the alias
                  (smtpmail-user-mail-address . "cmclear2@unl.edu")
                  (smtpmail-smtp-service . 1025)
                  (smtpmail-stream-type  . nil)
                  (mu4e-compose-signature . "Colin McLear")
                  (mu4e-drafts-folder  . "/UNL/Drafts")
                  (mu4e-sent-folder  . "/UNL/Sent")
                  (mu4e-refile-folder  . "/UNL/Archive")
                  (mu4e-trash-folder  . "/UNL/Trash")))

         ;; Personal account
         (make-mu4e-context
          :name "Fastmail"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Fastmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "mclear@fastmail.com")
                  (user-full-name    . "Colin McLear Fastmail")
                  (smtpmail-smtp-server  . "smtp.fastmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  ;; use keychain for credentials
                  (smtp-auth-credentials "security find-generic-password -s mbsync-fastmail-password -w")
                  (mu4e-compose-signature . "Colin McLear")
                  (mu4e-drafts-folder  . "/Fastmail/Drafts")
                  (mu4e-sent-folder  . "/Fastmail/Sent")
                  (mu4e-refile-folder  . "/Fastmail/Archive")
                  (mu4e-trash-folder  . "/Fastmail/Trash"))))))

;;; Better Viewing – Mu4e Views
;; This makes mu4e render html emails in emacs via xwidgets.
;; It basically reproduces a modern email client experience. Depends on compiling emacs with xwidgets
;; to check that exwidgets are installed
;; evaluate (xwidget-webkit-browse-url "https://www.gnu.org/")

(use-package mu4e-views
  :straight (mu4e-views :type git :host github :repo "lordpretzel/mu4e-views")
  :after mu4e
  :demand t
  :general
  (:states '(normal motion) :keymaps 'mu4e-headers-mode-map
   "v"  #'mu4e-views-mu4e-select-view-msg-method)
  :config
  (setq mu4e-views-completion-method 'default) ;; use default for completion
  (setq mu4e-views-default-view-method "html") ;; make xwidgets default
  (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window) ;; when pressing n and p stay in the current window
  (setq mu4e-views-auto-view-selected-message t)) ;; automatically open messages when moving in the headers view)

;;; Mu4e Thread Folding
(use-package mu4e-thread-folding
  :straight (mu4e-thread-folding :type git :host github :repo "rougier/mu4e-thread-folding")
  :after mu4e
  :config
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                           :shortname ""
                           :function (lambda (msg) "  "))))
  (setq mu4e-headers-fields '((:empty         .    2)
                              (:human-date    .   12)
                              (:flags         .    6)
                              (:mailing-list  .   10)
                              (:from          .   22)
                              (:subject       .   nil)))
  (define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-headers-toggle-at-point)
  (define-key mu4e-headers-mode-map (kbd "<left>")    'mu4e-headers-fold-at-point)
  (define-key mu4e-headers-mode-map (kbd "<S-left>")  'mu4e-headers-fold-all)
  (define-key mu4e-headers-mode-map (kbd "<right>")   'mu4e-headers-unfold-at-point)
  (define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-headers-unfold-all))

;;; HTML Email (Org-Mime)
(use-package org-mime
  :straight t
  :after mu4e
  :config
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil)))

;;; Mu4e Icons
(use-package mu4e-marker-icons
  :straight t
  :after mu4e
  :init (mu4e-marker-icons-mode 1))

;;; Mu4e Dashboard
(use-package mu4e-dashboard
  :straight (:host github :type git :repo "rougier/mu4e-dashboard")
  :after mu4e)

;;; End Setup Email

(provide 'setup-email)
