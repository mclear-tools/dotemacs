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
  (setq mu4e-headers-date-format "%d/%m/%Y")
  (setq mu4e-speedbar-support t)
  (setq mu4e-use-fancy-chars t)
  ;; how to handle html-formatted emails
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser) t)
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
                  (mu4e-trash-folder  . "/Fastmail/Trash")))))
  )

;;; HTML Email (Org-Mime)
(use-package org-mime
  :straight t
  :after mu4e
  :config
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil)))
;;; Mu4e Dashboard
(use-package mu4e-dashboard
  :straight (:host github :type git :repo "rougier/mu4e-dashboard")
  :after mu4e
  :config
  (mu4e-dashboard-mode))

;;; End Setup Email

(provide 'setup-email)
