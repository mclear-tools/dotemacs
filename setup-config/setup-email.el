;;; Mu4e
(use-package mu4e
  ;; Tell straight to use homebrew mu4e
  :straight (:local-repo "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/" :type built-in)
  :commands mu4e
  :config
  ;; Finding the binary (installed w/homebrew)
  (setq mu4e-mu-binary (executable-find "mu"))

;;;; Syncing
  ;; Maildir
  (setq mu4e-maildir "~/.maildir")
  ;; Sync imap servers w/mbsync (via isync installed w/homebrew):
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  ;; Change filenames when moving
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)
  ;; Refresh mail using mbsync every 5 minutes
  (setq mu4e-update-interval (* 5 60))

;;;; Attachments
  (setq mu4e-attachments-dir "~/Downloads")

;;;; Viewing
  ;; Set headers
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                           :shortname ""
                           :function (lambda (msg) "  "))))
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M:%S"
        mu4e-headers-fields '((:empty          .   1)
                              (:flags          .   8)
                              (:human-date     .  22)
                              (:from-or-to     .  45)
                              (:subject        . 100)))
  (setq mu4e-speedbar-support t)
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-completing-read-function 'completing-read)

  ;; List of your email adresses:
  (setq mu4e-user-mail-address-list '("mclear@fastmail.com"
                                      "mclear@unl.edu"))

  ;; how to handle html-formatted emails
  ;; NOTE: superseded by xwidget support -- see mu4e-views below
  (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  ;; (setq mu4e-html2text-command 'mu4e-shr2text)

  ;; View in browser
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser) t)
  ;; Other options for rendering
  ;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")


  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;;;; Composing Email

  ;; Use mu4e system-wide
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Don't keep message compose buffers around after sending:
  (setq message-kill-buffer-on-exit t)
  ;;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; Only ask if a context hasn't been previously picked
  (setq mu4e-compose-context-policy 'ask-if-none)

  ;; Possible fix for outlook client reading problems in inline messages
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Writing-messages.html#How-can-I-avoid-Outlook-display-issues_003f
  (setq  message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote...")

  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

;;;; Sending Mail

  ;; Configure the function to use for sending mail
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; NOTE: Only use this if you have set up a GPG key!
  ;; Automatically sign all outgoing mails
  ;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  ;; Move messages to trash
  ;; See https://rakhim.org/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/
  ;; FIXME: This wont work since I have two email accounts
  ;; (fset 'cpm--email-move-to-trash "mTrash")
  ;; (define-key mu4e-headers-mode-map (kbd "d") 'cpm--email-move-to-trash)
  ;; (define-key mu4e-view-mode-map (kbd "d") 'cpm--email-move-to-trash)

;;;; Contexts

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
                  ;; use keychain for credentials
                  (smtpmail-smtp-service . 1025)
                  (smtpmail-stream-type  . nil)
                  (mu4e-compose-signature . (concat
                                             "Colin McLear\n"
                                             "Associate Professor\n"
                                             "Department of Philosophy\n"
                                             "University of Nebraska–Lincoln\n"
                                             "https://www.colinmclear.net"))
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
                  (mu4e-compose-signature . (concat
                                             "Colin McLear"))
                  (mu4e-drafts-folder  . "/Fastmail/Drafts")
                  (mu4e-sent-folder  . "/Fastmail/Sent")
                  (mu4e-refile-folder  . "/Fastmail/Archive")
                  (mu4e-trash-folder  . "/Fastmail/Trash")))))

  ;; Ask for context if none is set
  (setq mu4e-context-policy 'pick-first)

;;;; Quick Actions

  ;; Helpful discussion at
  ;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-05.org
  (defun cpm/capture-mail-follow-up (msg)
    "Capture for message follow-up"
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "mf"))

  (defun cpm/capture-mail-read-later (msg)
    "Capture for message read-later"
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "mr"))

  ;; Add custom actions for our capture templates
  (add-to-list 'mu4e-headers-actions
               '("follow up" . cpm/capture-mail-follow-up) t)
  (add-to-list 'mu4e-view-actions
               '("follow up" . cpm/capture-mail-follow-up) t)
  (add-to-list 'mu4e-headers-actions
               '("read later" . cpm/capture-mail-read-later) t)
  (add-to-list 'mu4e-view-actions
               '("read later" . cpm/capture-mail-read-later) t)

;;;; Mail Custom Bookmarks/Searches

  (setq mu4e-bookmarks '((:name "Inbox" :query "m:/UNL/inbox or m:/Fastmail/inbox" :key ?i)
                         (:name "Unread" :query "flag:unread AND NOT flag:trashed" :key ?u)
                         (:name "Drafts" :query "m:/UNL/drafts or m:/Fastmail/drafts" :key ?d)
                         (:name "Sent Mail" :query "m:/UNL/sent or m:/Fastmail/sent" :key ?s)
                         (:name "-----" :query "m:/UNL/inbox" :hide-unread t :key ?-)
                         (:name "Today" :query "date:today..now" :key ?t)
                         (:name "Yesterday" :query "date:2d..today and not date:today..now" :key ?y)
                         (:name "Last Week" :query "date:7d..now" :key ?w)
                         (:name "Last Month" :query "date:4w..now" :key ?m)
                         (:name "-----" :query "m:/UNL/inbox" :hide-unread t :key ?-)
                         (:name "Archive" :query "m:/UNL/archive or m:/Fastmail/archive" :key ?a)
                         (:name "Important" :query "flag:flagged" :key ?!)
                         (:name "Attachments" :query "flag:attach" :key ?A)
                         (:name "Messages with images" :query "mime:image/*" :key ?I)))

;;;; Miscellaneous

  ;; Store link to message if in header view, not to header query
  (setq mu4e-org-link-query-in-headers-mode nil)

  ;; Quickly store links for search queries
  (defun cpm/store-link-to-mu4e-query ()
    (interactive)
    (let ((org-mu4e-link-query-in-headers-mode t))
      (call-interactively 'org-store-link))))


;;;; End Mu4e

;;; Better Viewing – Mu4e Views
;; This makes mu4e render html emails in emacs via xwidgets.
;; It basically reproduces a modern email client experience. Depends on compiling emacs with xwidgets
;; to check that exwidgets are installed
;; evaluate (xwidget-webkit-browse-url "https://www.gnu.org/")
;; NOTE: need to add something about not loading remote images (mu4e-view-show-images ?)

(use-package mu4e-views
  :straight (mu4e-views :type git :host github :repo "lordpretzel/mu4e-views")
  :after mu4e
  :defer nil
  :bind (:map mu4e-headers-mode-map
	     ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
         ("C-v" . cpm/mu4e-text-view-toggle)
	     ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
	     ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
         ("f" . mu4e-views-toggle-auto-view-selected-message) ;; toggle opening messages automatically when moving in the headers view
         ("i" . mu4e-views-mu4e-view-as-nonblocked-html) ;; show currently selected email with all remote content
	     )
  :config
  (setq mu4e-views-completion-method 'default) ;; use default completion
  (setq mu4e-views-default-view-method "text") ;; make text the default
  (mu4e-views-mu4e-use-view-msg-method "text") ;; select the default
  ;; when pressing n and p stay in the current window
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
  ;; automatically open messages when moving in the headers view
  (setq mu4e-views-auto-view-selected-message t)

  (add-to-list 'mu4e-headers-actions
               '("e: export message" . mu4e-views-export-msg-action) t)
  (add-to-list 'mu4e-view-actions
               '("e: export message" . mu4e-views-export-msg-action) t))

(defun cpm/mu4e-text-view-toggle ()
  "Toggle between text and html-block views in mu4e."
  (interactive)
  (let ((view (mu4e-views--get-current-viewing-method-name)))
    (if (string= "text" view)
        (mu4e-views-view-current-msg-with-method "html")
      (mu4e-views-view-current-msg-with-method "text"))))

;;; Mu4e Icons
;; Use nice icons
(use-package mu4e-marker-icons
  :straight t
  :after mu4e
  :init (mu4e-marker-icons-mode 1))

;;; Mu4e Goodies
(use-package mu4e-goodies
  :straight (:type git :host github :repo "panjie/mu4e-goodies")
  :after mu4e
  :config/el-patch
  (defun mu4e-goodies-check-keywords ()
    (interactive "P")
    (let ((it (car mu4e-goodies-keywords))
          (list (cdr mu4e-goodies-keywords))
          (key-pos)
          (msg))
      (while (and (not key-pos) it)
        (unless (and (setq key-pos (mu4e-goodies-search-body-subject (car it)))
                     (not (funcall (cdr (assoc (cdr it) mu4e-goodies-rule-func)))))
          (setq key-pos nil)
          (setq it (car list)
                list (cdr list))))
      (when key-pos
        (goto-char (car key-pos))
        (overlay-put (make-overlay (car key-pos) (cdr key-pos)) 'face 'hi-yellow)
        (cond
         ((eq (cdr it) 'check-attach) (setq msg "You may have forgotten your attachment!"))
         ((eq (cdr it) 'check-cc) (setq msg "You may have forgotten your Cc!")))
        (setq msg (concat msg " Really send message?"))
        (or (y-or-n-p msg)
            (keyboard-quit)))))
  :config
  ;; add words to check attachments & cc list
  (add-to-list 'mu4e-goodies-keywords '("[aA]ttached" . check-attach))
  (add-to-list 'mu4e-goodies-keywords '("[cC]c'd" . check-cc))
  (add-to-list 'mu4e-goodies-keywords '("C[cC]'d" . check-cc))
  (add-to-list 'mu4e-goodies-keywords '("CCd" . check-cc))
  (require 'mu4e-goodies))

;;; Using Org & HTML (Org-MSG)
(use-package org-msg
  :straight (:type git :host github :repo "jeremy-compostella/org-msg")
  :after mu4e
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
	    org-msg-startup "hidestars indent inlineimages"
	    org-msg-greeting-fmt nil
	    org-msg-recipient-names nil
	    org-msg-greeting-name-limit 3
	    org-msg-default-alternatives '((new		        . (text html))
				                       (reply-to-html	. (text html))
				                       (reply-to-text	. (text)))
	    org-msg-convert-citation t)
  (org-msg-mode))


;;; Mu4e Dashboard
(use-package mu4e-dashboard
  :straight (:host github :type git :repo "rougier/mu4e-dashboard")
  :after mu4e)

;;; SVG Tag Mode
(use-package svg-tag-mode
  :straight (:type git :host github :repo "rougier/svg-tag-mode")
  :after mu4e)

;;; Sidebar
(use-package nano-sidebar
  :commands (nano-sidebar-toggle)
  :straight (:type git :host github :repo "rougier/nano-sidebar")
  :config
  (defun nano-sidebar-mu4e-init (frame sidebar)
    (select-frame sidebar)
    (find-file (concat cpm-elisp-dir "mu4e-dashboard.org"))
    (mu4e-dashboard-mode)
    (hl-line-mode)
    (set-window-dedicated-p nil t)
    (forward-char 8)
    (setq header-line-format "")
    (setq mode-line-format nil))

;;; Automatic mu4e dashboard sidebar when open mu4e
  (add-to-list 'nano-sidebar-properties
               `("mu4e"    36 dark ,bespoke-background nano-sidebar-mu4e-init) t)
  (set-frame-parameter nil 'name "mu4e")
  (nano-sidebar-toggle))

;;; End Setup Email

(provide 'setup-email)
