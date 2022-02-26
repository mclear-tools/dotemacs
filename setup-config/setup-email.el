;; Email settings
;; Assembled from many sources
;; I use mbsync and mu4e
;; For styling resources see:
;; https://github.com/rougier/nano-emacs/blob/master/nano-mu4e.el

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

;;;;; Header View Functions
  (defun mu4e-get-account (msg)
    (let* ((maildir (mu4e-message-field msg :maildir))
           (maildir (substring maildir 1)))
      (nth 0 (split-string maildir "/"))))

  (defun mu4e-get-maildir (msg)
    (let* ((maildir (mu4e-message-field msg :maildir))
           (maildir (substring maildir 1)))
      (nth 0 (reverse (split-string maildir "/")))))

  (defun mu4e-get-mailbox (msg)
    (format "%s|%s" (mu4e-get-account msg) (mu4e-get-maildir msg)))

  (defun mu4e-headers-tag (text tag face help query)
    "Make a clickable button with specified FACE displaying TEXT.
   When hovered, HELP is displayed. When clicked, mu4e QUERY is executed."
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map 'mu4e-headers-mode-map)
      (define-key map [mouse-1] `(lambda ()
                                   (interactive) (mu4e-headers-search ,query)))
      (concat
       (propertize text
                   'display tag
                   'face face
                   'mouse-face `(:foreground ,bespoke-salient)
                   'local-map map
                   'help-echo `(lambda (window _object _point)
                                 (let (message-log-max) (message ,help))))
       " ")))

  ;; Buttons
  (defun mu4e-headers-button (text face help query)
    "Make a clickable button with specified FACE displaying TEXT.
   When hovered, HELP is displayed. When clicked, mu4e QUERY is executed."
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map 'mu4e-headers-mode-map)
      (define-key map [mouse-1] `(lambda ()
                                   (interactive) (mu4e-headers-search ,query)))
      (propertize text
                  'face face
                  'mouse-face `(:foreground ,bespoke-background
                                :background ,bespoke-faded)
                  'local-map map
                  'help-echo `(lambda (window _object _point)
                                (let (message-log-max) (message ,help))))))

  (defun mu4e-headers-date-button (date face)
    (concat
     (mu4e-headers-button (format-time-string "%d" date)
                          face
                          (format-time-string "Mails from %d %B %Y" date)
                          (format-time-string "date:%Y%m%d" date))
     (propertize "/" 'face face)
     (mu4e-headers-button (format-time-string "%m" date)
                          face
                          (format-time-string "Mails from %B %Y" date)
                          (format-time-string "date:%Y%m" date))
     (propertize "/" 'face face)
     (mu4e-headers-button (format-time-string "%Y" date)
                          face
                          (format-time-string "Mails from %Y" date)
                          (format-time-string "date:%Y" date))))
  ;; Relative dates
  (defun mu4e-headers-is-today (date)
    (= (- (time-to-days (current-time)) (time-to-days date)) 0))

  (defun mu4e-headers-is-yesterday (date)
    (= (- (time-to-days (current-time)) (time-to-days date)) 1))

  (defun mu4e-headers-relative-date (msg)
    (let* ((thread  (mu4e-message-field msg :thread))
           (level (plist-get thread :level))
           (empty-parent (and thread (plist-get thread :empty-parent)))
           (child   (and thread (> (plist-get thread :level) 0)))
           (unread  (memq 'unread  (mu4e-message-field msg :flags)))
           (date (mu4e-msg-field msg :date))
           (diff (- (time-to-days (current-time)) (time-to-days date)))
           (face 'bespoke-salient))
      (setq face 'bespoke-faded)
      (cond ((mu4e-headers-is-today date)
             (mu4e-headers-button (format-time-string "     %H:%M" date)
                                  face
                                  (format-time-string "Mails from today")
                                  (format-time-string "date:%Y%m%d" date)))
            ((mu4e-headers-is-yesterday date)
             (mu4e-headers-button " Yesterday"
                                  face
                                  (format-time-string "Mails from yesterday")
                                  (format-time-string "date:%Y%m%d" date)))
            (t  (mu4e-headers-date-button date face)))))

  ;; Style & determine what flags to show
  (defun mu4e-headers-attach (msg)
    (cond ((memq 'flagged  (mu4e-message-field msg :flags))
           (propertize "!" 'face 'bespoke-strong))
          ((memq 'attach  (mu4e-message-field msg :flags))
           (propertize "" 'face 'bespoke-faded))
          (t " ")))

;;;;; Headers
  ;; Set headers
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                           :shortname ""
                           :function (lambda (msg) "  "))))

  (add-to-list 'mu4e-header-info-custom
               '(:relative-date . (:name "Relative date"
                                   :shortname ""
                                   :function mu4e-headers-relative-date)))

  (add-to-list 'mu4e-header-info-custom
               '(:mailbox-short . (:name "Mailbox"
                                   :shortname ""
                                   :function mu4e-get-mailbox)))

  (add-to-list 'mu4e-header-info-custom
               '(:attach . (:name "Attachment"
                            :shortname ""
                            :function mu4e-headers-attach)))


  (setq mu4e-headers-date-format "%D";; "%Y-%m-%d %H:%M:%S"
        mu4e-headers-fields '((:empty          .   1)
                              (:relative-date  .  12)
                              (:from-or-to     .  40)
                              (:subject        .  90)
                              (:tags           .  20)
                              (:mailbox-short  .  17)
                              (:attach         .   2)
                              ))

  ;; how to handle html-formatted emails
  ;; View in browser
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser) t)

  ;; Other options for rendering
  ;; NOTE: superseded by xwidget support -- see mu4e-views below
  (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  ;; (setq mu4e-html2text-command 'mu4e-shr2text)
  ;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

  ;; other display settings
  (setq mu4e-speedbar-support t)
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-completing-read-function 'completing-read)
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;;;; Composing Email

  ;; Use mu4e system-wide
  (setq mail-user-agent 'mu4e-user-agent)

  ;; List of your email adresses:
  (setq mu4e-user-mail-address-list '("mclear@fastmail.com"
                                      "mclear@unl.edu"))

  ;; Compose in new frame
  (setq mu4e-compose-in-new-frame t)

  ;; Don't keep message compose buffers around after sending:
  (setq message-kill-buffer-on-exit t)
  ;;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; Only ask if a context hasn't been previously picked
  (setq mu4e-compose-context-policy 'ask-if-none)

  ;; Possible fix for outlook client reading problems in inline messages
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Writing-messages.html#How-can-I-avoid-Outlook-display-issues_003f
  (setq  message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote...")

  ;; Check spelling
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
                                             "[[https://www.colinmclear.net]]"))
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
                  (mu4e-sent-folder  . "/Fastmail/Sent Items")
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
                         (:name "Trash" :query "m:/UNL/Trash or m:/Fastmail/Trash" :key ?T)
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

;;;; Better Tagging/Marking

  ;;--- Nicer actions display using emoji tags -----------------------------------
  ;; (plist-put (cdr (assq 'refile   mu4e-marks)) :char "⨯")
  (plist-put (cdr (assq 'refile   mu4e-marks)) :char "📂")
  (plist-put (cdr (assq 'unread   mu4e-marks)) :char "📩")
  (plist-put (cdr (assq 'trash    mu4e-marks)) :char "🗑️")
  (plist-put (cdr (assq 'untrash  mu4e-marks)) :char " ")
  (plist-put (cdr (assq 'delete   mu4e-marks)) :char "🧨")
  (plist-put (cdr (assq 'flag     mu4e-marks)) :char "🚩")
  (plist-put (cdr (assq 'unflag   mu4e-marks)) :char " ")
  (plist-put (cdr (assq 'move     mu4e-marks)) :char "📂")
  (plist-put (cdr (assq 'tag      mu4e-marks)) :char "👀")

  (setq mu4e-headers-show-target nil)

  (set-face-attribute 'mu4e-header-marks-face nil :inherit 'bold)

  ;; Add SVG tags
  ;; FIXME: unmarking doesn't remove SVG tags
  (defun mu4e-mark-at-point-advice (mark target)
    (interactive)
    (require 'svg-tag-mode)
    (let* ((msg (mu4e-message-at-point))
           (docid (mu4e-message-field msg :docid))
           (overlay (make-overlay (- (line-end-position) 10)
                                  (- (line-end-position) 0))))
      (save-excursion
        ;; (remove-overlays (line-beginning-position) (line-end-position))
        (delete-overlay (make-overlay (line-beginning-position) (line-end-position)))
        (if (eql mark 'unmark)
            (delete-overlay overlay)
          (cond ((eql mark 'refile)
                 (overlay-put overlay 'display (svg-tag-make "ARCHIVE" 'success 3 0)))
                ((eql mark 'trash)
                 (overlay-put overlay 'display (svg-tag-make "TRASH" 'error 5 0)))
                ((eql mark 'untrash)
                 (overlay-put overlay 'display (svg-tag-make "UNTRASH" 3 0)))
                ((eql mark 'delete)
                 (overlay-put overlay 'display (svg-tag-make "DELETE" 'error 4 0)))
                ((eql mark 'unread)
                 (overlay-put overlay 'display (svg-tag-make "UNREAD" 4 0)))
                ((eql mark 'flag)
                 (overlay-put overlay 'display (svg-tag-make "FLAG" 'warning 6 0)))
                ((eql mark 'unflag)
                 (overlay-put overlay 'display (svg-tag-make "UNFLAG" 4 0)))
                ((eql mark 'move)
                 (overlay-put overlay 'display (svg-tag-make "MOVE" 'success 6 0)))
                ((eql mark 'tag)
                 (overlay-put overlay 'display (svg-tag-make "TAG" 'shadow 7 0))))))))

  (advice-add 'mu4e-mark-at-point :after #'mu4e-mark-at-point-advice)


;;;; Miscellaneous

  (setq mu4e-completing-read-function 'completing-read)

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
  ;; Add completing read
  (add-to-list 'mu4e-marks
               '(tag
                 :char       ("g" . " ")
                 :prompt     "gtag"
                 :ask-target (lambda () (cpm/select-mail-tag))
                 :action      (lambda (docid msg target)
                                (mu4e-action-retag-message msg target))))
  (mu4e~headers-defun-mark-for tag)
  (define-key mu4e-headers-mode-map (kbd "G") 'mu4e-headers-mark-for-tag)
  (define-key-after (lookup-key mu4e-headers-mode-map [menu-bar headers])
    [mark-tag] '("Mark for tag" . mu4e-headers-mark-for-tag) 'mark-pattern)


  ;; actions to add tags
  (add-to-list 'mu4e-view-actions
               '("add/remove tags" . mu4e-action-retag-message) t)
  :config
  ;; add words to check attachments & cc list
  (add-to-list 'mu4e-goodies-keywords '("[aA]ttached" . check-attach))
  (add-to-list 'mu4e-goodies-keywords '("[cC]c'd" . check-cc))
  (add-to-list 'mu4e-goodies-keywords '("C[cC]'d" . check-cc))
  (add-to-list 'mu4e-goodies-keywords '("CCd" . check-cc))
  (require 'mu4e-goodies))

(defvar cpm-mail-tags
  '("phil105"
    "phil232"
    "phil871"
    "phil880"
    "phil971"
    "grad-admissions"
    "referee-reports"
    "publications"
    "conferences"
    "casrac"
    "edited-volume"
    ))

(defun cpm/select-mail-tag ()
  (interactive)
  (completing-read "Select Tag (+/-): " cpm-mail-tags))

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

  (defun cpm/org-msg-hooks ()
    "Hooks for org-msg"
    (progn
      (auto-fill-mode -1)
      (hl-line-mode 1)
      (company-mode 1)
      ;; FIXME: Try remove auto-save hook *locally* to avoid multiple saved drafts
      (remove-hook 'auto-save-hook #'cpm/full-auto-save t)))
  (add-hook 'org-msg-edit-mode-hook #'cpm/org-msg-hooks)

  (org-msg-mode))

;;; Email Addressing
;; function to return first name of email recipients
;; used by yasnippet
;; inspired by
;;http://blog.binchen.org/posts/how-to-use-yasnippets-to-produce-email-templates-in-emacs.html
;; http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/

(defun cpm/mu4e-get-names-for-yasnippet ()
  "Return comma separated string of names for an email"
  (interactive)
  (let ((email-name "") str email-string email-list email-name2 tmpname)
    (save-excursion
      (goto-char (point-min))
      ;; first line in email could be some hidden line containing NO to field
      (setq str (buffer-substring-no-properties (point-min) (point-max))))
    ;; take name from TO field - match series of names
    (when (string-match "^To: \"?\\(.+\\)" str)
      (setq email-string (match-string 1 str)))
    ;;split to list by comma
    (setq email-list (split-string email-string " *, *"))
    ;;loop over emails
    (dolist (tmpstr email-list)
      ;;get first word of email string
      (setq tmpname (car (split-string tmpstr " ")))
      ;;remove whitespace or ""
      (setq tmpname (replace-regexp-in-string "[ \"]" "" tmpname))
      ;;join to string
      (setq email-name
            (concat email-name ", " tmpname)))
    ;;remove initial comma
    (setq email-name (replace-regexp-in-string "^, " "" email-name))

    ;;see if we want to use the name in the FROM field
    ;;get name in FROM field if available, but only if there is only
    ;;one name in TO field
    (if (< (length email-list) 2)
        (when (string-match "^\\([^ ,\n]+\\).+writes:$" str)
          (progn (setq email-name2 (match-string 1 str))
                 ;;prefer name in FROM field if TO field has "@"
                 (when (string-match "@" email-name)
                   (setq email-name email-name2))
                 )))
    email-name))

;;; End Setup Email

(provide 'setup-email)
