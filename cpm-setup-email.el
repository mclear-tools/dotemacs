;;; setup-email.el --- summary -*- lexical-binding: t -*-

;; Author: Colin McLear
;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;;; Commentary:

;; Email settings; assembled from many sources. I use mbsync and mu4e. For
;; styling resources see: https://github.com/rougier/nano-emacs/blob/master/nano-mu4e.el

;;; Code:

;;;; Mu4e
(use-package mu4e
  ;; Tell package system to use homebrew mu4e. NOTE: This means that the user
  ;; must install mu & mu4e, which comes with it, on their system independently
  ;; of emacs.
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
  :commands (mu4e mu4e-compose-new mu4e-update-mail-and-index)
  :config/el-patch
  ;; Open mu org links in Email tab
  ;; :TEST: Is redefining the function the best way or should I use advice?
  (defun mu4e-org-open (link)
    "Open the org LINK in `Email' tab.
Open the mu4e message (for links starting with 'msgid:') or run
the query (for links starting with 'query:')."
    (require 'mu4e)
    (require 'tabspaces)
    (cond
     ((string-match "^msgid:\\(.+\\)" link)
      (if (member "Email" (tabspaces--list-tabspaces))
          (tab-bar-switch-to-tab "Email")
        (cpm-open-email-in-workspace))
      (mu4e-view-message-with-message-id (match-string 1 link)))
     ((string-match "^query:\\(.+\\)" link)
      (if (member "Email" (tabspaces--list-tabspaces))
          (tab-bar-switch-to-tab "Email")
        (cpm-open-email-in-workspace))
      (mu4e-headers-search (match-string 1 link) current-prefix-arg))
     (t (mu4e-error "Unrecognized link type '%s'" link))))
  :config
  ;; Finding the binary (installed w/homebrew)
  (setq mu4e-mu-binary (executable-find "mu"))
  ;; List of your email adresses:
  (setq mu4e-user-mail-address-list '("mclear@fastmail.com"
                                      "mclear@unl.edu"))
  ;; Use mu4e system-wide
  (setq mail-user-agent #'mu4e-user-agent)
  ;; Use completing-read
  (setq mu4e-completing-read-function 'completing-read))

;;;;; Get Mail & Sync
(with-eval-after-load 'mu4e
  ;; Maildir
  (setq mu4e-maildir "~/.maildir")
  ;; Sync imap servers w/mbsync (via isync installed w/homebrew):
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  ;; Change filenames when moving
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  ;; i.e. makes sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)
  ;; Refresh mail using mbsync every 5 minutes
  (setq mu4e-update-interval (* 5 60))
  ;; Updating
  ;; FIXME: right now this causes an updating loop for some reason
  ;; (add-hook 'mu4e-main-mode-hook #'mu4e-update-index)
  (bind-key "u" #'mu4e-update-index mu4e-main-mode-map))

;;;;; Composing Email
(with-eval-after-load 'mu4e
  ;; Compose in new frame
  (setq mu4e-compose-in-new-frame t)
  ;; Don't keep message compose buffers around after sending:
  (setq message-kill-buffer-on-exit t)
  ;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)
  ;; Only ask if a context hasn't been previously picked
  (setq mu4e-compose-context-policy 'ask-if-none)
  ;; Possible fix for outlook client reading problems in inline messages
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Writing-messages.html#How-can-I-avoid-Outlook-display-issues_003f
  (setq message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote..."
        message-citation-line-function #'message-insert-formatted-citation-line))

;;;;; Viewing

;;;;;; Header View Functions
(with-eval-after-load 'mu4e
  ;; TODO: fix faces so they inherit and don't rely on lambda-themes
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
                   'mouse-face '(:foreground homoglyph)
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
                  'mouse-face `(:foreground lambda-bg
                                :background lambda-mild)
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
           (face 'lambda-focus))
      (setq face 'lambda-meek)
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
           (propertize "!" 'face 'lambda-strong))
          ((memq 'attach  (mu4e-message-field msg :flags))
           (propertize "" 'face 'lambda-mild))
          (t " "))))

;;;;;; Headers
(with-eval-after-load 'mu4e
  ;; Add some custom headers
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

  ;; Set headers
  (setq mu4e-headers-date-format "%D";; "%Y-%m-%d %H:%M:%S"
        mu4e-headers-fields '(
                              (:flags          .  8)
                              (:mailbox-short  .  10)
                              ;; (:relative-date  .  11)
                              (:tags           .  12)
                              (:from-or-to     .  20)
                              (:subject        .  120)))

  ;; Handle html-formatted emails
  ;; View in browser (note there is also option to view as xwidget)
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser) t)
  ;; Prefer text
  ;;See https://www.djcbsoftware.nl/code/mu/mu4e/MSGV-Rich_002dtext-and-images.html
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext"))
  ;; Fix visibility in dark mode for rich text
  ;; (setq shr-color-visible-luminance-min 80)

  ;; Don't show all related emails
  (setq mu4e-headers-include-related nil)

  ;; other display settings
  (setq mu4e-speedbar-support t)
  ;; Store link to message if in header view, not to header query
  (setq mu4e-org-link-query-in-headers-mode nil)
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode))

;; Quickly store links for search queries
(defun lem-store-link-to-mu4e-query ()
  (interactive)
  (let ((mu4e-org-link-query-in-headers-mode t))
    (call-interactively 'org-store-link)))

;; Execute Marks
(with-eval-after-load 'mu4e
  (bind-key "x" (lambda() (interactive) (mu4e-mark-execute-all t)) mu4e-headers-mode-map)
  (bind-key "x" (lambda() (interactive) (mu4e-mark-execute-all t)) mu4e-view-mode-map))


;;;;; Sending Mail
(with-eval-after-load 'mu4e
  (require 'smtpmail)
  ;; send function:
  (setq send-mail-function 'sendmail-send-it
        message-send-mail-function 'sendmail-send-it)

  ;; send program:
  ;; this is exeranal. remember we installed it before.
  (setq sendmail-program (executable-find "msmtp"))

  ;; select the right sender email from the context.
  (setq message-sendmail-envelope-from 'header)

  ;; Configure the function to use for sending mail
  ;; (setq message-send-mail-function #'smtpmail-send-it)
  (setq smtpmail-queue-dir (concat mu4e-maildir "/queued-mail/"))
  (setq smtpmail-debug-info t)
  ;; :TEST: Try a fix for encoding issues with sent mail especially
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64)))

;;;;; Contexts
(with-eval-after-load 'mu4e
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
                  (smtp-auth-credentials "security find-generic-password -s mbsync-unl-pass -w")
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
                  (user-full-name    . "Colin McLear")
                  (smtpmail-smtp-server  . "smtp.fastmail.com")
                  (smtpmail-user-mail-address . "mclear@fastmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  ;; use keychain for credentials
                  (smtp-auth-credentials "security find-generic-password -s mbsync-fastmail-pass -w")
                  (mu4e-compose-signature . (concat
                                             "Colin McLear\n"
                                             "[[https://www.colinmclear.net]]"))
                  (mu4e-drafts-folder  . "/Fastmail/Drafts")
                  (mu4e-sent-folder  . "/Fastmail/Sent Items")
                  (mu4e-refile-folder  . "/Fastmail/Archive")
                  (mu4e-trash-folder  . "/Fastmail/Trash")))))

  ;; Ask for context if none is set
  (setq mu4e-context-policy 'pick-first))

;;;;; Maildirs
(with-eval-after-load 'mu4e
  ;; Frequently used maildirs
  (setq mu4e-maildir-shortcuts '((:maildir "/Fastmail/Archive"    :key ?a)
                                 (:maildir "/Fastmail/Inbox"      :key ?i)
                                 (:maildir "/Fastmail/Starred"    :key ?w)
                                 (:maildir "/Fastmail/Sent Items" :key ?s)
                                 (:maildir "/UNL/Inbox"           :key ?I)
                                 (:maildir "/UNL/Archive"         :key ?A)
                                 (:maildir "/UNL/Archive1"        :key ?1)
                                 (:maildir "/UNL/Sent"            :key ?S)))

  ;; Show maildirs with 0 messages
  (setq mu4e-main-hide-fully-read nil))


;;;;; Searching
;; Limit searches?
(setq mu4e-search-results-limit -1)

;;;;; Attachments
(with-eval-after-load 'mu4e
  ;; Set default dir
  (setq mu4e-attachment-dir (concat (getenv "HOME") "/Downloads"))

  ;; Save all attachments
  ;; https://github.com/sje30/emacs/blob/master/mu4e-view-save-all-attachments.el
  ;; Suggested keybinding mnemnonic: > is to redirect the files to output
  ;; everything. (define-key mu4e-view-mode-map ">"
  ;; 'mu4e-view-save-all-attachments)

  (defvar bulk-saved-attachments-dir mu4e-attachment-dir)

  (defun cleanse-subject (sub)
    (replace-regexp-in-string
     "[^A-Z0-9]+"
     "-"
     (downcase sub)))

  (defun mu4e-view-save-all-attachments (&optional arg)
    "Save all MIME parts from current mu4e gnus view buffer."
    ;; Copied from mu4e-view-save-attachments
    (interactive "P")
    (cl-assert (and (eq major-mode 'mu4e-view-mode)
                    (derived-mode-p 'gnus-article-mode)))
    (let* ((msg (mu4e-message-at-point))
           (id (cleanse-subject (mu4e-message-field msg :subject)))
           (attachdir (concat bulk-saved-attachments-dir "/" id))
	       (parts (mu4e~view-gather-mime-parts))
           (handles '())
           (files '())
           dir)
      (mkdir attachdir t)
      (dolist (part parts)
        (let ((fname (or
		              (cdr (assoc 'filename (assoc "attachment" (cdr part))))
                      (seq-find #'stringp
                                (mapcar (lambda (item) (cdr (assoc 'name item)))
                                        (seq-filter 'listp (cdr part)))))))
          (when fname
            (push `(,fname . ,(cdr part)) handles)
            (push fname files))))
      (if files
          (progn
            (setq dir
		          (if arg (read-directory-name "Save to directory: ")
		            attachdir))
            (cl-loop for (f . h) in handles
                     when (member f files)
                     do (mm-save-part-to-file h
					                          (cpm-next-free
					                           (expand-file-name f dir)))))
        (mu4e-message "No attached files found"))))

  (defun cpm-next-free (file)
    "Return name of next unique 'free' FILE.
If /tmp/foo.txt and /tmp/foo-1.txt exist, when this is called
with /tmp/foo.txt, return /tmp/foo-2.txt.  See
`cpm-test-next-free' for a test case.  This is not very efficient
if there are a large number of files already in the directory
with the same base name, as it simply starts searching from 1
each time until it finds a gap.  An alternative might be to do a
wildcard search for all the filenames, extract the highest number
and then increment it."
    ;; base case is easy; does file exist already?
    (if (not  (file-exists-p file))
        file
      ;; othwerwise need to iterate through f-1.pdf
      ;; f-2.pdf, f-3.pdf ... until we no longer find a file.
      (let ((prefix (file-name-sans-extension file))
	        (suffix (file-name-extension file))
	        (looking t)
	        (n 0)
	        (f)
	        )
        (while looking
	      (setq n (1+ n))
	      (setq f (concat prefix "-" (number-to-string n) "." suffix))
	      (setq looking (file-exists-p f)))
        f)))

  (defun cpm-test-next-free ()
    (let (f)
      (dotimes (i 100)
        (setq f (cpm-next-free "/tmp/rabbit.txt"))
        (write-region "hello" nil f))))

  ;; Check Attachments
  ;; See https://github.com/panjie/mu4e-goodies

  (require 'hi-lock)

  (defvar lem-mail-rule-func
    '((check-attach . lem-mail-draft-attach-p)
      (check-cc . lem-mail-draft-cc-p)))

  (defvar lem-mail-keywords
    '(("[aA]ttachment" . check-attach)
      ("[aA]ttached" . check-attach)
      ("[cC]c'd" . check-cc)
      ("C[cC]'d" . check-cc)
      ("CCd" . check-cc))
    "Keywords to be alerted. An alist like:
  \( (regexp-of-keywords . rules-for-keywords) ... )")

  (defun lem-mail-draft-attach-p ()
    "Check if current email draft has at least one attachment."
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "\<#part .*filename=.*" (point-max) t)))

  (defun lem-mail-draft-cc-p ()
    "Check if current email draft has cc field."
    (message-fetch-field "Cc"))

  (defun lem-mail-search-body-subject (keyword &optional start)
    "Search for keyword in the current mail's subject and body. Return
  the pos of the keyword which is a cons cell, nil if not found."
    ;; check for subject
    (save-excursion
      (if (and start (<= start (point-max)))
          (goto-char start)
        (message-goto-subject))
      (if (re-search-forward keyword (point-max) t)
          ;; check if the keyword is found in a cited line
          (let ((current-pos (point)))
            (beginning-of-line)
            (if (or (search-forward message-yank-prefix
                                    (+ (point) (length message-yank-prefix))
                                    t)
                    (search-forward message-yank-cited-prefix
                                    (+ (point) (length message-yank-cited-prefix))
                                    t))
                (lem-mail-search-body-subject keyword current-pos)
              (cons (match-beginning 0) (match-end 0))))
        nil)))

  (add-hook 'message-send-hook
            (defun lem-mail-check-keywords ()
              (interactive "P")
              (let ((it (car lem-mail-keywords))
                    (list (cdr lem-mail-keywords))
                    (key-pos)
                    (msg))
                (while (and (not key-pos) it)
                  (unless (and (setq key-pos (lem-mail-search-body-subject (car it)))
                               (not (funcall (cdr (assoc (cdr it) lem-mail-rule-func)))))
                    (setq key-pos nil)
                    (setq it (car list)
                          list (cdr list))))
                (when key-pos
                  (goto-char (car key-pos))
                  (overlay-put (make-overlay (car key-pos) (cdr key-pos)) 'face 'hi-yellow)
                  (cond
                   ((eq (cdr it) 'check-attach) (setq msg "You may forget your attachment!"))
                   ((eq (cdr it) 'check-cc) (setq msg "You may forget your Cc!")))
                  (setq msg (concat msg " Really send message?"))
                  (or (y-or-n-p msg)
                      (keyboard-quit))))))

  (defun lem-mail-check-keywords ()
    (interactive "P")
    (let ((it (car lem-mail-keywords))
          (list (cdr lem-mail-keywords))
          (key-pos)
          (msg))
      (while (and (not key-pos) it)
        (unless (and (setq key-pos (lem-mail-search-body-subject (car it)))
                     (not (funcall (cdr (assoc (cdr it) lem-mail-rule-func)))))
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
            (keyboard-quit))))))

;;;;; Mu4e & Swiftbar
(with-eval-after-load 'mu4e
  (defun lem-swiftbar-email-update ()
    "Update swiftbar mail plugin"
    (interactive)
    (eshell-command "open -g 'swiftbar://refreshplugin?name=mail-mu'")
    (message "Swiftbar updated!"))

  (add-hook 'mu4e-index-updated-hook #'lem-swiftbar-email-update))

;;;;; Mail Tagging
(with-eval-after-load 'mu4e
  ;; Tag mail messages
  ;; See https://github.com/panjie/mu4e-goodies

  ;; Helper functions/vars
  ;;--------------------------------------------------
  (defsubst lem-mail~get-real-addr (addr)
    "Parse addr which is the result of mu4e-message-fields to get
the real email address"
    (if (listp addr)       ;; already parsed by mu4e
        (cdr (car addr))
      (if (stringp addr)   ;; raw address like: "ABC <abc@abc.com>"
          (car (mail-header-parse-address addr)))))

  (defvar lem-mail~header-handlers nil
    "Internal handlers of header view for mu >= 1.5")

  (defun lem-mail~header-advice (orig-func &rest args)
    "General advice for plugins for header view"
    (let* ((str (apply orig-func args))
           (msg (car args))
           (field (cadr args)))
      (dolist (func lem-mail~header-handlers)
        (setq str (funcall func msg field (mu4e-message-field msg field) str)))
      str))

  (when (functionp 'mu4e~headers-field-value) ; mu >= 1.5
    (advice-add 'mu4e~headers-field-value :around #'lem-mail~header-advice))

  ;; Tags for emails (from info pages of mu4e)
  ;;--------------------------------------------------
  ;; Add completing read
  (add-to-list 'mu4e-marks
               '(tag
                 :char       ("g" . " ")
                 :prompt     "gtag"
                 :ask-target (lambda () (lem-select-mail-tag))
                 :action      (lambda (docid msg target)
                                (mu4e-action-retag-message msg target))))

  (mu4e~headers-defun-mark-for tag)
  (define-key mu4e-headers-mode-map (kbd "G") 'mu4e-headers-mark-for-tag)
  (define-key-after (lookup-key mu4e-headers-mode-map [menu-bar headers])
    [mark-tag] '("Mark for tag" . mu4e-headers-mark-for-tag) 'mark-pattern)

  ;; Actions to add tags
  ;;--------------------------------------------------
  (add-to-list 'mu4e-view-actions
               '("add/remove tags" . mu4e-action-retag-message) t)

  ;; Tags & Completing read function
  ;;--------------------------------------------------
  (defvar lem-mail-tags
    '("casrac"
      "committee"
      "conferences"
      "edited-volume"
      "ergo"
      "grad-admissions"
      "phil105"
      "phil232"
      "phil871"
      "phil880"
      "phil971"
      "placement"
      "publications"
      "rationality-book"
      "referee-reports"
      "supervision")
    "List of email tags")

  (defun lem-select-mail-tag ()
    (interactive)
    (completing-read "Select Tag (+/-): " lem-mail-tags))

  ;; Quickly add/remove/search tag (named QT**) in header/message view
  ;;--------------------------------------------------
  (defvar lem-mail~quick-tag "QT**"
    "Quick tag.")

  (defun lem-mail-add-del-quick-tag ()
    "Quickly add/del tags."
    (interactive)
    (let* ((msg (mu4e-message-at-point))
           (oldtags (mu4e-message-field msg :tags)))
      (if (member lem-mail~quick-tag oldtags)
          (mu4e-action-retag-message msg (concat "-" lem-mail~quick-tag))
        (mu4e-action-retag-message msg (concat "+" lem-mail~quick-tag)))))

  ;;
  ;; Show tags in the header view
  ;;--------------------------------------------------
  ;;
  (defun lem-mail-header-add-tags-handler (msg field f-v str)
    "Add tags to header view's subject field like: [TAG][TAG] subject..."
    (let* ((val (or str f-v)))
      (if (eq field :subject)
          (let ((tags (mu4e-message-field msg :tags)))
            (if tags
                (setq val (concat
                           (mapconcat (function (lambda (x) (propertize (concat "[" x "]") 'face 'fringe)))
                                      tags "")
                           " "
                           val))
              val))
        val)))

  (cond ((functionp 'mu4e~headers-field-value) ; for mu>=1.5
         (add-to-list 'lem-mail~header-handlers 'lem-mail-header-add-tags-handler))
        ((listp 'mu4e~headers-field-handler-functions) ; for mu<1.5
         (add-to-list 'mu4e~headers-field-handler-functions (lambda (msg field val width)
                                                              "" (lem-mail-header-add-tags-handler msg field val nil))))
        (t nil)))


;;;;; Quick Actions
(with-eval-after-load 'mu4e
  ;; Helpful discussion at
  ;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-05.org
  (defun lem-capture-mail-respond (msg)
    "Capture for message follow-up with schedule & deadline."
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "mr"))

  (defun lem-capture-mail-schedule (msg)
    "Capture for message follow-up with schedule."
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "ms"))

  (defun lem-capture-mail-link (msg)
    "Capture for message read-later"
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "ml"))

  ;; Add custom actions for our capture templates
  (add-to-list 'mu4e-headers-actions
               '("Link" . lem-capture-mail-link) t)
  (add-to-list 'mu4e-view-actions
               '("Link" . lem-capture-mail-link) t)
  (add-to-list 'mu4e-headers-actions
               '("Respond" . lem-capture-mail-respond) t)
  (add-to-list 'mu4e-view-actions
               '("Respond" . lem-capture-mail-respond) t)
  (add-to-list 'mu4e-headers-actions
               '("Schedule" . lem-capture-mail-schedule) t)
  (add-to-list 'mu4e-view-actions
               '("Schedule" . lem-capture-mail-schedule) t))

;;;;; Mail Custom Bookmarks/Searches
(with-eval-after-load 'mu4e
  (setq mu4e-bookmarks '((:name "Inbox"       :query "m:/UNL/inbox or m:/Fastmail/inbox"      :key ?i)
                         (:name "Unread"      :query "flag:unread AND NOT flag:trashed"       :key ?u)
                         (:name "Drafts"      :query "m:/UNL/drafts or m:/Fastmail/drafts"    :key ?d)
                         (:name "Sent Mail"   :query "m:/UNL/sent or m:/Fastmail/sent"        :key ?s)
                         (:name "Trash"       :query "m:/UNL/Trash or m:/Fastmail/Trash"      :key ?T)
                         (:name "-----"       :query "m:/UNL/inbox" :hide-unread t            :key ?-)
                         (:name "Today"       :query "date:today..now"                        :key ?t)
                         (:name "Yesterday"   :query "date:2d..today and not date:today..now" :key ?y)
                         (:name "Last Week"   :query "date:7d..now"                           :key ?w)
                         (:name "Last Month"  :query "date:4w..now"                           :key ?m)
                         (:name "-----"       :query "m:/UNL/inbox" :hide-unread t            :key ?-)
                         (:name "Archive"     :query "m:/UNL/archive or m:/Fastmail/archive"  :key ?a)
                         (:name "Important"   :query "flag:flagged"                           :key ?!)
                         (:name "Attachments" :query "flag:attach"                            :key ?A)
                         (:name "Images"      :query "mime:image/*"                           :key ?I))))




;;;;; Better Icons
;; All-the-icons for marking
;; Use all-the-icons
;;https://github.com/emacsmirror/mu4e-marker-icons
;;https://github.com/djcb/mu/issues/1795

(with-eval-after-load 'mu4e
  ;; Depends on all-the-icons
  (defgroup mu4e-marker-icons nil
    "Display icons for mu4e markers."
    :group 'mu4e-marker-icons)

  (defvar mu4e-marker-icons-marker-alist
    '((mu4e-headers-seen-mark      . mu4e-marker-icons-saved-headers-seen-mark)
      (mu4e-headers-new-mark       . mu4e-marker-icons-saved-headers-new-mark)
      (mu4e-headers-unread-mark    . mu4e-marker-icons-saved-headers-unread-mark)
      (mu4e-headers-signed-mark    . mu4e-marker-icons-saved-headers-signed-mark)
      (mu4e-headers-encrypted-mark . mu4e-marker-icons-saved-headers-encrypted-mark)
      (mu4e-headers-draft-mark     . mu4e-marker-icons-saved-headers-draft-mark)
      (mu4e-headers-attach-mark    . mu4e-marker-icons-saved-headers-attach-mark)
      (mu4e-headers-passed-mark    . mu4e-marker-icons-saved-headers-passed-mark)
      (mu4e-headers-flagged-mark   . mu4e-marker-icons-saved-headers-flagged-mark)
      (mu4e-headers-replied-mark   . mu4e-marker-icons-saved-headers-replied-mark)
      (mu4e-headers-trashed-mark   . mu4e-marker-icons-saved-headers-trashed-mark))
    "An alist of markers used in mu4e.")

  (defun mu4e-marker-icons--store (l)
    "Store mu4e header markers value from L."
    (mapcar (lambda (x) (set (cdr x) (symbol-value (car x)))) l))

  (defun mu4e-marker-icons--restore (l)
    "Restore mu4e header markers value from L."
    (let ((lrev (mapcar (lambda (x) (cons (cdr x) (car x))) l)))
      (mu4e-marker-icons--store lrev)))

  (defun mu4e-marker-icons-enable ()
    "Enable mu4e-marker-icons."
    (mu4e-marker-icons--store mu4e-marker-icons-marker-alist)
    (setq mu4e-use-fancy-chars t)
    (setq mu4e-headers-precise-alignment t)

    (setq mu4e-headers-seen-mark       `("S" . "⦾")
          mu4e-headers-new-mark        `("N" . "⦿")
          mu4e-headers-unread-mark     `("u" . "🖅")
          mu4e-headers-draft-mark      `("D" . "🖉")
          mu4e-headers-attach-mark     `("a" . "📎")
          mu4e-headers-passed-mark     `("P" . "")
          mu4e-headers-flagged-mark    `("F" . "⚑")
          mu4e-headers-replied-mark    `("R" . "↩")
          mu4e-headers-trashed-mark    `("T" . "🗑")
          mu4e-headers-encrypted-mark  `("x" . "🗝")
          mu4e-headers-signed-mark     `("s" . "✍")
          mu4e-headers-list-mark       '("s" . "Ⓛ")
          mu4e-headers-personal-mark   '("p" . "")))

  (defun mu4e-marker-icons-disable ()
    "Disable mu4e-marker-icons."
    (mu4e-marker-icons--restore mu4e-marker-icons-marker-alist))

  (define-minor-mode mu4e-marker-icons-mode
    "Display icons for mu4e markers."
    :require 'mu4e-marker-icons-mode
    :init-value nil
    :global t
    (if mu4e-marker-icons-mode
        (mu4e-marker-icons-enable)
      (mu4e-marker-icons-disable)))
  (mu4e-marker-icons-mode))

;;;;; Better Marking w/SVG Tags)

;;;;;; Default Mark Sign
(with-eval-after-load 'mu4e
  ;; --- Nicer actions display using SVG tags -----------------------------------
  (plist-put (cdr (assq 'refile   mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'trash    mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'untrash  mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'unread   mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'delete   mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'flag     mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'unflag   mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'move     mu4e-marks)) :char "×")
  (plist-put (cdr (assq 'tag      mu4e-marks)) :char "×")


  ;; Don't show refile target; use svg instead
  (setq mu4e-headers-show-target nil))

;;;;;; Add SVG Icon Overlays
(with-eval-after-load 'mu4e
  (defun mu4e-mark-at-point-advice (mark target)
    (interactive)
    (require 'svg-tag-mode)
    (let* ((msg (mu4e-message-at-point))
           (docid (mu4e-message-field msg :docid))
           (obeg  (+ (line-beginning-position) 9))
           (oend  (+ (line-beginning-position) 21))
           (overlay (make-overlay obeg oend)))
      (cond ((eql mark 'refile)
             (overlay-put overlay 'display (svg-tag-make "ARCHIVE" :face 'success :font-family "SF Mono" :inverse t 3 0)))
            ((eql mark 'trash)
             (overlay-put overlay 'display (svg-tag-make "TRASH"   :face 'error   :font-family "SF Mono" :inverse t 5 0)))
            ((eql mark 'untrash)
             (overlay-put overlay 'display (svg-tag-make "UNTRASH" :face 'warning :font-family "SF Mono" :inverse t 3 0)))
            ((eql mark 'delete)
             (overlay-put overlay 'display (svg-tag-make "DELETE"  :face 'error   :font-family "SF Mono" :inverse t 4 0)))
            ((eql mark 'unread)
             (overlay-put overlay 'display (svg-tag-make "UNREAD"  :face 'warning :font-family "SF Mono" :inverse t 4 0)))
            ((eql mark 'flag)
             (overlay-put overlay 'display (svg-tag-make "FLAG"    :face 'warning :font-family "SF Mono" :inverse t 6 0)))
            ((eql mark 'unflag)
             (overlay-put overlay 'display (svg-tag-make "UNFLAG"  :face 'warning :font-family "SF Mono" :inverse t 4 0)))
            ((eql mark 'move)
             (overlay-put overlay 'display (svg-tag-make "MOVE"    :face 'warning :font-family "SF Mono" :inverse t 6 0)))
            ((eql mark 'tag)
             (overlay-put overlay 'display (svg-tag-make "TAG"     :face 'region  :font-family "SF Mono" :inverse nil 7 0)))
            ((eql mark 'unmark)
             (save-excursion
               (remove-overlays (line-beginning-position) (line-end-position)))))))
  (advice-add 'mu4e-mark-at-point :after #'mu4e-mark-at-point-advice))

;;;;; Miscellaneous

;; Go to unread
(defvar lem-mu4e-unread-query
  "flag:unread")
(defun lem-go-to-mail-unread ()
  (interactive)
  (if (member "Email" (tabspaces--list-tabspaces))
      (progn
        (tab-bar-switch-to-tab "Email")
        (mu4e-headers-search lem-mu4e-unread-query))
    (progn
      (tabspaces-switch-or-create-workspace)
      (tab-bar-rename-tab "Email")
      (find-file (concat org-directory "mail.org"))
      (mu4e)
      (mu4e-headers-search lem-mu4e-unread-query))))

;; Go to inbox
(defvar lem-mu4e-inbox-query
  "(maildir:/UNL/INBOX OR maildir:/Fastmail/INBOX)")
(defun lem-go-to-mail-inbox ()
  (interactive)
  (if (member "Email" (tabspaces--list-tabspaces))
      (progn
        (tab-bar-switch-to-tab "Email")
        (mu4e-headers-search lem-mu4e-inbox-query))
    (progn
      (tabspaces-switch-or-create-workspace)
      (tab-bar-rename-tab "Email")
      (find-file (concat org-directory "mail.org"))
      (mu4e)
      (mu4e-headers-search lem-mu4e-inbox-query))))

;;;; Column Faces
;; requires mu 1.8+
(use-package mu4e-column-faces
  :after mu4e
  :config (mu4e-column-faces-mode))

;;;; Using Org & HTML (Org-MSG)
(use-package org-msg
  :after mu4e
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} ':t toc:nil author:nil email:nil \\n:t"
	    org-msg-startup "hidestars inlineimages"
	    org-msg-greeting-fmt nil
	    org-msg-recipient-names nil
	    org-msg-greeting-name-limit 3
	    org-msg-default-alternatives '((new		        . (html))
				                       (reply-to-html	. (html))
				                       (reply-to-text	. (text)))
	    org-msg-convert-citation t)

  (defun lem-org-msg-hooks ()
    "Hooks for org-msg"
    (progn
      (auto-fill-mode -1)
      (hl-line-mode 1)
      (lem-writing-mode-scroll-settings)
      ;; FIXME: Try remove auto-save hook *locally* to avoid multiple saved drafts
      (remove-hook 'auto-save-hook #'lem-full-auto-save t)))
  (add-hook 'org-msg-edit-mode-hook #'lem-org-msg-hooks)

  ;; Org HTML Styling
  (defconst cpm-org-msg-style
    (let* ((font-family '(font-family . "font-family: \"Helvetica\", \"Arial\", sans-serif"))
	       (font-size '(font-size . "12pt"))
	       (font `(,font-family ,font-size))
	       (line-height '(line-height . "1.5"))
	       (bold '(font-weight . "bold"))
	       (theme-color "#0071c5")
	       (color `(color . ,theme-color))
	       (table `(,@font (margin-top . "0px")))
	       (ftl-number `(,@font ,color ,bold (text-align . "left")))
	       (inline-modes '(asl c c++ conf cpp csv diff ditaa emacs-lisp
			                   fundamental ini json makefile man org plantuml
			                   python sh xml))
	       (inline-src `((color . ,(face-foreground 'default))
		                 (background-color . ,(face-background 'default))))
	       (code-src
	        (mapcar (lambda (mode)
		              `(code ,(intern (concat "src src-" (symbol-name mode)))
			                 ,inline-src))
		            inline-modes))
	       (base-quote '((padding-left . "5px") (margin-left . "10px")
		                 (margin-top . "10px") (margin-bottom . "0")
		                 (font-style . "italic") (background . "#f9f9f9")))
	       (quote-palette '("#324e72" "#6a3a4c" "#7a4900" "#ff34ff"
			                "#ff4a46" "#008941" "#006fa6" "#a30059"
			                "#ffdbe5" "#000000" "#0000a6" "#63ffac"))
	       (quotes
	        (mapcar (lambda (x)
		              (let ((c (nth x quote-palette)))
		                `(blockquote ,(intern (format "quote%d" (1+ x)))
				                     (,@base-quote
				                      (color . ,c)
				                      (border-left . ,(concat "3px solid "
						                                      (org-msg-lighten c)))))))
		            (number-sequence 0 (1- (length quote-palette))))))
      `((del nil (,@font (color . "grey") (border-left . "none")
	                     (text-decoration . "line-through") (margin-bottom . "0px")
	                     (margin-top . "10px") (line-height . "11pt")))
        (a nil (,color))
        (a reply-header ((color . "black") (text-decoration . "none")))
        (div reply-header ((padding . "3.0pt 0in 0in 0in")
		                   (border-top . "solid #e1e1e1 1.0pt")
		                   (margin-bottom . "20px")))
        (span underline ((text-decoration . "underline")))
        (li nil (,@font ,line-height (margin-bottom . "0px")
	                    (margin-top . "2px")))
        (nil org-ul ((list-style-type . "disc")))
        (nil org-ol (,@font ,line-height (margin-bottom . "0px")
		                    (margin-top . "0px") (margin-left . "30px")
		                    (padding-top . "0px") (padding-left . "5px")))
        (nil signature (,@font (margin-bottom . "20px")))
        (blockquote quote0 ,(append base-quote '((border-left . "3px solid #ccc"))))
        ,@quotes
        (code nil (,font-size (font-family . "monospace") (background . "#f9f9f9")))
        ,@code-src
        (nil linenr ((padding-right . "1em")
		             (color . "black")
		             (background-color . "#aaaaaa")))
        (pre nil ((line-height . "12pt")
	              ,@inline-src
	              (margin . "0px")
	              (font-size . "9pt")
	              (font-family . "monospace")))
        (div org-src-container ((margin-top . "10px")))
        (nil figure-number ,ftl-number)
        (nil table-number)
        (caption nil ((text-align . "left")
		              (background . ,theme-color)
		              (color . "white")
		              ,bold))
        (nil t-above ((caption-side . "top")))
        (nil t-bottom ((caption-side . "bottom")))
        (nil listing-number ,ftl-number)
        (nil figure ,ftl-number)
        (nil org-src-name ,ftl-number)

        (table nil (,@table ,line-height (border-collapse . "collapse")))
        (th nil ((border . "1px solid white")
	             (background-color . ,theme-color)
	             (color . "white")
	             (padding-left . "10px") (padding-right . "10px")))
        (td nil (,@table (padding-left . "10px") (padding-right . "10px")
		                 (background-color . "#f9f9f9") (border . "1px solid white")))
        (td org-left ((text-align . "left")))
        (td org-right ((text-align . "right")))
        (td org-center ((text-align . "center")))

        (div outline-text-4 ((margin-left . "15px")))
        (div outline-4 ((margin-left . "10px")))
        (h4 nil ((margin-bottom . "0px") (font-size . "12pt")
                 (text-decoration . "underline") ,font-family))
        (h3 nil ((margin-bottom . "0px") ,color (font-size . "14pt")
	             ,font-family))
        (h2 nil ((margin-top . "20px") (margin-bottom . "20px")
                 ,color (font-size . "14pt") ,font-family))
        (h1 nil ((margin-top . "20px")
	             (margin-bottom . "20px") ,color (font-size . "16pt")
	             ,font-family))
        (p nil ((text-decoration . "none") (margin-bottom . "0px")
	            (margin-top . "10px") (line-height . "1.35") (padding-bottom . "5px") ,font-size
	            ,font-family))
        (div nil (,@font (line-height . "1.25"))))))
  (setq org-msg-enforce-css cpm-org-msg-style)
  (org-msg-mode))



;;; Provide
(provide 'cpm-setup-email)
;;; setup-email.el ends here
