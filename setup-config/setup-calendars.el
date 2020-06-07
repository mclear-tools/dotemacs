;;; Calendars
(setq diary-file (concat cpm-local-dir "diary-files/diary"))
(setq diary-location (concat cpm-local-dir "diary-files/"))
(setq org-agenda-include-diary t)
(setq diary-display-function 'diary-fancy-display)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)

;; The following doesn't use org-mac-ical but if I were to ever use that this seems relevant
;; https://emacs.stackexchange.com/questions/49117/org-mac-ical-not-working

;; calendars you want to download
;; each item links to a remote iCal calendar
(when (file-directory-p (concat cpm-local-dir "diary-files/"))
  (setq calendars
        `(("work" . ,cal-work)
          ("gcal" . ,cal-gcal)
          ("family" . ,cal-family)
          )))

(defun cpm--getcal (url file)
  "Download ics file and add it to file"
  (let ((tmpfile (url-file-local-copy url)))
    (icalendar-import-file tmpfile file)
    (kill-buffer (car (last (split-string tmpfile "/"))))))

(defun cpm/getcals ()
  "Load a set of ics calendars into emacs diary files"
  (interactive)
  (mapc #'(lambda (x)
            (let ((file (concat diary-location (car x)))
                  (url (cdr x)))
              (message (concat "Loading " url " into " file))
              (find-file file)
              ;; (flush-lines "^[& ]") ;; if you import ical as non marking
              (erase-buffer) ;; to avoid duplicating events
              (cpm--getcal url file)
              ))
        calendars)
  ;; send everything to a diary file
  (shell-command-to-string "cat work family gcal > diary"))

(defun cpm/refresh-ical ()
  "get ical appointments via batch emacs call"
  (interactive)
  (shell-command-to-string "emacs-get-cals.sh")
  (cpm/org-agenda-refresh))

;; add exchange support
(use-package excorporate
  :after org
  :disabled t
  :config
  ;; configure excorporate
  ;; allow opening the exchange calendar with 'e' from calendar
  (evil-define-key 'motion calendar-mode-map "e" #'exco-calendar-show-day)

  (setq-default
   ;; configure email address and office 365 exchange server adddress for exchange web services
   excorporate-configuration
   (`(,unl-email . ,unl-exchange))
   ;; integrate emacs diary entries into org agenda
   org-agenda-include-diary t
   )
  ;; activate excorporate and request user/password to start connection
  (excorporate)
  ;; enable the diary integration (i.e. write exchange calendar to emacs diary file -> ~/.emacs.d/diary must exist)
  (excorporate-diary-enable)
  (defun cpm/exco-agenda-update-diary ()
    "call excorporate to update the diary for today"
    (exco-diary-diary-advice (calendar-current-date) (calendar-current-date) #'message "diary updated")
    )

  ;; update the diary every time the org agenda is refreshed
  (add-hook 'org-agenda-cleanup-fancy-diary-hook 'cpm/exco-agenda-update-diary))

(provide 'setup-calendars)
