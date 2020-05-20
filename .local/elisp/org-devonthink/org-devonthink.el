;;;; org-devonthink.el - Support for links to dtp messages by their UUID
;; version 1.1, by John Wiegley <johnw@gnu.org>

(require 'org)
(org-add-link-type "x-devonthink-item" 'org-dtp-open)

;;;###autoload
(defun org-dtp-open (record-location)
  "Visit the dtp message with the given Message-ID."
  (shell-command (concat "open x-devonthink-item:" record-location)))

;;;###autoload
(defun org-get-dtp-link (&optional given-name)
  (interactive)
  (let ((name (or given-name
                  (do-applescript (format "
    tell application \"DEVONthink Pro\"
        get name of content record
    end tell"))))
    (location (do-applescript (format "
    tell application \"DEVONthink Pro\"
        get uuid of content record
    end tell"))))
    (org-make-link-string
     (concat "x-devonthink-item://" location) name)))

;;;###autoload
(defun org-insert-dtp-link ()
  (interactive)
  (let (name)
    (when (region-active-p)
      (setq name (buffer-substring-no-properties (region-beginning)
                                                 (region-end)))
      (delete-region (region-beginning) (region-end)))
    (insert (org-get-dtp-link name))))

;;;###autoload
(defun org-dtp-store-link ()
  "Store a link to an dtp e-mail message by Message-ID."
  (let ((link-name
     (with-temp-buffer
       (clipboard-yank)
       (buffer-string))))
    (org-store-link-props
     :type "x-devonthink-item"
     :link (cons (concat "x-devonthink-item://" link-name)
         (concat "x-devonthink-item://" link-name))
     :description (file-name-nondirectory link-name))))


;; org-devonthink.el ends here
