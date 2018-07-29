;;; tramp-term.el --- Automatic setup of directory tracking in ssh sessions.

;; Copyright (C) 2014 Randy Morris

;; Author: Randy Morris <randy.morris@archlinux.us>
;; Version: 0.3
;; Package-Version: 20180223.1527
;; Keywords: tramp, ssh
;; URL: https://github.com/randymorris/tramp-term.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides a way to initiate ssh sessions within emacs
;; and have directory tracking automatically set up for editing files
;; with TRAMP.  No configuration is required on the remote host.
;;
;; Currently this is designed to work with a bash shell on the remote
;; host, however with a little work this could be changed to allow
;; different shells via a per-host configuration.
;;
;; The ideas presented here and various bits of code within were
;; lifted directly from http://www.emacswiki.org/emacs-se/AnsiTermHints.
;; Consider the editors of that page as contributors to this package.

;;; Code:

(require 'term)
(require 'tramp)

(defvar tramp-term-after-initialized-hook nil
  "Hook called after tramp has been initialized on the remote
  host.  Hooks should expect a single arg which contains the
  hostname used to connect to the remote machine.")

;;;###autoload
(defun tramp-term ()
  "Create an ansi-term running ssh session and automatically
enable tramp integration in that terminal."
  (interactive)
  (let* ((host (tramp-term--select-host))
         (hostname (car (last host)))
         (prompt-bound nil))
    (if (or (> (length host) 2)
            (eql (length hostname) 0))
        (message "Invalid host string")
      (unless (eql (catch 'tramp-term--abort (tramp-term--do-ssh-login host)) 'tramp-term--abort)
        (tramp-term--initialize hostname)
        (run-hook-with-args 'tramp-term-after-initialized-hook hostname)
        (message "tramp-term initialized")))))

;; I imagine TRAMP has utility functions that would replace most of
;; this.  Needs investigation.
(defun tramp-term--do-ssh-login (host)
  "Perform the ssh login dance."
  (let* ((user "")
         (hostname (car (last host))))
    (when (= (length host) 2)
      (setq user (format "%s@" (car host))))
    (tramp-term--create-term hostname "ssh" (format "%s%s" user hostname)))
  (save-excursion
    (let ((bound 0))
      (while (not (tramp-term--find-shell-prompt bound))
        (let ((yesno-prompt (tramp-term--find-yesno-prompt bound))
              (passwd-prompt (tramp-term--find-passwd-prompt bound))
              (service-unknown (tramp-term--find-service-unknown bound)))
          (cond (yesno-prompt
                 (tramp-term--confirm)
                 (setq bound (1+ yesno-prompt)))
                (passwd-prompt
                 (tramp-term--handle-passwd-prompt)
                 (setq bound (1+ passwd-prompt)))
                (service-unknown (throw 'tramp-term--abort 'tramp-term--abort))
                (t (sleep-for 0.1))))))))

(defun tramp-term--find-shell-prompt (bound)
  (re-search-backward tramp-shell-prompt-pattern bound t))

(defun tramp-term--find-yesno-prompt (bound)
  (re-search-backward tramp-yesno-prompt-regexp bound t))

(defun tramp-term--find-passwd-prompt (bound)
  (re-search-backward tramp-password-prompt-regexp bound t))

(defun tramp-term--find-service-unknown (bound)
  (re-search-backward "Name or service not known" bound t))

(defun tramp-term--handle-passwd-prompt ()
  "Reads a password from the user and sends it to the server."
  (term-send-raw-string
   (concat (read-passwd "Password: ") (kbd "RET"))))

(defun tramp-term--confirm ()
  "Prompts the user to continue, aborts if they decline."
  (if (yes-or-no-p "Continue? ")
      (term-send-raw-string (concat "yes" (kbd "RET")))
    (term-send-raw-string (concat "no" (kbd "RET")))
    (throw 'tramp-term--abort 'tramp-term--abort)))

(defun tramp-term--initialize (hostname)
  "Send bash commands to set up tramp integration."
  (term-send-raw-string (format "
function set-eterm-dir {
    echo -e \"\\033AnSiTu\" \"ssh:$USER\"
    echo -e \"\\033AnSiTc\" \"$PWD\"
    echo -e \"\\033AnSiTh\" \"%s\"
    history -a
}
PROMPT_COMMAND=\"${PROMPT_COMMAND:+$PROMPT_COMMAND ;} set-eterm-dir\"
clear
" hostname)))

(defun tramp-term--select-host ()
  "Return a host from a list of hosts."
  (let ((crm-separator "@"))
    (completing-read-multiple "[user@]host: " (tramp-term--parse-hosts "~/.ssh/config"))))

(defun tramp-term--parse-hosts (ssh-config)
  "Parse any host directives from SSH-CONFIG file and return them
as a list of strings"
  (mapcar 'cadr (delete nil (tramp-parse-sconfig ssh-config))))

(defun tramp-term--create-term (new-buffer-name cmd &rest switches)
  "Create an ansi-term running an arbitrary command, including
extra parameters."
  (let ((new-buffer-name (generate-new-buffer-name (format "*%s*" new-buffer-name))))
    (with-current-buffer (make-term new-buffer-name cmd nil (car switches))
      (rename-buffer new-buffer-name)   ; Undo the extra "*"s that
                                        ; make-term insists on adding
      (term-mode)
      (term-char-mode)
      (term-set-escape-char ?\C-x))
    (switch-to-buffer new-buffer-name)))

(provide 'tramp-term)
;;; tramp-term.el ends here
