;;; helm-flyspell.el --- Helm extension for correcting words with flyspell  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Andrzej Pronobis <a.pronobis@gmail.com>

;; Author: Andrzej Pronobis
;; URL: https://github.com/pronobis/helm-flyspell
;; Package-Version: 20170210.1901
;; Keywords: convenience
;; Package-Requires: ((helm "1.6.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; To use, just put your cursor on or after the misspelled word and
;; run helm-flyspell-correct. You can of course bind it to a key as
;; well by adding this to your `~/.emacs` file:
;; (define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)
;;
;; When invoked, it will show the list of corrections suggested by
;; Flyspell and options to save the word in your personal dictionary
;; or accept it in the buffer or the session. If a pattern is typed,
;; it will be used to filter the corrections. It can also be directly
;; saved to the dictionary, even if it is different from the initial
;; word. The new typed word will also replace the word typed in the
;; buffer.

;;; Code:

;; Requires
(require 'helm)
(require 'flyspell)


(defun helm-flyspell--always-match (_candidate)
  "Return true for any CANDIDATE."
  t)

(defun helm-flyspell--option-candidates (word)
  "Return a set of options for the given WORD."
  (let ((opts (list (cons (format "Save \"%s\"" word) (cons 'save word))
                    (cons (format "Accept (session) \"%s\"" word) (cons 'session word))
                    (cons (format "Accept (buffer) \"%s\"" word) (cons 'buffer word)))))
    (unless (string= helm-pattern "")
      (setq opts (append opts (list (cons (format "Save \"%s\"" helm-pattern) (cons 'save helm-pattern))
                                    (cons (format "Accept (session) \"%s\"" helm-pattern) (cons 'session helm-pattern))
                                    (cons (format "Accept (buffer) \"%s\"" helm-pattern) (cons 'buffer helm-pattern))))))
    opts))

(defun helm-flyspell (candidates word)
  "Run helm for the given CANDIDATES given by flyspell for the WORD.
Return a selected word to use as a replacement or
a tuple of (command, word) to be used by flyspell-do-correct."
  (helm :sources (list (helm-build-sync-source (format "Suggestions for \"%s\" in dictionary \"%s\""
                                                       word (or ispell-local-dictionary
                                                                ispell-dictionary
                                                                "Default"))
                         :candidates candidates
                         :action 'identity
                         :candidate-number-limit 9999
                         :fuzzy-match t)
                       (helm-build-sync-source "Options"
                         :candidates (lambda ()
                                       (helm-flyspell--option-candidates word))
                         :action 'identity
                         :candidate-number-limit 9999
                         :match 'helm-flyspell--always-match
                         :volatile t))
        :buffer "*Helm Flyspell*"
        :prompt "Correction: "))

;;;###autoload
(defun helm-flyspell-correct ()
  "Use helm for flyspell correction.
Adapted from `flyspell-correct-word-before-point'."
  (interactive)
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (let ((cursor-location (point))
        (word (flyspell-get-word)))
    (if (consp word)
        (let ((start (car (cdr word)))
              (end (car (cdr (cdr word))))
              (word (car word))
              poss ispell-filter)
          ;; now check spelling of word.
          (ispell-send-string "%\n")	;put in verbose mode
          (ispell-send-string (concat "^" word "\n"))
          ;; wait until ispell has processed word
          (while (progn
                   (accept-process-output ispell-process)
                   (not (string= "" (car ispell-filter)))))
          ;; Remove leading empty element
          (setq ispell-filter (cdr ispell-filter))
          ;; ispell process should return something after word is sent.
          ;; Tag word as valid (i.e., skip) otherwise
          (or ispell-filter
              (setq ispell-filter '(*)))
          (if (consp ispell-filter)
              (setq poss (ispell-parse-output (car ispell-filter))))
          (cond
           ((or (eq poss t) (stringp poss))
            ;; don't correct word
            (message "%s is correct" (funcall ispell-format-word-function word))
            t)
           ((null poss)
            ;; ispell error
            (error "Ispell: error in Ispell process"))
           (t
            ;; The word is incorrect, we have to propose a replacement.
            (let ((res (helm-flyspell (nth 2 poss) word)))
              (cond ((stringp res)
                     (flyspell-do-correct res poss word cursor-location start end cursor-location))
                    (t
                     (let ((cmd (car res))
                           (wrd (cdr res)))
                       (if (string= wrd word)
                           (flyspell-do-correct cmd poss wrd cursor-location start end cursor-location)
                         (progn
                           (flyspell-do-correct cmd poss wrd cursor-location start end cursor-location)
                           (flyspell-do-correct wrd poss word cursor-location start end cursor-location)))))))))
          (ispell-pdict-save t)))))

(provide 'helm-flyspell)
;;; helm-flyspell.el ends here
