;;; elisp-demos.el --- Elisp API Demos               -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/elisp-demos
;; Keywords: lisp, docs
;; Version: 2020.03.30
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Elisp API Demos

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subr-x)

(defconst elisp-demos--load-dir (file-name-directory
                                 (or load-file-name buffer-file-name)))

(defconst elisp-demos--elisp-demos.org (expand-file-name
                                        "elisp-demos.org"
                                        elisp-demos--load-dir))

(defun elisp-demos--search (symbol)
  (with-temp-buffer
    (insert-file-contents elisp-demos--elisp-demos.org)
    (goto-char (point-min))
    (when (re-search-forward
           (format "^\\* %s$" (regexp-quote (symbol-name symbol)))
           nil t)
      (let (beg end)
        (forward-line 1)
        (setq beg (point))
        (if (re-search-forward "^\\*" nil t)
            (setq end (line-beginning-position))
          (setq end (point-max)))
        (string-trim (buffer-substring-no-properties beg end))))))

(defun elisp-demos--syntax-highlight (orgsrc)
  (with-temp-buffer
    (insert orgsrc)
    (delay-mode-hooks (org-mode))
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (buffer-string)))

(defun elisp-demos--symbols ()
  (with-temp-buffer
    (insert-file-contents elisp-demos--elisp-demos.org)
    (goto-char (point-min))
    (let (symbols)
      (while (re-search-forward "^\\* \\(.+\\)$" nil t)
        (push (intern (match-string-no-properties 1)) symbols))
      (nreverse symbols))))

(declare-function org-show-entry "org" ())
(declare-function org-insert-heading "org" (&optional arg invisible-ok top))

(defun elisp-demos-find-demo (symbol)
  "Find the demo of the SYMBOL."
  (interactive
   (let* ((sym-here (symbol-at-point))
          (symbols (elisp-demos--symbols))
          (default-val (and sym-here
                            (memq sym-here symbols)
                            (symbol-name sym-here)))
          (prompt (if default-val
                      (format "Find demo (default: %s): " default-val)
                    "Find demo: ")))
     (list (read (completing-read prompt
                                  (mapcar #'symbol-name symbols)
                                  nil t nil nil default-val)))))
  (cl-assert symbol)
  (find-file elisp-demos--elisp-demos.org)
  (goto-char (point-min))
  (and (re-search-forward
        (format "^\\* %s$" (regexp-quote (symbol-name symbol))))
       (goto-char (line-beginning-position))
       (org-show-entry))
  t)

;; Borrowed from `helpful--read-symbol'
(defun elisp-demos--read-symbol (prompt predicate)
  (let* ((sym-here (symbol-at-point))
         (default-val
           (when (funcall predicate sym-here)
             (symbol-name sym-here))))
    (when default-val
      (setq prompt
            (replace-regexp-in-string
             (rx ": " eos)
             (format " (default: %s): " default-val)
             prompt)))
    (intern (completing-read prompt obarray
                             predicate t nil nil
                             default-val))))

(defun elisp-demos-add-demo (symbol)
  "Add demo for SYMBOL."
  (interactive
   (list (elisp-demos--read-symbol "Add demo: "
                                   (lambda (sym)
                                     (or (functionp sym)
                                         (special-form-p sym)
                                         (macrop sym))))))
  ;; Try to reuse existing window
  (let* ((buffer (get-file-buffer elisp-demos--elisp-demos\.org))
         (window (and buffer (get-buffer-window buffer))))
    (if window
        (select-window window)
      (find-file elisp-demos--elisp-demos\.org)))
  (goto-char (point-min))
  (or
   (catch 'found
     (while (re-search-forward "^\\* \\(.+\\)$" nil t)
       (cond ((string= (match-string-no-properties 1) (symbol-name symbol))
              (goto-char (line-beginning-position))
              (user-error "%s already exists" symbol))
             ((string< (symbol-name symbol) (match-string-no-properties 1))
              (goto-char (line-beginning-position))
              (throw 'found t)))))
   (goto-char (point-max)))
  (org-insert-heading)
  (insert (symbol-name symbol) "\n"
          "\n"
          "#+BEGIN_SRC elisp\n"
          "\n"
          "#+END_SRC")
  (search-backward "\n#+END_SRC"))

;;; * C-h f (`describe-function')

(defun elisp-demos-help-find-demo-at-point ()
  "Find the demo at point."
  (interactive)
  (let ((offset (- (point) (get-text-property (point) 'start))))
    (and (elisp-demos-find-demo (get-text-property (point) 'symbol))
         ;; Skip heading and an empty line
         (forward-line 2)
         (forward-char offset))))

(defvar elisp-demos-help-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elisp-demos-help-find-demo-at-point)
    map))

;;;###autoload
(defun elisp-demos-advice-describe-function-1 (function)
  (let ((src (and (symbolp function)
                  (elisp-demos--search function)))
        (buf (get-buffer "*Help*")))
    (when (and src buf)
      (with-current-buffer buf
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            ;; Ensure two final newlines
            (if (not (eq (char-before) ?\n))
                (insert "\n\n")
              (if (not (eq (char-before (1- (point))) ?\n))
                  (insert "\n")))
            (insert
             (propertize (elisp-demos--syntax-highlight src)
                         'start (point)
                         'symbol function
                         'keymap elisp-demos-help-keymap)
             "\n")
            (unless (eobp) (insert "\n"))))))))

;;; * helpful.el - https://github.com/Wilfred/helpful

(defvar helpful--sym)
(declare-function helpful--heading "helpful")

;;;###autoload
(defun elisp-demos-advice-helpful-update ()
  (let ((src (and (symbolp helpful--sym)
                  (elisp-demos--search helpful--sym))))
    (when src
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^References$")
          (goto-char (line-beginning-position))
          (let ((inhibit-read-only t))
            (insert
             (helpful--heading "Demos")
             (propertize (elisp-demos--syntax-highlight src)
                         'start (point)
                         'symbol helpful--sym
                         'keymap elisp-demos-help-keymap)
             "\n\n")))))))

;;; * JSON

(declare-function json-encode-string "json" (string))

(defun elisp-demos--export-json-file (json-file)
  "Export all demos as json to JSON-FILE."
  (require 'json)
  (with-temp-buffer
    (insert-file-contents elisp-demos--elisp-demos.org)
    (goto-char (point-min))
    (let ((output-buffer (generate-new-buffer " *elisp-demos-json*"))
          title body beg end)
      (while (re-search-forward "^\\* \\(.+\\)$" nil t)
        (setq title (match-string-no-properties 1))
        (setq beg (save-excursion
                    (forward-line 1)
                    (line-beginning-position)))
        (setq end (save-excursion
                    (if (re-search-forward "^\\* " nil t)
                        (line-beginning-position)
                      (point-max))))
        (setq body (buffer-substring-no-properties beg end))
        (setq title (string-trim title))
        (setq body (string-trim body))
        (with-current-buffer output-buffer
          (insert
           (json-encode-string title) ": " (json-encode-string body) ",\n")))
      (with-current-buffer output-buffer
        (delete-char -2)
        (goto-char (point-min)) (insert "{\n")
        (goto-char (point-max)) (insert "}\n")
        (write-region (point-min) (point-max) json-file))
      (kill-buffer output-buffer))))

(provide 'elisp-demos)
;;; elisp-demos.el ends here
