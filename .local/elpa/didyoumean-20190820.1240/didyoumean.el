;;; didyoumean.el --- Did you mean to open another file? -*- lexical-binding: t; -*-

;; Authors: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; URL: https://gitlab.com/kisaragi-hiu/didyoumean.el
;; Package-Version: 20190820.1240
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; didyoumean.el is free software: you can redistribute it and/or modify
;; it under the terms of the MIT License.

;;; Commentary:
;; Ask for the right file to open.

;; FIXME: `didyoumean' too often breaks code that use `find-file' in Lisp

;;; Code:

(require 'cl-lib)

(defgroup didyoumean nil
  "Did you mean to open another file?"
  :group 'convenience
  :prefix "didyoumean-")

(defcustom didyoumean-ignored-extensions '(".elc" "~" ".bak")
  "Do not suggest files that have these extensions.

More accurately, these are suffixes, to allow backup files to be
ignored easily."
  :group 'didyoumean
  :type '(repeat string))

(defcustom didyoumean-custom-ignore-function nil
  "Do not suggest files that make this function return t."
  :group 'didyoumean
  :type '(choice (const :tag "None" nil)
                 function))

(defvar didyoumean--history nil "History for `didyoumean' prompts.")

(defun didyoumean--matching-files (file)
  "Return files that seems to be similar in name to FILE (excluding itself)."
  (when (stringp file)
    (cl-remove-if
     (lambda (x) (or (not (string-prefix-p file x))
                     (equal file x)
                     ;; don't suggest extensions in this list
                     (and didyoumean-ignored-extensions
                          ;; remove all nils, if the list is not empty
                          ;; then an extension has matched, so this
                          ;; `x' will be ignored
                          (cl-remove nil (mapcar
                                          (lambda (suf)
                                            (string-suffix-p suf x :ignore-case))
                                          didyoumean-ignored-extensions)))
                     ;; don't suggest anything that d-c-i-f says so
                     (and (functionp didyoumean-custom-ignore-function)
                          (funcall didyoumean-custom-ignore-function file))))
     (directory-files "." (file-name-absolute-p file)))))

;;;###autoload
(defun didyoumean ()
  "Prompt for files similar to the current file if they exist."
  (interactive)
  (let* ((matching-files (didyoumean--matching-files buffer-file-name))
         (comp-read-func
          (cond ((or (bound-and-true-p ivy-mode)
                     (bound-and-true-p helm-mode))
                 ;; `completing-read' would be advised in this case
                 #'completing-read)
                ;; the list needs to be visible up front
                (t (require 'ido)
                   #'ido-completing-read)))
         (correct-file
          (when matching-files
            (funcall comp-read-func
                     "Did you mean: "
                     `(,buffer-file-name ,@matching-files)
                     nil nil nil
                     'didyoumean--history)))
         (this-file (current-buffer)))
    (when (and correct-file
               (not (equal buffer-file-name correct-file)))
      (message "%s" correct-file)
      (message "%s" this-file)
      (find-file correct-file)
      ;; killing the buffer during find-file-hook, sure...?
      (kill-buffer this-file))))

;;;###autoload
(add-hook 'find-file-hook #'didyoumean)

(provide 'didyoumean)
;;; didyoumean.el ends here
