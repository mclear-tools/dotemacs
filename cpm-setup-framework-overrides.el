;;; cpm-setup-framework-overrides.el --- Personal overrides for Lambda-Emacs framework -*- lexical-binding: t -*-

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

;;; Commentary:

;; Personal configurations that override or extend the base Lambda-Emacs
;; framework. These were moved from the main framework files to keep
;; personal customizations separate from the core framework.

;;; Code:

;;;; Git Commit Configuration
;; Personal git commit settings with auto-fill and meow integration
(use-package git-commit
  :after magit
  :hook (git-commit-mode . cpm/git-commit-auto-fill-everywhere)
  :custom (git-commit-summary-max-length 50)
  :preface
  (defun cpm/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 80 characters."
    (setq fill-column 80)
    (setq-local comment-auto-fill-only-comments nil))
  :config
  (with-eval-after-load 'meow
    (add-hook 'git-commit-mode-hook
              (lambda ()
                (meow-insert-mode)))))

(provide 'cpm-setup-framework-overrides)
;;; cpm-setup-framework-overrides.el ends here