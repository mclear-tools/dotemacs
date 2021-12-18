;;; setup-emacs-git-version.el ---

;; Copyright © 2012 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Modified: Colin Mclear
;; Keywords: emacs,
;; Created: 2012-09-21;
;; Last changed: 12-03-2021 14:47:57
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;


;;; Code:

(defconst emacs-version-git-commit "32d12840e6"
  "String giving the git sha1 from which this Emacs was built.

SHA1 of git commit that was used to compile this version of
emacs. The source used to compile emacs are taken from Savannah's
git repository at `http://git.savannah.gnu.org/r/emacs.git' or
git://git.savannah.gnu.org/emacs.git

See both `http://emacswiki.org/emacs/EmacsFromGit' and
`http://savannah.gnu.org/projects/emacs' for further
information.")

(provide 'setup-emacs-git-version)

;; setup-emacs-git-version.el ends here
