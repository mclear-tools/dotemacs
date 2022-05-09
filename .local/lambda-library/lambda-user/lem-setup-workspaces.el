;;; lem-setup-workspaces.el --- summary -*- lexical-binding: t -*-

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
;; Workspaces leveraging tab-bar and project.el. A "workspace" is just a tab
;; with an isolated set of buffers (see consult function). 𝛌-Emacs sets things
;; up so that these workspaces are nicely displayed out of the way in the echo
;; area rather than as visible tabs in the header tab-line at the top of the
;; frame.


;;; Code:



;;; Provide
(provide 'lem-setup-workspaces)
