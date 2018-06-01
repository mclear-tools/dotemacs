This is a major mode for Debian changelog files.  The main features
are:

 - fontification (varies with upload urgency, etc).
 - create a entry for a new version (guessing the version number).
 - finalize a version with proper timestamp and syntax.
 - add an entry from another file in the source package.
 - interface with `debian-bug' to fetch list of bugs from the web,
   read a bug report via browse-url or as email, close a bug with
   thanks.
 - closed bugs are fontified and clickable to view them via browse-url.

The mode is entered automatically when editing a debian/changelog file.
See the menus "Bugs" and "Changelog" for commands or do `C-h m' to get
the list of keybindings.

From other files in unpacked sources, do `M-x debian-changelog-add-entry'
to add an entry for that file in the changelog file.

History

V1.00 30aug00  Peter S Galbraith <psg@debian.org>
 -  Prior version had no changelogs; starting one now.
    This is the potato version plus extensions by Chris Waters (easymenu;
    better menus, font-lock support).
V1.01 30aug00  Peter S Galbraith <psg@debian.org>
 - debian-changelog-finalise-last-version: Use XEmacs' (user-mail-address)
   function if variable user-mail-address is undefined.
   Thanks to Robert Bihlmeyer <robbe@orcus.priv.at>, closes Bug#61524
 - debian-changelog-finalise-last-version: Takes account of some env vars
   Thanks to Rafael Laboissiere <rafael@icp.inpg.fr>, closes Bug#61226
 - debian-changelog-close-bug:  new command.
V1.02 23Feb01  Peter S Galbraith <psg@debian.org>
 - Added `debian-changelog-suggest-version', a mechanisn for guessing
   what the new version number should be.
   Closes half of Bug#85412
V1.03 23Feb01  Peter S Galbraith <psg@debian.org>
 - Fixed `fill-paragraph' by tweaks to paragraph-start and
   paragraph-separate variables.
   Closes second half of Bug#85412
V1.04 23Feb01  Peter S Galbraith <psg@debian.org>
 - Added `debian-changelog-web-bugs' `debian-changelog-web-packages'
   `debian-changelog-web-package'
V1.05 23Feb01  Peter S Galbraith <psg@debian.org>
 - made `debian-changelog-suggest-package-name' more picky about finding
   an acceptable name.
V1.06 28Feb01  Peter S Galbraith <psg@debian.org>
 - Create customizable variables debian-changelog-full-name and
   debian-changelog-mailing-address.
 - Make debian-changelog-finalise-last-version use them.
V1.07 28Feb01  Peter S Galbraith <psg@debian.org>
 - debian-changelog-suggest-version: Handle epochs!
   closes: Bug#87964: dpkg-dev-el: does wrong things with epochs
V1.08 07Mar01  Peter S Galbraith <psg@debian.org>
   debian-changelog-suggest-version: Handle package names with hyphens!
   closes: #88589 and #88245
V1.09 09Mar01  Peter S Galbraith <psg@debian.org>
   debian-changelog-suggest-version: better regexps for version numbers
   Created debian-changelog-increment-version
V1.10 10Mar01  Peter S Galbraith <psg@debian.org>
   tweaks docs for debian-changelog-mode function concerning
   add-log-mailing-address (now obsolete).
V1.11 24Apr01  Peter S Galbraith <psg@debian.org>
   Add stuff to try to trim out obsolete "Local Variables:" block from
   changelog files.
V1.12 24Apr01  Peter S Galbraith <psg@debian.org>
   Modify font-lock code. closes: #93243
V1.13 27Apr01  Peter S Galbraith <psg@debian.org>
   Move code concerning local variables near beginning of file such that
   `hack-local-variables' doesn't complain.
V1.14 30Apr01  Peter S Galbraith <psg@debian.org>
   Add `critical' bug severity (see http://bugs.debian.org/94475)
V1.15 30Apr01  Peter S Galbraith <psg@debian.org>
   Tweak font-locking bug number regexp to match dpkg-parsechangelog 1.9.1
V1.16 30Apr01  Peter S Galbraith <psg@debian.org>
   Added debian-changelog-web-bug (will bound to a mouse button later)
V1.17 30Apr01  Peter S Galbraith <psg@debian.org>
   debian-changelog-increment-version: Handle 3.5.4.0 case (single digits)
   closes: #95831
V1.18 30Apr01  Peter S Galbraith <psg@debian.org>
   Add mouse interface to web-bug (with green highlight).
V1.19 01May01  Peter S Galbraith <psg@debian.org>
   Add imenu support as `History'.  Bug: The history menu is empty when
   point is on the (mouse-highlighted) bug number (using emacs-20.7).
V1.20 02May01  Peter S Galbraith <psg@debian.org>
   Leave `mode: debian-changelog-mode' alone for native packages.
V1.21 02May01  Peter S Galbraith <psg@debian.org>
   Fix empty History menu when on bug numbers.
V1.22 02May01  Peter S Galbraith <psg@debian.org>
   Fontify version number (e.g. NMU in warning-face)
V1.23 02May01  Peter S Galbraith <psg@debian.org>
   Bypass imenu-progress-message because it breaks byte-compilation (?)
V1.24 03May01  Peter S Galbraith <psg@debian.org>
   Correct fix for imenu-progress-message macro (can't rely on variable
   defined here for loading of imenu during byte-compilation).
V1.25 04May01  Peter S Galbraith <psg@debian.org>
   Add `experimental' distribution.
V1.26 04May01  Peter S Galbraith <psg@debian.org>
   Web site changed the URL for package searches:
   http://cgi.debian.org/cgi-bin -> http://packages.debian.org/cgi-bin
V1.27 04May01  Peter S Galbraith <psg@debian.org>
   Set new version to `experimental' when last one was set to that.
   closes: #96260: Default to the same distribution as the previous release
V1.28 04May01  Peter S Galbraith <psg@debian.org>
   Make `set-distribution' and `set-urgency' unavailable when changelog
   is finalised (error at command line and menu grayed-out).
V1.29 04May01  Peter S Galbraith <psg@debian.org>
   Add-to auto-mode-alist in case not using dpkg-dev-el package.
V1.30 09May01  Peter S Galbraith <psg@debian.org>
   Fixed brain-damaged auto-mode-alist added in V1.29 (*blush*).
V1.31 28May01  Peter S Galbraith <psg@debian.org>
   Fix typo (closes: #98577).
   Add a message display after each call to browse-url.
V1.32 28May01  Peter S Galbraith <psg@debian.org>
 - XEmacs21's easy-menu-define doesn't like :active.
 - XEmacs21 need easy-menu-add call in mode setup.
 - debian-changelog-setheadervalue: check at this lower level if finalised.
V1.33 29May01  Peter S Galbraith <psg@debian.org>
   Fix History IMenu for XEmacs21 (it doesn't autoload
   match-string-no-properties).
V1.34 29May01  Peter S Galbraith <psg@debian.org>
 - debian-changelog-fontify-version: allow version numbers with many hyphens
 - debian-changelog-suggest-version: heavy changes to deal with many hyphens
V1.35 06Jun01 Peter S Galbraith <psg@debian.org>
 - patch from Brian Warner <warner@lothar.com> to make
   debian-changelog-local-variables-maybe-remove-done really buffer-local.
 - Change another occurrence of make-local-variable.
V1.36 11Jun01 Peter S Galbraith <psg@debian.org>
   changed urgency "critical" to "emergency".
   See http://lists.debian.org/debian-policy-0106/msg00095.html
V1.37 11Jun01 Peter S Galbraith <psg@debian.org>
   debian-changelog-suggest-version: another tweak when upstream version
   number contains hyphens (closes: #100162).
V1.38 13Jun01 Peter S Galbraith <psg@debian.org>
   debian-changelog-suggest-version: peppered regexp-quote at various places
   to match package names and version that contain regexp characters.
V1.39 13Jun01 Peter S Galbraith <psg@debian.org>
   change (provide 'debian-changelog) to (provide 'debian-changelog-mode)
   (closes: #100639)  Thanks *again* Yann Dirson!
V1.40 22Jun01 Peter S Galbraith <psg@debian.org>
   Changed urgency "emergency" back to "critical" (!)
   See http://lists.debian.org/debian-policy-0106/msg00240.html
V1.41 04Jul01 Peter S Galbraith <psg@debian.org>
   debian-changelog-finalised-p updated by Tommi Virtanen <tv@debian.org>
   (closes: #102088)
V1.42 10Jul01 Peter S Galbraith <psg@debian.org>
   debian-changelog-finalised-p: tweak regexp (really closes: #102088)
V1.43 25Jul01 Peter S Galbraith <psg@debian.org>
   font-lock enforces 2 space exactly between email and date.
V1.44 26Jul01 Peter S Galbraith <psg@debian.org>
    No conditions left to keep variable block (See bug #105889)
    - Removed debian-changelog-package-native-p function.
    - Removed debian-changelog-local-variables-email-p function.
    - Removed debian-changelog-local-variables-remove-address function.
    - Removed debian-changelog-local-variables-remove-mode function.
    - Created debian-changelog-local-variables-remove function.
V1.45 15Aug01 Peter S Galbraith <psg@debian.org>
   Bug list menu added (via wget).
V1.46 15Aug01 Roland Mas <lolando@debian.org>
   One-character tweak to package name font-lock regexp.
V1.47 15Aug01 Peter S Galbraith <psg@debian.org>
   debian-changelog-web-bug: bug fix when called from menu
V1.48 19Sep01 Brian Warner <warner@lothar.com>
 - move to end of file before prompting for removal of local variables.
 - remove global def of debian-changelog-local-variables-maybe-remove-done.
V1.49 22Nov01 Roland Mas <lolando@debian.org>
   debian-changelog-suggest-version: tweak regexp for case of upstream
   version number with a single character.
V1.50 30Nov01 Roland Mas <lolando@debian.org>
   replaced debian-changelog.el by debian-changelog-mode.el
V1.51 24Jan02 Peter S Galbraith <psg@debian.org>
   debian-changelog-web-bugs: return all bugs for the source package.
V1.52 07Feb02 Peter S Galbraith <psg@debian.org>
   debian-changelog-build-bug-menu: return all bugs for the source package.
V1.53 13May02 Peter S Galbraith <psg@debian.org>
   debian-changelog-mode:  Add call to hack-local-variables since the "Local
   variables:" block wasn't parsed otherwise.  Strange.
V1.54 29May02 Peter S Galbraith <psg@debian.org>
   s/font-latex-warning-face/debian-changelog-warning-face/
   Now that was a weird leftover from cut/paste!
V1.55 03June02 Peter S Galbraith <psg@debian.org>
   fontify woody-proposed-updates as frozen.
V1.56 25July02 Peter S Galbraith <psg@debian.org>
   debian-changelog-mode: Remove call to hack-local-variables added in V1.53
   since a "mode: debian-changelog" setting created an infinite loop.
   The bug I attemped to fix in V1.53 occurred when debian-changelog-mode
   was invoked using the debian-changelog-find-file-hook mecanism in
   50dpkg-dev-el.el.  This invoked debian-changelog-mode which called
   kill-all-local-variables, deleting our settings.  To get around this, I
   no longer call 'text-mode' and copied whatever setting we need from it
   (because it also kill-all-local-variables).
   closes: #153982.
V1.57 29July02 Peter S Galbraith <psg@debian.org>
   debian-changelog-mode: Reinsert kill-all-local-variables removed in
   last version.  It's used by font-lock-mode to turn on font-lock-mode
   when global-font-lock-mode is used.  Since this kills the Local
   Variables, the mode can no longer be entered late in the game as was
   done in 50dpkg-dev-el.el by a find-file-hooks.  Instead, use a
   change-log-mode-hook which is less intrusive anyway.
V1.58 29July02 Peter S Galbraith <psg@debian.org>
   debian-changelog-greater-than: new function to determine if a version
   number is greater than another.  Used it to incorporate some logic
   for for better guessing of new version numbers for native packages to
   fix bug #113964.
V1.59 02Aug2002 Peter S Galbraith <psg@debian.org>
   Remove a bunch of code duplicated in debian-bug.el and load that file
   instead.
    debian-changelog-web-bugs -> debian-bug-web-bugs
    debian-changelog-web-bug  -> debian-bug-web-bug
    debian-changelog-web-packages -> debian-bug-web-packages
    debian-changelog-web-package  -> debian-bug-web-package
   dpkg-dev-el package should depend on versioned debbugs-el.
V1.60 15Aug2002 Peter S Galbraith <psg@debian.org>
   Update list of possible distributions to upload to from list given
   from http://bugs.debian.org/150466  (Closes: #156762)
V1.61 20Aug2002 Peter S Galbraith <psg@debian.org>
   Prompt for confirmation and give *big* warning if user wants to set
   the upload distribution to a -security one.  See discussion on
   http://bugs.debian.org/150466
V1.62 20Aug2002 Peter S Galbraith <psg@debian.org>
V1.63 05Sep2002 Peter S Galbraith <psg@debian.org>
   Fontify bugs on multiple-line closes: statements.  Patch from
   Frédéric Bothamy.  (Closes: #159041)
V1.64 05Sep2002 Peter S Galbraith <psg@debian.org>
   debian-changelog-suggest-version fix (Closes: #159643)
V1.65 05Sep2002 Peter S Galbraith <psg@debian.org>
 - Stupid bug fix.  s/debian-bug-bug-alist/debian-bug-alist/.
 - Bug closing regexp enhancement from Roland Mas.
V1.66 24Oct2002 Peter S Galbraith <psg@debian.org>
 - Add UNRELEASED distribution, patch from Junichi Uekawa
   <dancer@netfort.gr.jp> with additional menu entry (Closes: #166163).
   See bug #164470 for relevance and usage of UNRELEASED distribution.
V1.67 14Apr2003 Peter S Galbraith <psg@debian.org>
 - Use debian-bug.el's debian-bug-open-alist (needs emacs-goodies-el 19.4)
V1.68 21Apr2003 Peter S Galbraith <psg@debian.org>
   Byte-compilation cleanup.
V1.69 27Apr2003 Peter S Galbraith <psg@debian.org>
 - defcustom debian-changelog-mode-hook added.  (Closes: #190853)
 - debian-changelog-add-version creates new version in empty file
   (Closes: #191285)
V1.70 28May2003 Peter S Galbraith <psg@debian.org>
 - Define (really) match-string-no-properties for XEmacs (Closes: #195181)
V1.71 02Sep2003 Peter S Galbraith <psg@debian.org>
 - When closing a bug, insert bug title and thanks if bug info was
   downloaded from the web.
V1.72 17Sep2003 Peter S Galbraith <psg@debian.org>
 - Added browse-url link to `Best Practices for debian/changelog' in menu.
V1.73 04Nov2003 Peter S Galbraith <psg@debian.org>
 - checkdoc fixed (not complete!)
 - Add autoload tag.
V1.74 22Nov2003 Peter S Galbraith <psg@debian.org>
 - Make `debian-changelog-add-entry' works from files in unpacked sources.
   Thanks to Junichi Uekawa for suggesting it (Closes: #220641)
V1.75 27Nov2003 Peter S Galbraith <psg@debian.org>
 - Add menu entry for "Archived Bugs for This Package", for
   "Developer Page for This Package" and
   "Developer Page for This Maintainer".
 - Added function `debian-changelog-maintainer' and interactive command
   `debian-changelog-web-developer-page'.
V1.76 17Dec2003 Peter S Galbraith <psg@debian.org>
 - debian-changelog-setdistribution: Use `should-use-dialog-box-p' on XEmacs
   (Closes: #224187)
V1.77 19Feb2004 Peter S Galbraith <psg@debian.org>
 - Add file NEWS.Debian to auto-mode-alist.  Thanks to Chris Lawrence
   for suggesting it.  (Closes: #233310)
V1.78 14Apr2004 Peter S Galbraith <psg@debian.org>
 - debian-changelog-setdistribution: Dismiss warning window when setting
   distribution to security.  Thanks to Martin Schulze (Closes: #234730)
 - Should mark line beginning with a tab as invalid. Fontified in warning
   face.  Thanks to Michel Daenzer (Closes: #235310).
V1.79 07June2005 Jari Aalto <jari.aalto@cante.net>
 - fix byte-compilation warning about
   `(fboundp (quote imenu))' called for effect (Closes: #309788)
V1.80 15Sep2005   Rafael Laboissiere <rafael@debian.org>
 - Add debian-changelog-add-version-hook defaulting to
   debian-changelog-add-new-upstream-release (Closes: #296725)
V1.81 19Sep2005 Peter S Galbraith <psg@debian.org>
 - Add outline-regexp and C-cC-n and C-cC-p movement commands as
   suggested by Romain Francoise <rfrancoise@debian.org> (Closes: #322994)
V1.82 05Sep2006 Peter Samuelson <peter@p12n.org>
 - Add tilde support for upstream version numbers (Closes: #382514)
V1.83 11Oct2006 Luca Capello <luca@pca.it>
 - Rename `debian-changelog-maintainer' to `debian-changelog-last-maintainer',
   this is what the function really work on
 - `debian-changelog-last-maintainer' now returns a list of "(NAME EMAIL)"
   and not only EMAIL
 - Add `debian-changelog-comaintainer-insert', which actually inserts the
   co-maintainer name in the form "[ NAME ]"
 - Add `debian-changelog-comaintainer', which checks if we're in a
   co-maintenance, calling `debian-changelog-comaintainer-insert'
 - Add co-maintenance support to `debian-changelog-unfinalise-last-version'
V1.84 14May2007 Peter S Galbraith <psg@debian.org>
 - Use "date -R" instead of deprecated "822-date"
   (Closes: #423142, #423155, #423828)
 - Tighter regexp for finalisation string
V1.85 25Jul2007 Peter S Galbraith <psg@debian.org>
 - Adapt patch from Luca Capello <luca@pca.it> for bug #431091
V1.86 08Aug2007 Peter S Galbraith <psg@debian.org>
 - auto-mode-alist  for "/debian/*NEWS" files (Closes: #424779)
V1.87 02Sep2007 Peter S Galbraith <psg@debian.org>
 - Implement pacakge lookup on http://packages.debian.org/
   See http://bugs.debian.org/87725
 - Patch from Luca Capello <luca@pca.it> to add keys to generate the
   open bug alist.
V1.88 12Apr2008 Trent W. Buck <trentbuck@gmail.com>
 - Generalize auto-mode-alist entry.
   See http://bugs.debian.org/457047
V1.89 23Feb2009 Jari.aalto@cante.net
 - finalize date in UTC (User configurable) (Closes: #503700)
V1.90 24Oct2009 Rafael Laboissiere <rafael@debian.org>
 - debian-changelog-close-bug does not work properly under XEmacs 21.4.21
   because the arguments passed to replace-in-string in the inline function
   debian-chagelog--rris are in the wrong order. Closes: #476271
V1.91 12Nov2009 Peter S Galbraith <psg@debian.org>
   Updated URL for "Best practices".
V1.92 27Apr2010 Peter S Galbraith <psg@debian.org>
   Invoke `debian-bug-build-bug-menu' with SOURCE arg set to t.
   Needs debian-el 33.2
V1.93 10May2010 Peter S Galbraith <psg@debian.org>
   Fix typo (Closes: #580818)
V1.94 28Jul2010 Kevin Ryde <user42@zip.com.au>
   Simplify auto-mode-alist (Closes: #587924)
V1.95 01Dec2013 Matt Kraai <kraai@debian.org>
   Change the default urgency to medium (Closes: #731105)
V1.96 06Nov2016 Guido Günther <agx@sigxcpu.org>
 Bug fix: "improve handling of {old-, }stable-proposed-updates", thanks
   to Guido Gunther (Closes: #818010).
V1.97 06Nov2016 Pierre Carrier (on 2013-07-04)
 https://bugs.launchpad.net/ubuntu/+source/emacs-goodies-el/+bug/1197870
 Bug fix #803767 debian-changelog-mode: don't rely on external date
 See also https://github.com/pcarrier/debian-changelog-mode/commit/285d4cc938468fd3d7d74584da7981705727fbab
V1.98 06Nov2016 Kumar Appaiah <a.kumar@alumni.iitm.ac.in>
 highlight backports (Closes: #708317)

Acknowledgements:  (These people have contributed)
  Roland Rosenfeld <roland@debian.org>
  James LewisMoss <dres@ioa.com>
  Rafael Laboissiere <rafael@icp.inpg.fr>
  Brian Warner <warner@lothar.com>
  Yann Dirson <dirson@debian.org>
