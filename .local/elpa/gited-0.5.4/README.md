# Gited: Operate on Git branches like dired

[![Build Status](https://api.travis-ci.org/calancha/Gited.svg?branch=master)](https://travis-ci.org/calancha/Gited)

This library lists the branches in a Git repository.  Then you can
operate on them with a dired-like interface.

The command **gited-list-branches** prompts for the kind of branch
(local branches, remote branches or tags) and lists them.
This command is used quite often, thus it might be convenient
to give it a key binding.  For instance, if *gited.el* is in
your *load-path*, then you can bind it to **C-x C-g** in Dired buffers
by adding the following lines into your .emacs file:

```
(require 'gited)
(define-key dired-mode-map "\C-x\C-g" 'gited-list-branches)
```

If you are familiar with Dired, then you already know how to use
Gited; that's because most of the Gited commands with a Dired equivalent
share same keybindings.
For instance *gited-rename-branch* is bound to 'R' as *dired-do-rename*.
Similarly, *gited-mark* is bound to 'm' as *dired-mark*.

## How to push to the remote repo. your local changes

Suppose you want to update a file *foo* (*).

From the Gited buffer:

```
c master RET ;  Checkout master branch (**).
*< ; Synchronize with remote repository.
```

Now, update *foo* with your changes and save it.

From the Gited buffer:

```
A ; Stage your changes.
```

```
C-c c "Updated foo" RET ; Commit them.
```

```
*> ; Public your changes into the remote repository.
```

---
(*) We have restricted to 1 file for simplicity.  The recipe works
    for N>=1 files.

(**) For changes that require several commits you might prefer to
     work in a separated branch 'feature'.  In that case you'd
     merge the master branch with 'feature' before ```*>```).


### Screenshots

List the tags from the Emacs repository:

![ScreenShot](/screenshots/gited-tags-screenshot.png)

List the remote branches from the Emacs repository with author and commit date:

![ScreenShot](/screenshots/gited-remote-branches-verbose-screenshot.png)

### Bugs/TODO

* Currently, *origin* is assumed as the remote repository:
  Remove some hardcode *origin* around, and extend it
  to handle multiple remotes.
  
* Pull requests are not implemented.
