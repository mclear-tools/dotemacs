This Emacs module implements a bridge to libvterm to display a terminal in a
Emacs buffer.

Installation

And add this to your `init.el`:

```
(add-to-list 'load-path "path/to/emacs-libvterm")
(require 'vterm)
```

If you want to have the module compiled, wrap the call to `require` as follows:

```
(add-to-list 'load-path "path/to/emacs-libvterm")
(let (vterm-install)
  (require 'vterm))
```
