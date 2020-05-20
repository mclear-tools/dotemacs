`frog-jump-buffer' allows you to hop to any Emacs buffer in 2-3 key strokes.

`(frog-jump-buffer)' is the main entry-point.  Bind it to your preferred key-binding.

It opens the `frog-menu' buffer selector.  The buffers appear in order
of most recent display or selection.

Selecting the `avy' character next to a buffer switches to that
buffer.

Use `0' to toggle between opening in the same window or
`(other-window)'.

The numbers 1 through 6 will cycle through the default buffer filters.
