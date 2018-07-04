Shows a list of imenu entries for the current buffer, in another
buffer with the name "*Ilist*".

Activation and deactivation:
M-x imenu-list-minor-mode

Key shortcuts from "*Ilist*" buffer:
<enter>: Go to current definition
<space>: display current definition
<tab>: expand/collapse subtree

Change "*Ilist*" buffer's position and size:
`imenu-list-position', `imenu-list-size'.

Should invoking `imenu-list-minor-mode' also select the "*Ilist*"
window?
`imenu-list-focus-after-activation'
