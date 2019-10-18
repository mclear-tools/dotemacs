This porting makes blackboard-theme no longer rely on color-theme package

How to use:
First, add a local directory to custome-theme-load-path,
(add-to-list 'custom-theme-load-path "~/home/$USER/drop/the/theme/to")
Then drop this theme into it,
M-x load-theme, then choose blackboard, it should work
Or, simple use (load-theme 'blackboard t) to enable the theme from start.
