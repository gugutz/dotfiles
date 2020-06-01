

# You can save the layout of either a single workspace or an entire output (e.g. LVDS1). Of course, you can repeat this step multiple times if you want to save/restore multiple workspaces/outputs.

# i3-save-tree(1) is a tool to save the layout. It will print a JSON representation of i3’s internal layout data structures to stdout. Typically, you may want to take a quick look at the output, then save it to a file and tweak it a little bit:

i3-save-tree --workspace 1 > ~/dotfiles/i3/layouts/workspace-1.json

# Please note that the output of i3-save-tree(1) is NOT useful until you manually modify it — you need to tell i3 how to match/distinguish windows (for example based on their WM_CLASS, title, etc.). By default, all the different window properties are included in the output, but commented out. This is partly to avoid relying on heuristics and partly to make you aware how i3 works so that you can easily solve layout restoring problems.

# How to modify the file manually is described in [EditingLayoutFiles].

