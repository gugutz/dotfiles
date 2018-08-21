# usign -A argument makes new-session behaves like attach-session if session name already exists
tmux new-session -A -s "emacs-ide"

# first split the window vertically for bottom terminal
tmux split-window -v
# resize bottom pane to be smaller
tmux resize-pane -D 14
# execute some command on bottom pane
tmux send-keys "rake" C-m

# create right pane for tailing rails logs
# select upper pane again to split it horizontally
tmux select-pane -U
# open emacs in it
tmux send-keys "emacsclient -c -t" C-m
