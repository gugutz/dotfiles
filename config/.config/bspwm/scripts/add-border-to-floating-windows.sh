# adds a large border on floating windows to highlight them
on=4 off=0
bspc query -N -n .floating.window | xargs -I {node} bspc config -n {node} border_width "$on"
bspc query -N -n .!floating.window | xargs -I {node} bspc config -n {node} border_width "$off"
bspc subscribe node_state | while read -r _ _ _ node state status; do
    [[ "$state" == "floating" ]] && bspc config -n "$node" border_width "${!status}"
done
