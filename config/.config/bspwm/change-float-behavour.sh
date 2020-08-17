#!/bin/sh

# If you want to modify the behaviour of sticky window to make them also follow the focused desktop between monitors, you can use this subscribe script:
# source: https://www.reddit.com/r/bspwm/comments/hq8ygu/making_floating_scratchpad_windows_follow_focused/fxx4giv?utm_source=share&utm_medium=web2x

while :; do
    bspc subscribe monitor_focus -c 1 > /dev/null
    while bspc node 'any.!local.!hidden.sticky.window' -d focused; do
        :
    done
done
