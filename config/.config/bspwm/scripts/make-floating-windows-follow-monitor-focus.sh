# make floating windows follow mouse clics on different monitors
# source: https://www.reddit.com/r/bspwm/comments/hq8ygu/making_floating_scratchpad_windows_follow_focused/fxx4giv?utm_source=share&utm_medium=web2x
while bspc subscribe monitor_focus -c 1 > /dev/null; do
    while bspc node 'any.!local.!hidden.sticky.window' -d focused; do
        :
    done
done
