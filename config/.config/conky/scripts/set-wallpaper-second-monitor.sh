#! /bin/bash

# the +1920+0 after the -g 1920x1080 argument is the offset. the x offset is 1920, so the wallpaper is rendered on the second monitor

xwinwrap -ni -o 0.9 -g 1920x1080+1920+0 -fs -s -st -sp -b -nf -- mplayer -nosound -loop 0 -saturation 100 -brightness -20 -contrast 50 -wid WID ~/.conky/taunix/images/matrix.gif