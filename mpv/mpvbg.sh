#!/bin/sh
#
# http://github.com/mitchweaver/bin
#
# play a video on the root window with mpv
#

if [ ! -f "$1" ] ; then
    set -- "${HOME}/var/tmp/wall"
fi

pkill -9 xwinwrap

xwin="xwinwrap -ni -fdt -sh rectangle -un -b -nf -ov -fs -- "

mpv="mpv --wid WID --no-config --keepaspect=no --loop \
    --no-border --vd-lavc-fast --x11-bypass-compositor=no \
    --gapless-audio=yes --vo=xv --hwdec=auto --really-quiet \
    --name=mpvbg"

$xwin $mpv "$1" > /dev/null 2>&1 &

# store our pid here so we can avoid killing our background later
# Example: $ kill $(pgrep mpv | grep -v $(pgrep -P $(cat ${HOME}/.cache/mpvbg.pid)))
#           --- Here we are killing "all mpvs, except with THIS PID"
#           --- This lets us kill a video we're watching, without stopping our desktop background!
echo -n $! > ${HOME}/.cache/mpvbg.pid
