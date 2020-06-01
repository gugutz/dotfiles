#!/bin/bash
#Give a name to the terminal window.
TITLE="cmatrix"
#HxW size given in # of charactors. Example: 25x60
DIMENSIONS=1920x1080
#X+Y position given in pixels. Example: 100+200
POSITION=0+0
TERMOPTS="--hide-borders --hide-toolbar --hide-menubar --title=$TITLE"

if [[ -z $DIMENSIONS || -z $POSITION ]] || [[ -z $TITLE ]] ; then
	echo "wallterm has not been configured! Please edit this script before running it."
	exit 1
fi

xfce4-terminal $TERMOPTS --geometry=$DIMENSIONS+$POSITION &
#gnome-terminal $TERMOPTS --geometry=$DIMENSIONS+$POSITION &

opt1="-r "$TITLE" -b add,below,sticky"
opt2="-r "$TITLE" -b add,skip_taskbar,skip_pager"

while true 
do
	if [[ "$(wmctrl -l | grep -o "$TITLE")" = $TITLE ]] ; then 
		wmctrl $opt1 && wmctrl $opt2
		echo 'done!'
		break
	fi
done