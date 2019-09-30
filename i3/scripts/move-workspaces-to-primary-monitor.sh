#!/bin/sh
# PRIMARY_SCREEN=$( xrandr -q | awk '/ connected primary/ {print $1}' )
PRIMARY_SCREEN="DVI-I-3"

if [ -n "$PRIMARY_SCREEN" ] ; then
    i3-msg "move workspace to output $PRIMARY_SCREEN"
else
    # there is no primary screen
    i3-nagbar -m 'No primary screen defined' -t warning
fi