#!/bin/bash

# Source: this github issue recommended on polybar github page itself
# URL: https://github.com/polybar/polybar/issues/763


# The difference from this script and the "default" one:

# * Consistently show tray on desired monitor (HDMI1 if present, otherwise eDP1). Could also use two different bar configs, but since I want my bars to otherwise be identical that is a hassle.
# * Assume xrandr & i3 are used and present. No need for extra complexity for use-cases that are irrelevant for me
# * Use flock to avoid race conditions
# * Disown polybar instance & close stdin to keep autorandr from getting confused
# * Preserve polybar output in logfile
home_primary_monitor=DVI-I-2
home_secondary_monitor=DVI-I-1
work_primary_monitor=eDP1
work_secondary_monitor=DP1
outputs=$(xrandr --query | grep " connected" | cut -d" " -f1)

bar_top=gugutz_top
bar_bottom=gugutz_bottom
outputsArray=($outputs)

connectedOutputs=$(xrandr | grep " connected" | sed -e "s/\([A-Z0-9]\+\) connected.*/\1/")
activeOutput=$(xrandr | grep -E " connected (primary )?[1-9]+" | sed -e "s/\([A-Z0-9]\+\) connected.*/\1/")

tray_output=eDP1

# Terminate already running bar instances
killall -v polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar > /dev/null; do sleep 0.5; done

# get a list of all currently connected monitors


for monitor in $outputs; do

    export MONITOR=$monitor
    export TRAY_POSITION=none

    # render different bars for different monitors
    if [[ $monitor == $home_primary_monitor ]] || [[ $monitor == $work_secondary_monitor ]]; then
        # if theres also a second monitor, kill all instances first so there wont be two bars loaded
        echo "main monitor $monitor detected. loading main bars for it."
        tray_output=$monitor
        MONITOR=$monitor polybar $bar_top --reload -l info &
        MONITOR=$monitor polybar $bar_bottom --reload -l info &
        echo "Bars launched for primary monitor..."
    fi
    # if any of the secondary monitors is connected, load different bars for the small monitors
    if [[ $monitor == $home_secondary_monitor ]] || [[ $monitor == $work_primary_monitor ]]; then
        echo "second monitor $monitor is also connected. loading different bars for it."
        MONITOR=$monitor polybar second_monitor_bar_top --reload -l info &
        MONITOR=$monitor polybar second_monitor_bar_bottom --reload -l info &
        # its this fucking else clause that is missing things up
        # elif [[$monitor == $work_primary_monitor]]; then
        echo "Bars launched for secondary monitor..."
        # case for when only the work notebook monitor is detected
    elif [[ $monitor == $work_primary_monitor ]]; then
        echo "only one monitor detected = $monitor"
        echo "array of monitors $outputs"
        echo "array of monitors $outputs[0]"
        echo "array of monitors $outputs[1]"
        echo "monitor home primary $home_primary_monitor"
        echo "monitor home secondary $home_secondary_monitor"
        echo "monitor work primary $work_primary_monitor"
        echo "monitor work secondary $work_secondary_monitor"
        tray_output=$monitor
        MONITOR=$monitor polybar gugutz_top --reload -l info &
        MONITOR=$monitor polybar gugutz_bottom --reload -l info &
        echo "Bars launched..."
    fi
done

# dunstify -u low  "Bars launched"
