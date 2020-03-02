#!/bin/bash

# Unfortunately it seems the tray can only be displayed on one polybar instance (and monitor) at a time. And when not specifying which monitor gets the tray it seems kind of random when using the above script. So, with thanks to @TobiasKB I'm now using the following script (executed by autorandr):

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
work_primary_monitor=eDPI1
work_secondary_monitor=DP1

(
    flock 200

    # Terminate already running bar instances
    killall -q polybar

    # Wait until the processes have been shut down
    while pgrep -u $UID -x polybar > /dev/null; do sleep 0.5; done

    # get a list of all currently connected monitors
    outputs=$(xrandr --query | grep " connected" | cut -d" " -f1)
    #tray_output=eDP1

    for monitor in $outputs; do

        export MONITOR=$monitor
        export TRAY_POSITION=none

        if [[ $monitor == home_primary_monitor ]] || [[ $monitor == $work_secondary_monitor ]]; then
            tray_output=$monitor
        elif [[ $monitor == work_primary_monitor ]]; then
            # if only the work primary (notebook screen) monitor is connected, then output the tray to it
            tray_output=$monitor
        fi


        # define tray position
        if [[ $monitor == $tray_output ]]; then
            TRAY_POSITION=right
        fi

        # render different bars for different monitors
	      if [ $monitor == $home_primary_monitor ] || [ $monitor == $work_secondary_monitor ]
	      then
            # if home primary or work secondary (the big monitors) are connected, load the main bars on them
            echo "main monitor detected. loading main bars for it."
            MONITOR=$monitor polybar --reload gugutz_top --quiet & </dev/null >/var/tmp/polybar-$m.log 2>&1 200>&- &
            disown
            MONITOR=$monitor polybar --reload gugutz_bottom --quiet & </dev/null >/var/tmp/polybar-$m.log 2>&1 200>&- &
            disown
	      elif [ $monitor == $home_secondary_monitor ] || [$monitor == $work_primary_monitor]
        then
            # if second monitors are connected, load different bars for them
            echo "second monitor is connected. loading different bars for it."
            MONITOR=$monitor polybar --reload second_monitor_bar_top --quiet & </dev/null >/var/tmp/polybar-$m.log 2>&1 200>&- &
            disown
            MONITOR=$monitor polybar --reload second_monitor_bar_bottom --quiet & </dev/null >/var/tmp/polybar-$m.log 2>&1 200>&- &
            disown
        else
            # for any other case, just load the main top and bottom bars
            echo "only one monitor detected."
            MONITOR=$monitor polybar --reload gugutz_top --quiet & </dev/null >/var/tmp/polybar-$m.log 2>&1 200>&- &
            disown
            MONITOR=$monitor polybar --reload gugutz_bottom --quiet & </dev/null >/var/tmp/polybar-$m.log 2>&1 200>&- &
            disown
        fi
    done
) 200>/var/tmp/polybar-launch.lock



echo "Bars launched..."
# dunstify -u low  "Bars launched"
