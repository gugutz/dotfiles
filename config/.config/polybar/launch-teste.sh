#!/bin/bash
(
  flock 200

  killall -q polybar

  while pgrep -u $UID -x polybar > /dev/null; do sleep 0.5; done

  outputs=$(xrandr --query | grep " connected" | cut -d" " -f1)
  tray_output=DVI-I-1

  for m in $outputs; do
    if [[ $m == "DVI-I-2" ]]; then
      tray_output=$m
    fi
  done

  for m in $outputs; do
    export MONITOR=$m
    export TRAY_POSITION=none
    echo $outputs
    if [[ $m == $tray_output ]]; then
      TRAY_POSITION=right
    fi

    polybar --reload gugutz_top </dev/null >/var/tmp/polybar-$m.log 2>&1 200>&- &
    polybar --reload gugutz_bottom </dev/null >/var/tmp/polybar-$m.log 2>&1 200>&- &
    disown
  done
) 200>/var/tmp/polybar-launch.lock
