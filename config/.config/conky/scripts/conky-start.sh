#!/bin/bash
COMPONENTS_PATH=~/.config/conky/themes/personal

killall conky
sleep 10 &
# Load left panel
# conky -c ${COMPONENTS_PATH}/left-panel.conky &
conky -c ~/.config/conky/themes/conky-blue/conkyrc &
# Load right panel
conky -c ${COMPONENTS_PATH}/right-panel.conky &
# conky -c ${COMPONENTS_PATH}/zenzire.conky &

exit 0
