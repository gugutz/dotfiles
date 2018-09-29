#!/bin/bash
COMPONENTS_PATH="/home/tau/dotfiles/conky/components"

killall conky
sleep 10 &
# Load left panel
conky -c ${COMPONENTS_PATH}/left-panel.conky &
# Load right panel
conky -c ${COMPONENTS_PATH}/right-panel.conky &
# conky -c ${COMPONENTS_PATH}/zenzire.conky &

exit 0
