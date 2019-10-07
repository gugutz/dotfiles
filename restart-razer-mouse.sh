#!/bin/bash
#Restart all USB devices
#/usr/local/bin/restart_usb.sh
set -euo pipefail
IFS=$'\n\t'

VENDOR="1532"
PRODUCT="001f"

for DIR in $(find /sys/bus/usb/devices/ -maxdepth 1 -type l); do
  if [[ -f $DIR/idVendor && -f $DIR/idProduct &&
        $(cat $DIR/idVendor) == $VENDOR && $(cat $DIR/idProduct) == $PRODUCT ]]; then
    echo 0 > $DIR/authorized
    sleep 1 
    echo 1 > $DIR/authorized
  fi
done
