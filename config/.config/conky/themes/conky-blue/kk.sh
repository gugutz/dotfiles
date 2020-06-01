#!/bin/sh
killall conky &
sleep 1
cd ~/.conky
conky -c ./conkyrc &
conky -c ./conkyrc2 & exit
