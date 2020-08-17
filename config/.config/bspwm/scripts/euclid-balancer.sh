#!/bin/dash

if [ "$(pgrep -cx euclid_balancer)" -gt 1 ] ; then
	killall euclid_balancer && exit 0
else

  bspc subscribe node_add node_remove node_state node_geometry | while read line; do
	  for wid in $(bspc query -N -d -n .window); do
		  bspc node "${wid}#@north" -B || true
		  bspc node "${wid}#@south" -B || true
	  done
  done

fi
