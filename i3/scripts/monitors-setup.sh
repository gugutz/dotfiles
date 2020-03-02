#!/bin/bash

# Find current monitors

xrandr_output=/tmp/xrandr.txt
xrandr>$xrandr_output

# monitors IDs
home_main_monitor="DVI-I-2"
home_second_monitor="DVI-I-1"
# PS: In STAIRS, the second monitor is the main big monitor, while the main monitor is the notebook's monitor
stairs_main_monitor="eDP1"
stairs_second_monitor="DP1"

xrandr_command=

# colors for echo commands using tput command
# color must be reseted at the end of the line or the entire file will use previous used color
red=`tput setaf 1`
green=`tput setaf 2`
reset_color=`tput sgr0`
bg_white=`tput setab 7`


# Set main and secondary monitors
# xrandr prints "DP-1 connected" and "DP-2 disconnected" or vice versa.
main_monitor=
if grep -F "$home_main_monitor connected" $xrandr_output >/dev/null;  then
    main_monitor=$home_main_monitor
    echo "Main monitor set to ${green}$main_monitor${reset_color}"
elif grep -F "$stairs_main_monitor connected" $xrandr_output >/dev/null;  then
    main_monitor=$stairs_main_monitor
    echo "Main monitor set to ${green}$main_monitor${reset_color}"
else
   echo "Main monitor not found!"
fi


second_monitor=
if grep -F "$home_second_monitor connected" $xrandr_output >/dev/null;  then
    second_monitor=$home_second_monitor
    echo "Second monitor set to ${green}$second_monitor${reset_color}"
#
elif grep -F "$stairs_second_monitor connected" $xrandr_output >/dev/null;  then
    second_monitor=$stairs_second_monitor
    echo "Second monitor set to ${green}$second_monitor${reset_color}"
#
else
   echo "Second monitor not found!"
fi

# if HOME second monitor is disconnected, use main monitor to both i3 screens
if [ "$main_monitor" = "$home_main_monitor" ] && [ "$second_monitor" = "" ]; then
  echo "External monitor is not connected!"
  # hack to make i3 think the second monitor is present and send all workspaces to main monitor
  second_monitor=$main_monitor
  xrandr --output \$main_monitor --primary --mode 1920x1080 --pos 0x0 --rotate normal
  #$xrandr_command="xrandr --output \$main_monitor --primary --mode 1920x1080 --pos 0x0 --rotate normal"
  #
elif [ "$second_monitor" = "$home_second_monitor" ]; then
    echo "Found HOME secondary monitor!"
    echo "Setting home dual screen layout with extended screen mode"
    xrandr --output $main_monitor --primary --mode 1920x1080 --pos 0x0 --rotate normal --output $secondary_monitor --mode 1280x1024 --pos 1920x56 --rotate normal
    # xrandr_command="xrandr --output \$main_monitor --primary --mode 1920x1080 --pos 0x0 --rotate normal --output \$secondary_monitor --mode 1280x1024 --pos 1920x56 --rotate normal"

    #
    # If STAIRS second monitor is connected, use both screens in extended mode
elif [ "$main_monitor" = "$stairs_main_monitor" ] && [ "$second_monitor" = "$stairs_second_monitor" ]; then
    echo "Found STAIRS secondary monitor!"
    echo "Second monitor is on ${reset_color}$second_monitor${reset_color}"
    echo "Setting dual screen layout to use extended screen mode"
    echo "PS: In STAIRS, the second monitor is the big screen in front of me, while the main monitor is the notebook's monitor"
    echo "## inverting monitor variable names in i3 file so that the tray output doenst mess up and is sent to the big screen"
    temp=$second_monitor
    second_monitor=$main_monitor
    main_monitor=$temp
    xrandr --output \$primary_monitor --primary --mode 1920x1080 --pos 0x0 --rotate normal --output \$secondary_monitor --mode 1920x1080 --pos 1920x0 --rotate normal
    #xrandr_command="xrandr --output \$primary_monitor --primary --mode 1920x1080 --pos 0x0 --rotate normal --output \$secondary_monitor --mode 1920x1080 --pos 1920x0 --rotate normal"
    #
    ## if in STAIRS and second monitor is disconnected, then overlap both screens in the main (notebook) monitor
elif [ "$main_monitor" = "$stairs_main_monitor" ] && [ "$second_monitor" = "" ]; then
    echo "STAIRS second monitor is NOT connected!"
    echo "Main monitor is on ${reset_color}$main_monitor${reset_color}"
    echo "Setting single screen layout"
    echo "Overlaping both screens to main (notebook) monitor on ${reset_color}$main_monitor${reset_color}"
    second_monitor=$main_monitor
    xrandr --output \$primary_monitor --primary --mode 1920x1080 --pos 0x0 --rotate normal --output \$secondary_monitor --mode 1920x1080 --pos 0x0 --rotate normal
    #xrandr_command="xrandr --output \$primary_monitor --primary --mode 1920x1080 --pos 0x0 --rotate normal --output \$secondary_monitor --mode 1920x1080 --pos 0x0 --rotate normal"
    #
fi

# mount i3 monitor config file

# cat <<EOT > ~/dotfiles/i3/config.monitors
# ################################################
# #                                              #
# #         gugutz i3-gaps config file           #
# #          http://github.com/gugutz            #
# #                                              #
# ################################################

# ################################################
# # SET FIRST AND SECOND MONITORS
# # this must be run before the rest of the script


# # name of first and second monitors
# # find out yours with the commands:
# # xrandr --current OR xrandr --listmonitors
# #
# # Home monitor config
# #set $main_monitor DVI-I-2
# #set $secondary_monitor DVI-I-1
# # STAIRS monitor config
# # set $main_monitor  eDP1
# # set $secondary_monitor  DP1

# #exec_always --no-startup-id xrandr --output $main_monitor --main

# #exec --no-startup-id ~/dotfiles/i3/scripts/monitors-setup.sh

# # monitors set with my monitors-setup.sh script
# set \$primary_monitor $main_monitor
# set \$secondary_monitor $second_monitor

# # xrandr command to setup screens layout
# exec_always --no-startup-id $xrandr_command

# EOT
