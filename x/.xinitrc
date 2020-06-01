#!/bin/bash
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# from the arch wiki:
# ~/.xinitrc is handy to run programs depending on X and set environment variables on X server startup. If it is present in a user's home directory, startx and xinit execute it. Otherwise startx will run the default /etc/X11/xinit/xinitrc.


# Sourcing xprofile from a session started with xinit
#
# The xprofile files are natively sourced by the following display managers:
# - GDM - /etc/gdm/Xsession
# - LightDM - /etc/lightdm/Xsession
# - LXDM - /etc/lxdm/Xsession
# - SDDM - /usr/share/sddm/scripts/Xsession
#
# In case of any display manager that isnt in the above list, xprofile has to be source from xinitrc
# It is possible to source xprofile from a session started with one of the following programs:
# - startx
# - xinit
# - XDM
# - Any other Display manager which uses ~/.xsession or ~/.xinitrc
# All of these execute, directly or indirectly, ~/.xinitrc or /etc/X11/xinit/xinitrc if it does not exist. That is why xprofile has to be sourced from these files.
#
# Make sure this is before the 'exec' command or it won't be sourced.

if [ -f /etc/gdm/Xsession ] || [ -f /etc/lightdm/Xsession ] || [ -f /etc/lxdm/Xsession ] || [ -f /usr/share/sddm/scripts/Xsession ]; then
    if [ -f ~/.xprofile ]
    then
        . ~/.xprofile
    elif  [ -f /etc/xprofile ]
    then
        . /etc/xprofile
    fi
fi

# Executed by startx (run your window manager from here)

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

userprofile=$HOME/.xprofile

# DEFAULT_SESSION='i3 --shmlog-size 0'
DEFAULT_SESSION='bspwm'

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

if [ -f "$userprofile" ]; then
    source "$userprofile"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

get_session(){
	  local dbus_args=(--sh-syntax --exit-with-session)
	  case $1 in
		    awesome) dbus_args+=(awesome) ;;
		    bspwm) dbus_args+=(bspwm-session) ;;
		    budgie) dbus_args+=(budgie-desktop) ;;
		    cinnamon) dbus_args+=(cinnamon-session) ;;
		    deepin) dbus_args+=(startdde) ;;
		    enlightenment) dbus_args+=(enlightenment_start) ;;
		    fluxbox) dbus_args+=(startfluxbox) ;;
		    gnome) dbus_args+=(gnome-session) ;;
		    i3|i3wm) dbus_args+=(i3 --shmlog-size 0) ;;
		    jwm) dbus_args+=(jwm) ;;
		    kde) dbus_args+=(startkde) ;;
		    lxde) dbus_args+=(startlxde) ;;
		    lxqt) dbus_args+=(lxqt-session) ;;
		    mate) dbus_args+=(mate-session) ;;
		    xfce) dbus_args+=(xfce4-session) ;;
		    openbox) dbus_args+=(openbox-session) ;;
		    *) dbus_args+=($DEFAULT_SESSION) ;;
	  esac

	  echo "dbus-launch ${dbus_args[*]}"
}

# adjust the monitor layout
# xrandr --output DVI-I-2 --primary --right-of DVI-I-1

exec $(get_session)
