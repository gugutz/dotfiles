#!/bin/bash

nitrogen_wallpaper() {
    echo $(tac "$HOME/.config/nitrogen/bg-saved.cfg" | grep file= -m1 | sed 's/file=//')
}

run_lock() {
    i3lock -i "$(nitrogen_wallpaper)" -t $@
}

case "$1" in
    lock)
        if [[ $(pgrep -c i3lock) -eq 0 ]]; then
            st -e cmatrix &
            i3-fullscreen
            run_lock -n

        fi
        ;;
    logout)
        i3-msg exit || bspc quit
        ;;
    suspend)
        run_lock && systemctl suspend
        ;;
    reboot)
        run_lock && systemctl reboot
        ;;
    poweroff)
        run_lock && systemctl poweroff
        ;;
    *)
        echo "Usage: $0 [lock|logout|suspend|reboot|poweroff]"
        exit 1
        ;;
esac

exit 0
