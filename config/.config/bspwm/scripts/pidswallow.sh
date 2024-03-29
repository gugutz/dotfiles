#!/bin/sh
usage="pidswallow (pid swallow for bspwm)
Hides terminal window automatically, so that you don't have to
pidswallow [OPTION ...]
OPTIONS:
        -h  --help              Show this message
        -s  --swallow <CWID>    Hides parent window of the given child window id.
        -v  --vomit <CWID>      Unhides parent window of the given child window id.
        -t  --toggle <CWID>     toggle between swallow and vomit. (default)
        -l  --loop              listen and hide / unhide window on launch / remove.
        -V  --verbose           Shows usefull information.
bugs/issues: https://github.com/liupold/pidswallow.
"
swallowable=" $TERMINAL urxvt kitty "
blacklist=" $swallowable "
verbose=0

vomit() {
    # focuses parent if child is dead.
    unset cwid pwid # similar to local
    cwid="$1"
    [ -f "/tmp/swallowed-by-$cwid" ] || return 1
    pwid=$(cat "/tmp/swallowed-by-$cwid")
    bspc node "$pwid" --flag hidden=off                       #*
    xprop -id "$cwid" >/dev/null 2>&1 || bspc node "$pwid" -f #*
    [ "$verbose" -eq 1 ] && echo "$cwid vomited $pwid"
    rm "/tmp/swallowed-by-$cwid"
}

swallow() {
    # return values
    # 0 -> Found and swallowed
    # 1 -> No swallowable window found.
    # 2x -> xprop return a non zero exit code.(x is returned by xprop)
    # 3 -> window lacks a _NET_WM_PID property.
    # 4 -> window already swallowed
    unset cwid pwid cname pname ppid cpid # similar to local.
    cwid="$1"
    cpid="$(xprop _NET_WM_PID -id "$cwid" 2>/dev/null)" || return "2$?"
    [ "${cpid#*not found*}" != "$cpid" ] && return 3 # window didn't have _NET_WM_PID
    cpid="${cpid##* }"
    cname="$(ps -p "$cpid" -o comm=)"
    [ "${blacklist#* $cname }" != "$blacklist" ] && return 0
    for parent in $(pstree -ATsp "$cpid" \
                        | sed -n -e 's|(|:|g' -e 's|)||g' -e 's|---|\n|g' -e 's|-+-|\n|g' -e '1p' | tac); do
        pname="${parent%%:*}" # ancestor name
        [ "${swallowable#* $pname }" != "$swallowable" ] \
            && ppid="${parent##*:}" && break
    done
    [ -z "$ppid" ] && return 1 # nothing swallowable.
    pwid="$(xdotool search --pid "$ppid" 2>/dev/null)"
    bspc node "$pwid".!hidden --flag hidden=on || return 4                      #*
    [ "$verbose" -eq 1 ] && echo "$cname($cwid) swallowed $pname($pwid)"
    echo "$pwid" > "/tmp/swallowed-by-$cwid"
}

toggle() {
    cwid="$1"
    if [ -f "/tmp/swallowed-by-$cwid" ]; then
        vomit "$cwid"
        return "$?"
    else
        swallow "$cwid"
        return "$?"
    fi
}

loop() { #*
    bspc subscribe node_add node_remove report | while read -r event; do
        [ "${event#node_add}" != "$event" ] && swallow "${event##* }"
        [ "${event#node_remove}" != "$event" ] && vomit "${event##* }"
        [ "$event" = "W" ] && return 0
    done
    return 1 # bspc subscribe exited unexpectedly
}

[ -p /dev/stdin ] && eval set -- "$* $(cat /dev/stdin)" # basic pipe support
[ "$#" -eq 0 ] && echo "$usage" && exit 1

TEMP=$(getopt -o 'Vhlt:s:v:' --long 'verbose,help,loop,toggle:,swallow:,vomit:' \
              -n 'pidswallow' -- "$@")
eval set -- "${TEMP}"; unset TEMP

while true; do
    case "$1" in
        '-h' | '--help' )
            echo "$usage" && exit 0;;
        '-V'|'--verbose')
            verbose=1
            shift; continue;;
        '-t'|'--toggle')
            toggle "$2" || exit "$?"
            shift 2; continue;;
        '-s'|'--swallow')
            swallow "$2" || exit "$?"
            shift 2; continue;;
        '-v'|'--vomit')
            vomit "$2" || exit "$?"
            shift 2; continue;;
        '-l'|'--loop')
            loop || exit "$?"
            shift; continue;;
        '--')
            shift; break;;
        *)
            echo "Internal error!" >&2
            echo "$usage" && exit 1;;
    esac
done

# toggle WID on first non-flag argument
[ -n "$*" ] && toggle "$1" && shift

# error if there's more than one non-flag argument
if [ -n "$*" ]; then
    echo "Unrecognized trailing option '$*'" >&2
    exit 1
fi
