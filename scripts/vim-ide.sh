#!/usr/bin/env zsh

local session_name
session_name="$(pwd | rev | cut -d '/' -f1 | rev)"

if ! $(tmux has-session -t "$session_name" 2> /dev/null); then
    tmux -2 new-session -d -s "$session_name"

if [ -f .tconf ]; then       # normally the config will be in the project root
        tconf=".tconf"
          elif [ -f ../.tconf ]; then  # if it's not, check the parent directory too
                  tconf="../.tconf"
                    fi
                      [ "$tconf" ] && source "$tconf"
                  fi

                  tmux -2 attach-session -t "$session_name"
