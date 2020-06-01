#!/bin/bash

# mount i3 config file

cat $HOME/dotfiles/i3/config.monitors \
    $HOME/dotfiles/i3/config.general > $HOME/dotfiles/i3/config
echo "## generated i3 config file at $HOME/dotfiles/i3"
#exec /usr/bin/i3
