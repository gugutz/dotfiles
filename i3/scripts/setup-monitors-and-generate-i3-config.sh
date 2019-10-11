#!/bin/bash
echo "## Setting up monitors layout"
~/dotfiles/i3/scripts/monitors-setup.sh
echo "## Generating i3 config file"
~/dotfiles/i3/scripts/mount-i3-config-file.sh
