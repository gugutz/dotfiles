#!/bin/bash

# Source: https://github.com/alols/xcape

# Map an unused modifier's keysym to the spacebar's keycode and make it a
# control modifier. It needs to be an existing key so that emacs won't
# spazz out when you press it. Hyper_L is a good candidate.
spare_modifier="Hyper_L"
xmodmap -e "keycode 65 = $spare_modifier"
xmodmap -e "remove mod4 = $spare_modifier" # hyper_l is mod4 by default
xmodmap -e "add Control = $spare_modifier"

# Map space to an unused keycode (to keep it around for xcape to
# use).
xmodmap -e "keycode any = space"

# Finally use xcape to cause the space bar to generate a space when tapped.
xcape -e "$spare_modifier=space"

# Note regarding xmodmap
# If you are in the habit of remapping keycodes to keysyms (eg, using xmodmap), there are two issues you may encounter.

# You will need to restart xcape after every time you modify the mapping from keycodes to keysyms (eg, with xmodmap), or xcape will still use the old mapping.

# The key you wish to send must have a defined keycode. So for example, with the default mapping Control_L=Escape, you still need an escape key defined in your xmodmap mapping. (I get around this by using 255, which my keyboard cannot send).
