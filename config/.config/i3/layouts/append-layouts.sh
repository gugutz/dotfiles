# From a terminal or script:
i3-msg "workspace 1; append_layout /home/michael/.i3/workspace-1.json"

# In your i3 configuration file, you can autostart i3-msg like this:
# (Note that those lines will quickly become long, so typically you would store
#  them in a script with proper indentation.)
exec --no-startup-id "i3-msg 'workspace 1; append_layout /home/michael/.i3/workspace-1.json'"
