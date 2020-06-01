#!/bin/sh
# Depends on TLP
# https://github.com/linrunner/TLP

# if [ "$(bluetooth)" = "bluetooth = on" ]
# then
#     echo "  "
# else
#     echo "  "
# fi

#!/bin/sh
if [ $(bluetoothctl show | grep "Powered: yes" | wc -c) -eq 0 ]
then
  echo "%{F#66ffffff}"
else
  if [ $(echo info | bluetoothctl | grep 'Device' | wc -c) -eq 0 ]
  then
    echo ""
  fi
  echo "%{F#2193ff}"
fi


