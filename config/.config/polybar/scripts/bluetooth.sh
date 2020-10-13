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
  echo "%{T3}%{F#66ffffff} %{T-}"
else
  if [ $(echo info | bluetoothctl | grep 'Device' | wc -c) -eq 0 ]
  then
    echo "%{T3}  %{T-}"
  fi
  echo "%{T3} %{F#2193ff} %{T-}"
fi
