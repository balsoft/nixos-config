{ bash, iw, iconfont, ...}:
''
#!${bash}/bin/bash
WIFI="`${iw}/bin/iw wlan0 info | grep ssid | cut -f2 -d' '`"
if [[ `wc -c <<< "$WIFI"` -lt 2 ]]
then
  echo "<span font='${iconfont}'></span>"
  exit 33
else
  echo "<span font='${iconfont}'></span> $WIFI"
  exit 0
fi
''
