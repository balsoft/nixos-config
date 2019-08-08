{ bash, iw, wpa_supplicant_gui, iconfont, ... }: ''
  #!${bash}/bin/bash
  case $BLOCK_BUTTON in
    1) ${wpa_supplicant_gui}/bin/wpa_gui & ;;
    2) pkexec rfkill block wifi;;
    3) pkexec rfkill unblock wifi;;
  esac
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
