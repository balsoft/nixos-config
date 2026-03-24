{ iconfont, brightnessctl, lib, config, ... }: ''
  case $BLOCK_BUTTON in
       4) sudo ${lib.getExe brightnessctl} s '10%+';;
       5) sudo ${lib.getExe brightnessctl} s '10%-';;
  esac
  LIGHT=$(${lib.getExe brightnessctl} | grep "Current brighness" | head -1 | cut -d'(' -f 2 | cut -d')' -f 1)
  if [[ $LIGHT -lt 33 ]]
  then
    icon=
  else
    if [[ $LIGHT -lt 66 ]]
    then
      icon=
    else
      icon=
    fi
  fi
  ${if config.device == "ASUS-Laptop" then ''
    if [[ `cat /sys/devices/platform/asus-nb-wmi/als_enable` -eq 1 ]] 
    then
      icon=""
    fi
  '' else
    ""}
  [[ -n $BLOCK_BUTTON ]] && text=" $LIGHT"
  echo "<span font='${iconfont}'>$icon</span>$text"
''
