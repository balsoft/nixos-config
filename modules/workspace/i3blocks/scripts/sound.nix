{ pamixer, lxqt, iconfont, config, lib, ... }: ''
  case $BLOCK_BUTTON in
       2) ${pamixer}/bin/pamixer -t;;
       4) ${pamixer}/bin/pamixer -i 5;;
       5) ${pamixer}/bin/pamixer -d 5;;
  esac
  code=0
  if [[ `${pamixer}/bin/pamixer --get-mute` = "true" ]]
  then
    volume=""
    end=""
    icon="婢"
  else
    volume=`${pamixer}/bin/pamixer --get-volume`
    end="%"
    if [[ $volume -lt 33 ]]
    then
      icon="奄"
    else
      if [[ $volume -lt 66 ]]
      then
        icon="奔"
      else
        icon="墳"
        code=33
      fi
    fi
  fi
  ${lib.optionalString (! config.deviceSpecific.bigScreen) "[[ -n $BLOCK_BUTTON ]] &&"} text=" $volume$end"
  echo "<span font='${iconfont}'>$icon</span>$text"
  exit $code
''
