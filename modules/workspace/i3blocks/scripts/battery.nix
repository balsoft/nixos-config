{ acpi, bash, iconfont, low_threshold ? 10, ... }: ''
  #!${bash}/bin/bash
  BATTERY="`${acpi}/bin/acpi -b | grep --invert-match unavailable | head -1`"
  STATUS=`awk -F'[,:] ' '{print $2}' <<< "$BATTERY"`
  CHARGE=`awk -F'[,%] ' '{print $2}' <<< "$BATTERY" | tr -d "%"`
  TIME=`awk -F', ' '{print $3}' <<< "$BATTERY" | cut -d " " -f 1`
  case "$STATUS" in
    Full) ;& "Not charging") icon=; text="FULL"; status=0
    ;;
    Charging) 
      text="$CHARGE% ($TIME)"
      status=0
      case $CHARGE in
        [2-3]*) icon=;;
        [4-5]*) icon=;;
        [6-7]*) icon=;;
        [8-9]*) icon=;;
        100) icon=;;
        *) icon=;;
      esac
    ;;
    Discharging)
      if [[ $CHARGE -gt ${toString low_threshold} ]]
      then
        status=0
      else
        status=33
      fi
      text="$CHARGE% ($TIME)";
      case $CHARGE in
        1?) icon=;;
        2?) icon=;;
        3?) icon=;;
        4?) icon=;;
        5?) icon=;;
        6?) icon=;;
        7?) icon=;;
        8?) icon=;;
        9?) icon=;;
        100) icon=;;
        *) ;;
      esac
    ;;
  esac
  echo "<span font=\"${iconfont}\">$icon</span> $text"
  exit $status
''
