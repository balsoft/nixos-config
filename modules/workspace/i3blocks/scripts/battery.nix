{ acpi, bash, iconfont, low_threshold ? 10, ... }: 
''
#!${bash}/bin/bash
BATTERY="`${acpi}/bin/acpi -b | grep --invert-match unavailable | head -1`"
STATUS=`awk -F'[,:] ' '{print $2}' <<< "$BATTERY"`
CHARGE=`awk -F'[,%] ' '{print $2}' <<< "$BATTERY" | tr -d "%"`
TIME=`awk -F', ' '{print $3}' <<< "$BATTERY" | cut -d " " -f 1`
case "$STATUS" in
  Full) ;& "Not charging") icon=; text="FULL"; status=0;;
  Charging) icon=; text="$CHARGE% ($TIME)"; status=0;;
  Discharging)
    if [[ $CHARGE -gt ${toString low_threshold} ]]
    then
      icon=
      text="$CHARGE% ($TIME)";
      status=0
    else
      icon=
      text="$CHARGE% ($TIME)";
      status=33
    fi
  ;;
esac
echo "<span font=\"${iconfont}\">$icon</span> $text"
exit $status
''
