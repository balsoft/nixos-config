{ upower, bash, iconfont, low_threshold ? 10, ... }: ''
  #!${bash}/bin/bash
  readarray -t DEVICES <<< "$(${upower}/bin/upower -e | grep -v "DisplayDevice$")"

  DELIM=""
  STATUS=0

  for dev in "''${DEVICES[@]}"; do
      INFO="$(${upower}/bin/upower -i "$dev")"
      PERCENTAGE="$(echo "$INFO" | grep 'percentage:' | grep -o '[[:digit:]]*')"
      if [ -n "$PERCENTAGE" ]; then
          STATE="$(echo "$INFO" | grep 'state:' | tr -s ' ' | cut -d' ' -f3)"
          if [[ "x$STATE" == "xfully-charged" ]] || [[ "x$STATE" == "xcharging" ]]; then
              TIME="$(echo "$INFO" | grep 'time to empty:' | tr -s ' ' | cut -d' ' -f5-6)"
              case $PERCENTAGE in
                  [2-3]*) icon=;;
                  [4-5]*) icon=;;
                  [6-7]*) icon=;;
                  [8-9]*) icon=;;
                  100) icon=;;
                  *) icon=;;
              esac
          else
              if [[ "$PERCENTAGE" -lt ${toString low_threshold} ]]; then
                  STATUS=33
              fi
              TIME="$(echo "$INFO" | grep 'time to full:' | tr -s ' ' | cut -d' ' -f5-6)"
              case $PERCENTAGE in
                  1?) icon=;;
                  2?) icon=;;
                  3?) icon=;;
                  4?) icon=;;
                  5?) icon=;;
                  6?) icon=;;
                  7?) icon=;;
                  8?) icon=;;
                  9?) icon=;;
                  100) icon=;;
                  *) icon=;;
              esac
          fi
          if [ -n "$DELIM" ]; then
              echo -n "| "
          else
              DELIM="yes"
          fi
          echo -n "<span font=\"${iconfont}\">"
          if echo "$INFO" | grep 'native-path:' | grep bluez > /dev/null; then
              echo -n ""
          fi
          echo -n "$icon</span>"
          if [ -n "$BLOCK_BUTTON" ]; then
              echo -n " $PERCENTAGE% "
              if [ -n "$TIME" ]; then echo -n "($TIME) "; fi
          fi
      fi
  done

  echo
  exit "$STATUS"
''
