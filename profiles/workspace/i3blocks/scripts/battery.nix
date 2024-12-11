{ upower, gnugrep, coreutils-full, lib, bash, iconfont, low_threshold ? 10, ... }: ''
  #!${bash}/bin/bash
  PATH=$PATH:${lib.makeBinPath [ gnugrep upower coreutils-full ]}
  readarray -t DEVICES <<< "$(${upower}/bin/upower -e | grep -v "DisplayDevice$")"

  DELIM=""
  STATUS=0

  for dev in "''${DEVICES[@]}"; do
      INFO="$(${upower}/bin/upower -i "$dev")"
      PERCENTAGE="$(echo "$INFO" | grep 'percentage:' | grep -o '[[:digit:]]*')"
      if [ -n "$PERCENTAGE" ]; then
          TIME="$(echo "$INFO" | grep 'time to' | tr -s ' ' | cut -d' ' -f5-6)"
          STATE="$(echo "$INFO" | grep 'state:' | tr -s ' ' | cut -d' ' -f3)"
          MODEL="$(echo "$INFO" | grep 'model:' | tr -s ' ' | cut -d' ' -f3)"
          if [[ "x$STATE" == "xfully-charged" ]] || [[ "x$STATE" == "xcharging" ]]; then
              case $PERCENTAGE in
                  [0-9]|1[0-9]) icon=;;
                  [2-3][0-9]) icon=;;
                  [4-5][0-9]) icon=;;
                  [6-7][0-9]) icon=;;
                  [8-9][0-9]) icon=;;
                  100) icon=;;
              esac
          else
              if [[ "$PERCENTAGE" -lt ${toString low_threshold} ]]; then
                  STATUS=33
              fi
              case $PERCENTAGE in
                  [0-9]) icon=;;
                  1[0-9]) icon=;;
                  2[0-9]) icon=;;
                  3[0-9]) icon=;;
                  4[0-9]) icon=;;
                  5[0-9]) icon=;;
                  6[0-9]) icon=;;
                  7[0-9]) icon=;;
                  8[0-9]) icon=;;
                  9[0-9]) icon=;;
                  100) icon=;;
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
              echo -n " $MODEL "
              echo -n "$PERCENTAGE% "
              if [ -n "$TIME" ]; then echo -n "($TIME) "; fi
          fi
      fi
  done

  echo
  exit "$STATUS"
''
