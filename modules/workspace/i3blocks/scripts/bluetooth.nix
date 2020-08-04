{ iconfont, bash, bluez, pulseaudio, utillinux, python3, ... }:
''
  #!${bash}/bin/bash
  if ${utillinux}/bin/rfkill | grep bluetooth > /dev/null; then
    if ${utillinux}/bin/rfkill | grep bluetooth | grep blocked > /dev/null; then
      if ${bluez}/bin/bluetoothctl info > /dev/null; then
        if ${pulseaudio}/bin/pactl list sinks | grep bluez > /dev/null; then
          echo -n ""
        else
          echo -n ""
        fi
      else
        echo -n ""
      fi
    else
      echo -n ""
    fi
  fi
  DEVICE=$(${bluez}/bin/bluetoothctl info | grep -o "[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]:[0-9A-F][0-9A-F]" | head -1)
  CHARGE=$(${python3.withPackages (ps: [ ps.pybluez ])}/bin/python3 ${./bluetooth_battery.py} $DEVICE)
  code=0
  case $CHARGE in
    1?) icon=; code=33;;
    2?) icon=; code=33;;
    3?) icon=;;
    4?) icon=;;
    5?) icon=;;
    6?) icon=;;
    7?) icon=;;
    8?) icon=;;
    9?) icon=;;
    100) icon=;;
    *) ; code=33;;
  esac
  echo "$icon"
  exit $code
''
