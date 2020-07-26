{ iconfont, bash, bluez, pulseaudio, utillinux, ... }:
''
  #!${bash}/bin/bash
  if ${utillinux}/bin/rfkill | grep bluetooth > /dev/null; then
    if ${utillinux}/bin/rfkill | grep bluetooth | grep blocked > /dev/null; then
      if ${bluez}/bin/bluetoothctl info > /dev/null; then
        if ${pulseaudio}/bin/pactl list sinks | grep bluez > /dev/null; then
          echo ""
          exit 33
        else
          echo ""
        fi
      else
        echo ""
      fi
    else
      echo ""
      exit 33
    fi
  fi
''
