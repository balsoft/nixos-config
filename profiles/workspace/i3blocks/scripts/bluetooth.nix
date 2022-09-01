{ iconfont, gnugrep, bash, bluez, pulseaudio, utillinux, python3, ... }:
''
  #!${bash}/bin/bash
  if ${utillinux}/bin/rfkill | ${gnugrep}/bin/grep bluetooth > /dev/null; then
    if ${utillinux}/bin/rfkill | ${gnugrep}/bin/grep bluetooth | ${gnugrep}/bin/grep blocked > /dev/null; then
      if ${bluez}/bin/bluetoothctl info > /dev/null; then
        if ${pulseaudio}/bin/pactl list sinks | ${gnugrep}/bin/grep bluez > /dev/null; then
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
''
