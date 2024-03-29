{ bash, gnugrep, coreutils-full, util-linux, networkmanager, iconfont, config, modemmanager, lib, ... }: ''
  #!${bash}/bin/bash
  PATH=$PATH:${lib.makeBinPath [ networkmanager coreutils-full gnugrep modemmanager util-linux.bin ]}
  export PATH
  CONNECTIONS=$(nmcli con show --active | tail +2 | tr -s ' ' | rev | cut -d' ' -f3 | rev)
  text=""
  for connection in $CONNECTIONS
  do
    grep wifi <<< $connection > /dev/null && {
      # SIGNAL=$(${networkmanager}/bin/nmcli d w | grep '^\*' | tr -s ' ' | cut -d' ' -f7)
      # if [[ $SIGNAL -lt 20 ]]
      # then
      #   text+=冷
      # elif [[ $SIGNAL -lt 40 ]]
      # then
      #   text+=爛
      # elif [[ $SIGNAL -lt 60 ]]
      # then
      #   text+=嵐
      # elif [[ $SIGNAL -lt 80 ]]
      # then
      #   text+=襤
      # else
      #   text+=蠟
      # fi
      text=
    }
    grep gsm <<< $connection >/dev/null && {
      MODEM=$(mmcli -K -L | tail -1 | cut -d: -f2 | tr -d ' ')
      STATUS=$(mmcli -K -m $MODEM)
      TECH=$(grep "modem.generic.access-technologies.value\[1\]" <<< $STATUS | cut -d: -f2 | tr -d ' ')

      # SIGNAL=$(grep "modem.generic.signal-quality.value" <<< $STATUS | cut -d: -f2 | tr -d ' ')
      # if [[ $SIGNAL -lt 20 ]]
      # then
      #   text+=""
      # elif [[ $SIGNAL -lt 40 ]]
      # then
      #   text+=""
      # elif [[ $SIGNAL -lt 60 ]]
      # then
      #   text+=""
      # elif [[ $SIGNAL -lt 80 ]]
      # then
      #   text+=""
      # else
      #   text+=""
      # fi
      text+=
      if [[ $TECH == lte ]]
      then
        text+="ﰒ"
      else
        text+="ﰑ"
      fi
    }
    grep ethernet <<< $connection > /dev/null && text+=""
  done
  code=0
  [[ $text == "" ]] && {
    text=
    code=33
  }
  echo "<span font='${iconfont}'>$text</span>"
  exit $code
''
