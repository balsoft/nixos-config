{ bash, networkmanager, iconfont, config, modemmanager, ... }: ''
  #!${bash}/bin/bash
  [[ $BLOCK_BUTTON -eq 1 ]] && ${config.defaultApplications.term.cmd} -e ${networkmanager}/bin/nmtui-connect
  CONNECTIONS=$(${networkmanager}/bin/nmcli con show --active | tail +2 | tr -s ' ' | rev | cut -d' ' -f3 | rev)
  text=""
  for connection in $CONNECTIONS
  do
    grep wifi <<< $connection > /dev/null && {
      SIGNAL=$(${networkmanager}/bin/nmcli d w | grep '^\*' | tr -s ' ' | cut -d' ' -f7)
      if [[ $SIGNAL -lt 20 ]]
      then
        text+=冷
      elif [[ $SIGNAL -lt 40 ]]
      then
        text+=爛
      elif [[ $SIGNAL -lt 60 ]]
      then
        text+=嵐
      elif [[ $SIGNAL -lt 80 ]]
      then
        text+=襤
      else
        text+=蠟
      fi
    }
    grep gsm <<< $connection >/dev/null && {
      MODEM=$(${modemmanager}/bin/mmcli -K -L | tail -1 | cut -d: -f2 | tr -d ' ')
      STATUS=$(${modemmanager}/bin/mmcli -K -m $MODEM)
      TECH=$(grep "modem.generic.access-technologies.value\[1\]" <<< $STATUS | cut -d: -f2 | tr -d ' ')
    
      SIGNAL=$(grep "modem.generic.signal-quality.value" <<< $STATUS | cut -d: -f2 | tr -d ' ')
      if [[ $SIGNAL -lt 20 ]]
      then
        text+=""
      elif [[ $SIGNAL -lt 40 ]]
      then
        text+=""
      elif [[ $SIGNAL -lt 60 ]]
      then
        text+=""
      elif [[ $SIGNAL -lt 80 ]]
      then
        text+=""
      else
        text+=""
      fi
      if [[ $TECH == lte ]]
      then
        text+=""
      else
        text+=""
      fi
    }
    grep ethernet <<< $connection > /dev/null && text+=""
  done
  code=0
  [[ $text == "" ]] && {
    text=
    code=33
  }
  echo "<span font='${iconfont}'>$text</span>"
  exit $code
''
