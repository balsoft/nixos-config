{ bash, curl, libnotify, sox, ... }:
''
  #!${bash}/bin/bash
  STATUS=$(${curl}/bin/curl -sw "%{http_code}" http://hydra.typeable.io/ping)
  if [ $STATUS -eq 200 ]
  then
    echo 🐍
    exit 0
  else
    msg="ПИЗДЕЦ ГИДРЕ"
    echo $msg
    ${libnotify}/bin/notify-send -u critical "$msg"
    ${sox}/bin/play -n synth 1 square 440 vol 1
    exit 33
  fi
''
