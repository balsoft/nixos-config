{ bash, curl, libnotify, sox, iconfont, ... }:
''
  #!${bash}/bin/bash
  STATUS=$(${curl}/bin/curl -sw "%{http_code}" https://hydra.typeable.io/ping)
  if [ $STATUS -eq 200 ]
  then
    echo '<span font="${iconfont}">🐍</span>'
    exit 0
  else
    msg="HYDRA DOWN"
    echo $msg
    exit 33
  fi
''
