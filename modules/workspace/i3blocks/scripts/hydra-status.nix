{ bash, curl, libnotify, sox, ... }:
''
  #!${bash}/bin/bash
  STATUS=$(${curl}/bin/curl -sw "%{http_code}" https://hydra.typeable.io/ping)
  if [ $STATUS -eq 200 ]
  then
    echo 🐍
    exit 0
  else
    msg="HYDRA DOWN"
    echo $msg
    exit 33
  fi
''
