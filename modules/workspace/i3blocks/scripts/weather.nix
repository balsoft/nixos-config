{ bash, config, curl, iconfont, ... }: ''
  #!${bash}/bin/bash
  WEATHER=$(${curl}/bin/curl wttr.in/\?format=3 | awk -F": " '{print $2}')
  ICON=$(echo $WEATHER | cut -d' ' -f1)
  TEXT=$(echo $WEATHER | cut -d' ' -f2)
  echo '<span font="Roboto Mono 11">$ICON</span> $TEXT'
  if [[ $BLOCK_BUTTON == 1 ]]
  then
    ${config.defaultApplications.term.cmd} --hold -e "${curl}/bin/curl wttr.in"
  fi
''
