{ bash, config, curl, iconfont, ... }: ''
  #!${bash}/bin/bash
  WTTR=$(curl wttr.in/?format=1)
  echo "<span font=\"Roboto Mono 11\">$WTTR</span>"
  if [[ $BLOCK_BUTTON == 1 ]]
  then
    ${config.defaultApplications.term.cmd} --hold -e "${curl}/bin/curl wttr.in"
  fi
''
 
