{ bash, config, curl, iconfont, ... }: ''
  #!${bash}/bin/bash
  WTTR=$(${curl}/bin/curl wttr.in/?format=1)
  echo "<span font=\"Roboto Mono 11\">$WTTR</span>"
''
