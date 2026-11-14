{ iconfont, bash, systemd, ... }:
''
  #!${bash}/bin/bash
  if [ -n "$BLOCK_BUTTON" ]; then
    if ! ${systemd}/bin/systemctl is-active -q powersave; then ${systemd}/bin/systemctl start powersave;
    elif ! ${systemd}/bin/systemctl is-active -q performance; then ${systemd}/bin/systemctl start performance;
    else ${systemd}/bin/systemctl start balanced; fi
  fi &
  echo -n '<span font="Material Icons">'
  status=0
  ${systemd}/bin/systemctl is-active -q powersave && echo -n "󱟦"
  ${systemd}/bin/systemctl is-active -q balanced && echo -n "󰗑"
  ${systemd}/bin/systemctl is-active -q performance && echo -n "󱓞" && status=33
  echo '</span>'
  exit $status
''
