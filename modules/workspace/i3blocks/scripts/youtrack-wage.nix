{ bash, yt-utilities, libqalculate, config, lib, ... }:
let ec = config.home-manager.users.balsoft.programs.emacs.finalPackage;
in
''
  #!${bash}/bin/bash
  set -euo pipefail
  export PATH="$PATH:${yt-utilities}/bin:${ec}/bin:${libqalculate}/bin"
  HOURS_MONTH=$(yt org local --since $(date +'%Y-%m-01') | tail -1)
  MONEY_MONTH=$(qalc -t "($HOURS_MONTH) * (${config.secrets.wage})")
'' + (if config.deviceSpecific.bigScreen then ''
  HOURS_DAY=$(yt org local --since $(date +'%Y-%m-%d') | tail -1)
  HOURS_YEAR=$(yt org local --since $(date +'%Y-01-01') | tail -1)
  MONEY_DAY=$(qalc -t "($HOURS_DAY) * (${config.secrets.wage})")
  MONEY_YEAR=$(qalc -t "($HOURS_YEAR) * (${config.secrets.wage})")
  TASK=$(emacsclient --eval "org-mode-line-string" 2>/dev/null || echo -n none)
  if ! [[ "$TASK" == "none" ]]; then
    HOURS_TASK=$(echo "$TASK" | head -1 | cut -d\" -f 2 | cut -d\[ -f2 | cut -d\] -f1)h
    MONEY_TASK=$(qalc -t -e "$HOURS_TASK * (${config.secrets.wage})" )
    echo "$MONEY_TASK/$MONEY_DAY($HOURS_DAY)/$MONEY_MONTH($HOURS_MONTH)/$MONEY_YEAR"
  else
    echo "$MONEY_DAY($HOURS_DAY)/$MONEY_MONTH($HOURS_MONTH)/$MONEY_YEAR"
  fi
'' else ''
  echo "$MONEY_MONTH($HOURS_MONTH)"
'')
