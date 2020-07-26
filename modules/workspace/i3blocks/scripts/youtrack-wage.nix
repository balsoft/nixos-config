{ bash, yt-utilities, libqalculate, config, lib, ... }:
let ec = config.home-manager.users.balsoft.programs.emacs.finalPackage;
in
''
  #!${bash}/bin/bash
  set -euo pipefail
  export PATH="$PATH:${yt-utilities}/bin:${ec}/bin:${libqalculate}/bin"

  HOURS_DAY=$(yt org local --since $(date +'%Y-%m-%d') | tail -1)
  HOURS_MONTH=$(yt org local --since $(date +'%Y-%m-01') | tail -1)
  HOURS_YEAR=$(yt org local --since $(date +'%Y-01-01') | tail -1)
  HOURS_TASK=$(emacsclient --eval "org-mode-line-string" | head -1 | cut -d\" -f 2 | cut -d\[ -f2 | cut -d\] -f1)h
  MONEY_TASK=$(qalc -t -e "$HOURS_TASK * (${config.secrets.wage})" )
  MONEY_DAY=$(qalc -t "($HOURS_DAY) * (${config.secrets.wage})")
  MONEY_MONTH=$(qalc -t "($HOURS_MONTH) * (${config.secrets.wage})")
  MONEY_YEAR=$(qalc -t "($HOURS_YEAR) * (${config.secrets.wage})")
'' + (if config.deviceSpecific.bigScreen then ''
  echo "$MONEY_TASK/$MONEY_DAY($HOURS_DAY)/$MONEY_MONTH($HOURS_MONTH)/$MONEY_YEAR"
'' else ''
  echo "$MONEY_MONTH($HOURS_MONTH)"
'')
