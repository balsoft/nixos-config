{ bash, yt-utilities, libqalculate, config, lib, ... }:
''
  #!${bash}/bin/bash
  CURRENT_TASK=$(emacsclient --eval "org-mode-line-string" | head -1 | cut -d\" -f 2 | cut -d\[ -f2 | cut -d\] -f1)h
  HOURS_DAY=$(yt org local --since $(date +'%Y-%m-%d') | tail -1)
  HOURS_MONTH=$(yt org local --since $(date +'%Y-%m-01') | tail -1)
  HOURS_YEAR=$(yt org local --since $(date +'%Y-01-01') | tail -1)
  MONEY_DAY=$(qalc -t -e "($HOURS_DAY+$CURRENT_TASK) * (${config.secrets.wage})")
  MONEY_MONTH=$(qalc -t -e "($HOURS_MONTH+$CURRENT_TASK) * (${config.secrets.wage})")
  MONEY_YEAR=$(qalc -t -e "($HOURS_YEAR+$CURRENT_TASK) * (${config.secrets.wage})")
'' + (if config.deviceSpecific.bigScreen then ''
  echo "$MONEY_DAY($HOURS_DAY)/$MONEY_MONTH($HOURS_MONTH)/$MONEY_YEAR"
'' else ''
  echo "$MONEY_MONTH($HOURS_MONTH)"
'')
