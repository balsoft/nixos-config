{ bash, yt-utilities, libqalculate, config, lib, ... }:
''
  #!${bash}/bin/bash
  CURRENT_TASK=$(emacsclient --eval "org-mode-line-string" | head -1 | cut -d\" -f 2 | cut -d\[ -f2 | cut -d\] -f1)h
  HOURS_DAY=$(qalc -t -e "$(yt org local --since $(date +'%Y-%m-%d') | tail -1) + $CURRENT_TASK")
  HOURS_MONTH=$(qalc -t "$(yt org local --since $(date +'%Y-%m-01') | tail -1) + $CURRENT_TASK")
  HOURS_YEAR=$(qalc -t "$(yt org local --since $(date +'%Y-01-01') | tail -1) + $CURRENT_TASK")
  MONEY_DAY=$(qalc -t "$HOURS_DAY * (${config.secrets.wage})")
  MONEY_MONTH=$(qalc -t "$HOURS_MONTH * (${config.secrets.wage})")
  MONEY_YEAR=$(qalc -t "$HOURS_YEAR * (${config.secrets.wage})")
'' + (if config.deviceSpecific.bigScreen then ''
  echo "$MONEY_DAY($HOURS_DAY)/$MONEY_MONTH($HOURS_MONTH)/$MONEY_YEAR"
'' else ''
  echo "$MONEY_MONTH($HOURS_MONTH)"
'')
