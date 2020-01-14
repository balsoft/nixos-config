{ bash, yt-utilities, libqalculate, config, lib, ... }:
''
  #!${bash}/bin/bash
  HOURS_DAY=$(yt org local --since $(date +'%Y-%m-%d') | tail -1)
  HOURS_MONTH=$(yt org local --since $(date +'%Y-%m-01') | tail -1)
  HOURS_YEAR=$(yt org local --since $(date +'%Y-01-01') | tail -1)
  MONEY_DAY=$(qalc -t -e "$HOURS_DAY * (${config.secrets.wage})")
  MONEY_MONTH=$(qalc -t -e "$HOURS_MONTH * (${config.secrets.wage})")
  MONEY_YEAR=$(qalc -t -e "$HOURS_YEAR * (${config.secrets.wage})")
'' + (if config.deviceSpecific.bigScreen then ''
  echo "$MONEY_DAY($HOURS_DAY)/$MONEY_MONTH($HOURS_MONTH)/$MONEY_YEAR"
'' else ''
  echo "$MONEY_MONTH($HOURS_MONTH)"
'')
