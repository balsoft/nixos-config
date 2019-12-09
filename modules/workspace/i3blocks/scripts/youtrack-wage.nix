{ bash, yt-utilities, libqalculate, config, ... }:
''
#!/bin/bash
HOURS_DAY=$(yt org local --since $(date +'%Y-%m-%d') | tail -1)
HOURS_MONTH=$(yt org local --since $(date +'%Y-%m-01') | tail -1)
HOURS_YEAR=$(yt org local --since $(date +'%Y-01-01') | tail -1)
MONEY_DAY=$(qalc -s "precision 1" -t -e "$HOURS_DAY * (${config.secrets.wage})")
MONEY_MONTH=$(qalc -s "precision 1" -t -e "$HOURS_MONTH * (${config.secrets.wage})")
MONEY_YEAR=$(qalc -s "precision 1" -t -e "$HOURS_YEAR * (${config.secrets.wage})")
echo "$MONEY_DAY($HOURS_DAY)/$MONEY_MONTH($HOURS_MONTH)/$MONEY_YEAR"
''
