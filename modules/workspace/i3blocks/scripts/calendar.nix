{ config, gcalcli, bash, iconfont, ... }: ''
  #!${bash}/bin/bash
  ping -c 1 calendar.google.com &> /dev/null || exit 1 
  AGENDA_NEXT="`PYTHONIOENCODING=utf8 ${
    gcalcli
  }/bin/gcalcli --nocolor search "*" 'now' 'now+6d' --nostarted --tsv | head -1`"
  DATE="`awk '{print $1 " " $2}' <<< "$AGENDA_NEXT"`"
  echo -n "<span font='${iconfont}'></span> "
  if [[ `date -d "$DATE" +'%u'` -eq `date +'%u'` ]]
  then
    echo -n `date -d "$DATE" +'%H:%M'`
  else
    echo -n `date -d "$DATE" +'%H:%M %A'`
  fi
  if [[ $BLOCK_BUTTON == "1" ]]
  then
    ${config.defaultApplications.term.cmd} --hold -e "${gcalcli}/bin/gcalcli agenda"
  fi
  if [[ $((`date -d "$DATE" +%s`-`date +%s`)) -lt 1800 ]]
  then
    code=33
  else
    code=0
  fi
  echo ":`awk '{$1=""; $2=""; $3=""; $4=""; print}' <<< "$AGENDA_NEXT" | tr -s " " | tr -s " "`"
''
