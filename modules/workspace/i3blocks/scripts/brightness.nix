{ iconfont, light, config, ... }:
''
case $BLOCK_BUTTON in
     4) ${light}/bin/light -A 5;;
     5) ${light}/bin/light -U 5;;
esac
LIGHT=`${light}/bin/light | cut -f 1 -d '.'`
if [[ $LIGHT -lt 33 ]]
then
  icon=
else
  if [[ $LIGHT -lt 66 ]]
  then
    icon=
  else
    icon=
  fi
fi
${
if config.device == "ASUS-Laptop" then 
''
if [[ `cat /sys/devices/platform/asus-nb-wmi/als_enable` -eq 1 ]] 
then
  icon=""
fi
'' 
else ""
}
echo "$icon $LIGHT"
''
