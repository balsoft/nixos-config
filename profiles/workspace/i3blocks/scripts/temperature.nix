{ ... }:
''
temp=$((`cat /sys/class/thermal/thermal_zone*/temp | sort | tail -1`/1000))
echo $temp °
[[ $temp -gt 80 ]] && exit 33
''
