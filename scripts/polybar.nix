{ pkgs, secret, theme, ... }:
rec {
    weather = { color_good ? theme.green, color_rain ? theme.red, color_cold ? theme.blue, city, city-id, owm-key ? secret.owm-key, terminal ? "${pkgs.konsole}/bin/konsole --noclose --fullscreen -e", interval ? 60 }: 
    pkgs.writeTextFile { 
        name = "weather.sh"; 
        text = ''
        #!${pkgs.bash}/bin/bash
        while true
        do
            {
            get_icon() {
                case $1 in 
                    01d) icon=🌣;;
                    01n) icon=🌙;;
                    02*) icon=🌤;;
                    03*) icon=☁;;
                    04*) icon=🌥;;
                    09*) icon=🌧;;
                    10*) icon=🌦;;
                    11*) icon=🌩;;
                    13*) icon=🌨;;
                    50*) icon=🌫;;
                esac
                echo $icon
            }
            get_color() {
                if [ $1 -lt 0 ]
                then
                    color=${color_cold}
                else
                    color=${color_good}
                fi
                if [[ $2 =~ ^(09|10|11).* ]]
                then
                    color=${color_rain}
                fi
                echo $color
            }
            weather=$(${pkgs.curl}/bin/curl -sf "http://api.openweathermap.org/data/2.5/weather?APPID=${owm-key}&id=${city-id}&units=metric")
            if [ ! -z "$weather" ]; then
                weather_temp=$(echo "$weather" | ${pkgs.jq}/bin/jq ".main.temp" | cut -d "." -f 1)
                weather_icon=$(echo "$weather" | ${pkgs.jq}/bin/jq -r ".weather[0].icon")
                
                echo "%{F${theme.bg} A:${terminal} "curl wttr.in/${city}":}%{T2}$(get_icon "$weather_icon")%{T-}" "$weather_temp°%{A-}"
                echo $(get_color $weather_temp $weather_icon)
            fi
            } > $1.new
            mv $1.new $1
            sleep ${toString interval}
        done''; 
        executable = true;
    };
    email = { color_unread ? theme.red, color_nounread ? theme.green, user, password, interval ? 10}: 
    pkgs.writeTextFile { 
        name = "email.py";
        text = ''
        #!${pkgs.python3}/bin/python3
        import imaplib
        import time
        import sys
        while True:
            obj = imaplib.IMAP4_SSL('imap.gmail.com', 993)
            obj.login("${user}", "${password}")
            obj.select()
            l = len(obj.search(None, 'unseen')[1][0].split())
            with open(sys.argv[1], "w") as f:
                f.write("%{F${theme.bg} }%{A:${pkgs.chromium}/bin/chromium https\://inbox.google.com:}%{T3}📧%{T-} "+str(l)+"%{A-}\n")
                f.write("${color_unread}" if l != 0 else "${color_nounread}")
            time.sleep(${toString interval})''; 
        executable = true;
    };
    now = {interval ? 60}: 
    pkgs.writeTextFile { 
        name = "now.sh";
        text = ''
        #!${pkgs.bash}/bin/bash
        while true
        do
            {
            echo -n "`date +'%%{F${theme.bg}} %%{T3}⌚%%{T-} %H:%M %%{T3}📆%%{T-} %A, %d'`"
            echo -n "%{F${theme.red}}"
            echo "`${pkgs.gcalcli}/bin/gcalcli --nocolor agenda 'now' 'now+1s' | head -2 | tail -1 | awk '{$1=""; $2=""; $3=""; $4=""; print}' | tr -s ' '`" 
            echo "${theme.fg}"
            } > $1.new
            mv $1.new $1
            sleep ${toString interval}
        done'';
        executable = true;
    };
    next = {interval ? 60}: 
    pkgs.writeTextFile {
        name = "next.sh";
        text = ''
            #!${pkgs.bash}/bin/bash
            while true
            do
                {
                AGENDA_NEXT="`${pkgs.gcalcli}/bin/gcalcli --nocolor --nostarted search "*" 'now' 'now+6d' | head -2 | tail -1`"
                DATE="`awk '{print $1 " " $2}' <<< "$AGENDA_NEXT"`"
                echo -n "%{F${theme.bg}}📅 "
                if [[ `date -d "$DATE" +'%u'` -eq `date +'%u'` ]]
                then
                    echo -n `date -d "$DATE" +'%H:%M'`
                else
                    echo -n `date -d "$DATE" +'%H:%M %A'`
                fi
                if [[ $((`date -d "$DATE" +%s`-`date +%s`)) -lt 1800 ]]
                then
                    color=${theme.red}
                else
                    color=${theme.blue}
                fi
                echo ": `awk '{print $3 " " $4 " " $5}' <<< "$AGENDA_NEXT"`"
                echo $color
                } > $1.new
                mv $1.new $1
                sleep ${toString interval}
            done
        ''; 
        executable = true;
    };

    text_and_color_for_powerline = (arr: ''
            ${builtins.concatStringsSep "\n" (builtins.genList (i: ''
            readarray -t arr${builtins.toString i} < "/tmp/${builtins.hashString "md5" (toString (builtins.elemAt arr i))}"
            text[${builtins.toString i}]=''${arr${builtins.toString i}[0]}
            color[${builtins.toString i}]=''${arr${builtins.toString i}[1]}
            '') (builtins.length arr))}
            color[${builtins.toString (builtins.length arr)}]="${theme.bg}"
    '');

    start_scripts = (arr: builtins.concatStringsSep "\n" (map (x: "${x} /tmp/${builtins.hashString "md5" (toString x)} &") arr));

    left_side = (arr: pkgs.writeTextFile {
        name = "polybar-left-side.sh";
        text = ''
            #!${pkgs.bash}/bin/bash
            ${start_scripts arr}
            while true
            do
                ${text_and_color_for_powerline arr}
                for index in `seq 0 ${builtins.toString(builtins.length arr)}`
                do 
                    cur_color=''${color[index]}
                    cur_text=''${text[index]}
                    next_color=''${color[`expr $index + 1`]}
                    if [[ $cur_color = $next_color ]]
                    then 
                        echo -n "%{B$cur_color}$cur_text%{B$next_color}%{F$cur_color}%{T4} %{T-}"
                    else
                        echo -n "%{B$cur_color}$cur_text%{B$next_color}%{F$cur_color}%{T4} %{T-}"
                    fi
                done
                echo
                sleep 1
            done'';
        executable = true;
    });

    network = {color_down ? theme.red, color_up ? theme.green, interval ? 5}: 
    pkgs.writeTextFile {
        name = "network.sh";
        text = ''
            #!${pkgs.bash}/bin/bash
            while true
            do
                WIFI="`nmcli d w | grep "\*" | awk '{print "%{F${theme.bg}}%{T5}"$8"%{T-}" $2}'`"
                {
                if [[ `wc -c <<< "$WIFI"` -lt 4 ]]
                then
                    echo "W down"
                    echo "${color_down}"
                else
                    echo "$WIFI"
                    echo "${color_up}"
                fi
                } > $1.new
                mv $1.new $1
                sleep ${toString interval}
            done
        '';
        executable = true;
    };

    battery = {color_charging ? theme.green, color_discharging ? theme.fg, color_full ? theme.blue, color_low ? theme.red, low_threshold ? 10, interval ? 1}: 
    pkgs.writeTextFile {
        name = "battery.sh";
        text = ''
            #!${pkgs.bash}/bin/bash
            while true
            do
                BATTERY="`${pkgs.acpi}/bin/acpi -b`"
                STATUS=`awk '{print $3}' <<< "$BATTERY" | tr -d ','`
                CHARGE=`awk '{print $4}' <<< "$BATTERY" | tr -d ',%'`
                TIME=`awk '{print $5}' <<< "$BATTERY"`
                {
                    echo -n "%{F${theme.bg}}"
                    case "$STATUS" in
                        Full) echo "%{T3}⚡ %{T-}FULL"; echo "${color_full}";;
                        Charging) echo "%{T3}⚡ %{T-}$CHARGE% ($TIME)"; echo "${color_charging}";;
                        Discharging)
                            echo "%{T3}🔋%{T-} $CHARGE% ($TIME)";
                            if [[ $CHARGE -gt ${builtins.toString low_threshold} ]]
                            then
                                echo "${color_discharging}"
                            else
                                echo "${color_low}"
                            fi
                        ;;
                    esac
                } > $1.new
                mv $1.new $1
                sleep ${toString interval}
            done
        '';
        executable = true;
    };
    status = {interval ? 0}: 
    pkgs.writeTextFile {
        name = "status.sh";
        text = ''
            while true
            do
                {
                    echo -n "%{F${theme.bg}}"
                    echo -n "`whoami`@`hostname` "
                    echo -n "`top -b -n2 -p 1 | fgrep "Cpu(s)" | tail -1 | awk -F'id,' -v prefix="$prefix" '{ split($1, vs, ","); v=vs[length(vs)]; sub("%", "", v); printf "%s%.1f%%\n", prefix, 100 - v }'` "
                    echo -n $(${pkgs.bc}/bin/bc -l <<< "scale=2; `cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq|sort|head -1`/1000000")
                    echo -n "GHz "
                    echo -n "$((`cat /sys/class/thermal/thermal_zone*/temp | sort | tail -1`/1000))° "
                    echo "`free | tail -2 | head -1 | awk '{print "scale=3; "$7"/1000000"}' | ${pkgs.bc}/bin/bc -l` GB"
                    echo "${theme.fg}"
                } > $1.new
                mv $1.new $1
                sleep ${toString interval}
            done
        '';
        executable = true;
    };
    right_side = (arr: pkgs.writeTextFile {
        name = "polybar-right-side.sh";
        text = ''
            #!${pkgs.bash}/bin/bash
            ${start_scripts arr}
            while true
            do
                ${text_and_color_for_powerline arr}
                for index in `seq 0 ${builtins.toString((builtins.length arr) - 1)}`
                do
                    cur_color=''${color[index]}
                    cur_text=''${text[index]}
                    prev_color=''${color[`expr $index - 1`]}
                    if [[ $cur_color = $prev_color ]]
                    then
                        echo -n "%{B$prev_color}%{F${theme.bg}}%{T4} %{T-}$cur_text"
                    else
                        echo -n "%{B$prev_color}%{F$cur_color}%{T4} %{T-}%{B$cur_color}$cur_text"
                    fi
                done
                sleep 1
                echo
            done'';
        executable = true;
    });
}
