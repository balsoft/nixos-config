{ pkgs, secret, theme, ... }:
rec {
    wrapScriptToLoop = interval: script:
    pkgs.writeTextFile {
        name = "${script.name}";
        executable = true;
        text = ''
            while true
            do
                touch /tmp/${script.name}.new
                ${script} 2>/dev/null > /tmp/${script.name}.new
                if [[ -e /tmp/${script.name}.new ]] && [[ `wc -l < /tmp/${script.name}.new` -gt 1 ]]
                then
                    mv /tmp/${script.name}.new /tmp/${script.name}
                fi
                sleep ${toString interval}
            done 2> /dev/null
        '';
    };

    weather = { color_good ? theme.green, color_rain ? theme.red, color_cold ? theme.blue, city, city-id, owm-key ? secret.owm-key, terminal ? "${pkgs.konsole}/bin/konsole --noclose --fullscreen -e", interval ? 60 }: 
    wrapScriptToLoop interval (pkgs.writeTextFile { 
        name = "weather"; 
        text = ''
            #!${pkgs.bash}/bin/bash
            ping -c 1 api.openweathermap.org &> /dev/null || exit 1 
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
            fi''; 
        executable = true;
    });
    email = { color_unread ? theme.red, color_nounread ? theme.green, user, password, interval ? 10}: 
    wrapScriptToLoop interval (pkgs.writeTextFile { 
        name = "email";
        text = ''
        #!${pkgs.python3}/bin/python3
        import imaplib
        try:
            obj = imaplib.IMAP4_SSL('imap.gmail.com', 993)
            obj.login("${user}", "${password}")
            obj.select()
            l = len(obj.search(None, 'unseen')[1][0].split())
            print("%{F${theme.bg} }%{A:${pkgs.chromium}/bin/chromium https\://inbox.google.com:}%{T3}📧%{T-} "+str(l)+"%{A-}")
            print("${color_unread}" if l != 0 else "${color_nounread}")
        except:
            pass'';
        executable = true;
    });
    time = {interval ? 1}:
    wrapScriptToLoop interval (pkgs.writeTextFile { 
        name = "time";
        text = ''
        #!${pkgs.bash}/bin/bash 
        echo "`date +'%%{F${theme.bg}} %%{T3}⌚%%{T-} %H:%M %%{T3}📆%%{T-} %A, %d'`"
        echo "${theme.fg}"
        '';
        executable = true;
    });
    now = {interval ? 60}: 
    wrapScriptToLoop interval (pkgs.writeTextFile { 
        name = "now";
        text = ''
        #!${pkgs.bash}/bin/bash
        ping -c 1 calendar.google.com &> /dev/null || exit 1 
        echo -n "%{F${theme.red}}"
        echo "`${pkgs.gcalcli}/bin/gcalcli --nocolor agenda 'now' 'now+1s' | head -2 | tail -1 | awk '{$1=""; $2=""; $3=""; $4=""; print}' | tr -s ' '`" 
        echo "${theme.fg}"
        '';
        executable = true;
    });
    next = {interval ? 60}: 
    wrapScriptToLoop interval (pkgs.writeTextFile {
        name = "next";
        text = ''
            #!${pkgs.bash}/bin/bash
            ping -c 1 calendar.google.com &> /dev/null || exit 1 
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
        ''; 
        executable = true;
    });

    text_and_color_for_powerline = (arr: ''
            ${builtins.concatStringsSep "\n" (builtins.genList (i: ''
            [[ -e "/tmp/${(builtins.elemAt arr i).name}" ]] && readarray -t arr${builtins.toString i} < "/tmp/${(builtins.elemAt arr i).name}"
            text[${builtins.toString i}]=''${arr${builtins.toString i}[0]}
            color[${builtins.toString i}]=''${arr${builtins.toString i}[1]}
            '') (builtins.length arr))}
            color[${builtins.toString (builtins.length arr)}]="${theme.bg}"
    '');

    start_scripts = (arr: builtins.concatStringsSep "\n" (map (x: "touch /tmp/${x.name} && ${x} &") arr));

    left_side = (arr: pkgs.writeTextFile {
        name = "polybar-left-side";
        text = ''
            #!${pkgs.bash}/bin/bash
            echo -n " "
            ${start_scripts arr}
            while true
            do
                ${text_and_color_for_powerline arr}
                for index in `seq 0 ${builtins.toString(builtins.length arr)}`
                do 

                    cur_color=''${color[index]}
                    cur_text=''${text[index]}
                    next_color=''${color[`expr $index + 1`]}
                    if [[ ! -z "$cur_text" ]]
                    then
                        if [[ $cur_color = $next_color ]]
                        then 
                            echo -n "%{B$cur_color}$cur_text%{B$next_color}%{F$cur_color}%{T4} %{T-}"
                        else
                            echo -n "%{B$cur_color}$cur_text%{B$next_color}%{F$cur_color}%{T4} %{T-}"
                        fi
                    fi
                done
                echo
                sleep 1
            done'';
        executable = true;
    });

    network = {color_down ? theme.red, color_up ? theme.green, interval ? 5}: 
    wrapScriptToLoop interval (pkgs.writeTextFile {
        name = "network";
        text = ''
            #!${pkgs.bash}/bin/bash
            WIFI="`nmcli d w | grep "\*" | awk '{print "%{F${theme.bg}}%{T5}"$8"%{T-} " $2}'`"
            if [[ `wc -c <<< "$WIFI"` -lt 4 ]]
            then
                echo "%{F${theme.bg}} W down"
                echo "${color_down}"
            else
                echo "$WIFI"
                echo "${color_up}"
            fi
        '';
        executable = true;
    });

    battery = {color_charging ? theme.green, color_discharging ? theme.fg, color_full ? theme.blue, color_low ? theme.red, low_threshold ? 10, interval ? 1}: 
    wrapScriptToLoop interval (pkgs.writeTextFile {
        name = "battery";
        text = ''
            #!${pkgs.bash}/bin/bash
            BATTERY="`${pkgs.acpi}/bin/acpi -b`"
            STATUS=`awk '{print $3}' <<< "$BATTERY" | tr -d ','`
            CHARGE=`awk '{print $4}' <<< "$BATTERY" | tr -d ',%'`
            TIME=`awk '{print $5}' <<< "$BATTERY"`
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
        '';
        executable = true;
    });
    status = {interval ? 0}: 
    wrapScriptToLoop interval (pkgs.writeTextFile {
        name = "status";
        text = ''
            echo -n "%{F${theme.bg}}"
            echo -n "`whoami`@`hostname` "
            echo -n "`top -b -n2 -p 1 | fgrep "Cpu(s)" | tail -1 | awk -F'id,' -v prefix="$prefix" '{ split($1, vs, ","); v=vs[length(vs)]; sub("%", "", v); printf "%s%.1f%%\n", prefix, 100 - v }'` "
            echo -n $(${pkgs.bc}/bin/bc -l <<< "scale=2; `cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq|sort|head -1`/1000000")
            echo -n "GHz "
            echo -n "$((`cat /sys/class/thermal/thermal_zone*/temp | sort | tail -1`/1000))° "
            echo "`free | tail -2 | head -1 | awk '{print "scale=3; "$7"/1000000"}' | ${pkgs.bc}/bin/bc -l` GB"
            echo "${theme.fg}"
        '';
        executable = true;
    });
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
                    if [[ ! -z "$cur_text" ]]
                    then
                        if [[ $cur_color = $prev_color ]]
                        then
                            echo -n "%{B$prev_color}%{F${theme.bg}}%{T4} %{T-}$cur_text"
                        else
                            echo -n "%{B$prev_color}%{F$cur_color}%{T4} %{T-}%{B$cur_color}$cur_text"
                        fi
                    fi
                done
                sleep 1
                echo " "
            done'';
        executable = true;
    });
}
