{ pkgs, secret, theme, ... }:
rec {
    wrapScriptToLoop = interval: script:
    pkgs.writeTextFile {
        name = "${script.name}";
        executable = true;
        text = ''
            while true
            do
                touch /tmp/${script.name} /tmp/${script.name}.new
                ${script} 2>/dev/null > /tmp/${script.name}.new
                if [[ -e /tmp/${script.name}.new ]] && [[ `wc -l < /tmp/${script.name}.new` -gt 1 ]]
                then
                    mv /tmp/${script.name}.new /tmp/${script.name}
                fi
                sleep ${toString interval}
            done
        '';
    };

    weather = { color_good ? theme.green, color_rain ? theme.red, color_cold ? theme.blue, city, city-id, owm-key ? secret.owm-key, terminal ? "${pkgs.konsole}/bin/konsole --noclose --fullscreen -e", interval ? 60 }: 
    wrapScriptToLoop interval (pkgs.writeTextFile { 
        name = "bar-weather"; 
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
        name = "bar-email";
        text = ''
        #!${pkgs.python3}/bin/python3
        import imaplib
        try:
            obj = imaplib.IMAP4_SSL('imap.gmail.com', 993)
            obj.login("${user}", "${password}")
            obj.select()
            l = len(obj.search(None, 'unseen')[1][0].split())
            print("%{F${theme.bg} }%{A:i3-msg workspace :}%{T6}%{T-} "+str(l)+"%{A-}")
            print("${color_unread}" if l != 0 else "${color_nounread}")
        except:
            pass'';
        executable = true;
    });
    time = {interval ? 10}:
    wrapScriptToLoop interval (pkgs.writeTextFile { 
        name = "bar-time";
        text = ''
        #!${pkgs.bash}/bin/bash 
        echo "`date +'%%{F${theme.bg}}%%{T6}%%{T-} %H:%M %%{T6}%%{T-} %A, %d'`"
        echo "${theme.fg}"
        '';
        executable = true;
    });
    now = {interval ? 60}: 
    wrapScriptToLoop interval (pkgs.writeTextFile { 
        name = "bar-now";
        text = ''
        #!${pkgs.bash}/bin/bash
        ping -c 1 calendar.google.com &> /dev/null || exit 1 
        echo $(PYTHONIOENCODING=utf8 ${pkgs.gcalcli}/bin/gcalcli --nocolor agenda 'now' 'now+1s' --tsv | head -1 | awk '{$1=""; $2=""; $3=""; $4=""; print}' | tr -s ' ')
        echo "${theme.fg}"
        '';
        executable = true;
    });
    next = {interval ? 60}: 
    wrapScriptToLoop interval (pkgs.writeTextFile {
        name = "bar-next";
        text = ''
            #!${pkgs.bash}/bin/bash
            ping -c 1 calendar.google.com &> /dev/null || exit 1 
            AGENDA_NEXT="`PYTHONIOENCODING=utf8 ${pkgs.gcalcli}/bin/gcalcli --nocolor search "*" 'now' 'now+6d' --nostarted --tsv | head -1`"
            DATE="`awk '{print $1 " " $2}' <<< "$AGENDA_NEXT"`"
            echo -n "%{F${theme.bg}}%{T6}%{T-} "
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
            echo ": `awk '{$1=""; $2=""; $3=""; $4=""; print}' <<< "$AGENDA_NEXT" | tr -s " " | tr -s " "`"
            echo $color
        ''; 
        executable = true;
    });

    text_and_color_for_powerline = (arr: ''
            ${builtins.concatStringsSep "\n" (builtins.genList (i: ''
            readarray -t arr${builtins.toString i} < "/tmp/${(builtins.elemAt arr i).name}"
            text[${builtins.toString i}]=''${arr${builtins.toString i}[0]}
            color[${builtins.toString i}]=''${arr${builtins.toString i}[1]}
            '') (builtins.length arr))}
            color[${builtins.toString (builtins.length arr)}]="${theme.bg}"
            text[${builtins.toString (builtins.length arr)}]=""
            color[${builtins.toString (builtins.length arr + 1)}]="${theme.bg}"
            text[${builtins.toString (builtins.length arr + 1)}]=""
    '');

    start_scripts = (arr: builtins.concatStringsSep "\n" (map (x: "touch /tmp/${x.name}; ${x} &") arr));

    left_side = (arr: pkgs.writeTextFile {
        name = "polybar-left-side";
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
                    next_text=''${text[`expr $index + 1`]}
                    next_color=''${color[`expr $index + 1`]}
                    if [[ -z "''${cur_text// }" ]]
                    then
                        continue
                    fi
                    if [[ -z "''${next_text// }" ]]
                    then
                        next_color=''${color[`expr $index + 2`]}
                    fi
                    if [[ $cur_color = $next_color ]]
                    then 
                        echo -n "%{B$cur_color}$cur_text%{B$next_color}%{F${theme.bg}}%{T4} %{T-}"
                    else
                        echo -n "%{B$cur_color}$cur_text%{B$next_color}%{F$cur_color}%{T4} %{T-}"
                    fi
                done
                echo
                ${pkgs.inotifyTools}/bin/inotifywait /tmp -e moved_to -qq
            done'';
        executable = true;
    });

    network = {color_down ? theme.red, color_up ? theme.green, interval ? 5}: 
    wrapScriptToLoop interval (pkgs.writeTextFile {
        name = "bar-network";
        text = ''
            #!${pkgs.bash}/bin/bash
            WIFI="`nmcli d w | grep "\*" | awk '{print $7"%"}'`"
            if [[ `wc -c <<< "$WIFI"` -lt 3 ]]
            then
                echo "%{F${theme.bg}}%{T6}%{T-} "
                echo "${color_down}"
            else
                echo "%{F${theme.bg}}%{T6}%{T-} $WIFI "
                echo "${color_up}"
            fi
        '';
        executable = true;
    });

    battery = {color_charging ? theme.green, color_discharging ? theme.fg, color_full ? theme.blue, color_low ? theme.red, low_threshold ? 10, interval ? 5}: 
    wrapScriptToLoop interval (pkgs.writeTextFile {
        name = "bar-battery";
        text = ''
            #!${pkgs.bash}/bin/bash
            BATTERY="`${pkgs.acpi}/bin/acpi -b`"
            STATUS=`awk -F', ' '{print $3}' <<< "$BATTERY" | tr -d ','`
            CHARGE=`awk -F', ' '{print $4}' <<< "$BATTERY" | tr -d ',%'`
            TIME=`awk -F', ' '{print $5}' <<< "$BATTERY"`
            echo -n "%{F${theme.bg}}"
            case "$STATUS" in
                Full) echo "%{T6}%{T-} FULL"; echo "${color_full}";;
                Charging) echo "%{T3}%{T-} $CHARGE% ($TIME)"; echo "${color_charging}";;
                Discharging) ;&
                Not charging)
                    if [[ $CHARGE -gt ${builtins.toString low_threshold} ]]
                    then
                        echo "%{T6}%{T-} $CHARGE% ($TIME)";    
                        echo "${color_discharging}"
                    else
                        echo "%{T6}%{T-} $CHARGE% ($TIME)";   
                        echo "${color_low}"
                    fi
                ;;
            esac
        '';
        executable = true;
    });
    sound = {}: pkgs.writeTextFile rec {
        name = "bar-sound";
        text = ''
            stdbuf -o0 pactl subscribe | stdbuf -o0 grep "sink" | stdbuf -o0 grep --invert  "input" > /tmp/${name}_events &
            while true
            do
                if [[ `${pkgs.pamixer}/bin/pamixer --get-mute` = "true" ]]
                then
                    volume=""
                    end=""
                    icon=""
                    color=${theme.blue}
                else
                    volume=`${pkgs.pamixer}/bin/pamixer --get-volume`
                    end="%"
                    if [[ $volume -lt 33 ]]
                     then
                        icon=""
                        color=${theme.fg}
                    else
                        if [[ $volume -lt 66 ]]
                        then
                            icon=""
                            color=${theme.green}
                        else
                            icon=""
                            color=${theme.red}
                        fi
                    fi
                fi
                echo "%{T6}%{F${theme.bg}}$icon%{T-}$volume$end" > /tmp/${name}.new
                echo $color >> /tmp/${name}.new
                mv /tmp/${name}.new /tmp/${name}
                ${pkgs.inotifyTools}/bin/inotifywait /tmp/${name}_events -qq
            done'';
        executable = true;
    };
    status = {interval ? 5}: 
    wrapScriptToLoop interval (pkgs.writeTextFile {
        name = "bar-status";
        text = ''
            echo -n "%{F${theme.bg}}"
            echo -n "%{T6}%{T-} "
            echo -n "`top -b -n2 -p 1 | fgrep "Cpu(s)" | tail -1 | awk -F'id,' -v prefix="$prefix" '{ split($1, vs, ","); v=vs[length(vs)]; sub("%", "", v); printf "%s%.1f%%\n", prefix, 100 - v }'` "
            echo -n $(${pkgs.bc}/bin/bc -l <<< "scale=2; `cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq|sort|head -1`/1000000")
            echo -n "GHz "
            echo -n "$((`cat /sys/class/thermal/thermal_zone*/temp | sort | tail -1`/1000))° "
            echo "`free | tail -2 | head -1 | awk '{print "scale=3; "$7"/1000000"}' | ${pkgs.bc}/bin/bc -l`GB"
            echo "${theme.fg}"
        '';
        executable = true;
    });
    right_side = (arr: pkgs.writeTextFile {
        name = "polybar-right-side";
        text = ''
            #!${pkgs.bash}/bin/bash
            ${start_scripts arr}
            while true
            do
                sleep 0.2
                ${text_and_color_for_powerline arr}
                for index in `seq 0 ${builtins.toString((builtins.length arr) - 1)}`
                do
                    cur_color=''${color[index]}
                    cur_text=''${text[index]}
                    prev_text=''${text[`expr $index - 1`]}
                    prev_color=''${color[`expr $index - 1`]}
                    if [[ -z "''${cur_text// }" ]]
                    then
                        continue
                    fi
                    if [[ -z "''${prev_text// }" ]]
                    then
                        prev_color=''${color[`expr $index - 2`]}
                    fi
                    if [[ $cur_color = $prev_color ]]
                    then
                        echo -n "%{B$prev_color}%{F${theme.bg}}%{T4} %{T-}$cur_text"
                    else
                        echo -n "%{B$prev_color}%{F$cur_color}%{T4} %{T-}%{B$cur_color}$cur_text"
                    fi
                done
                echo
                ${pkgs.inotifyTools}/bin/inotifywait /tmp -e moved_to -qq
            done'';
        executable = true;
    });
}
