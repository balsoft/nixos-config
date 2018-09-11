{ pkgs, secret, theme, config?{city-id="513378";}, ... }:
rec {
    weather = pkgs.writeTextFile { name="weather.sh"; text=''
        #!${pkgs.bash}/bin/bash
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
        weather=$(${pkgs.curl}/bin/curl -sf "http://api.openweathermap.org/data/2.5/weather?APPID=${secret.owm-key}&id=${config.city-id}&units=metric")
        if [ ! -z "$weather" ]; then
            weather_temp=$(echo "$weather" | ${pkgs.jq}/bin/jq ".main.temp" | cut -d "." -f 1)
            weather_icon=$(echo "$weather" | ${pkgs.jq}/bin/jq -r ".weather[0].icon")
            if [ $weather_temp -lt 0 ]
            then
                color=${theme.blue}
            else
                color=${theme.green}
            fi
            echo "%{F${theme.bg} A:${pkgs.konsole}/bin/konsole --noclose --fullscreen -e curl wttr.in/Ozery:}%{T2}$(get_icon "$weather_icon")%{T-}" "$weather_temp°%{A-}"
            echo $color
        fi''; executable = true;};
    email = pkgs.writeTextFile { name="email.py"; text=''
        #!${pkgs.python3}/bin/python3
        import imaplib
        obj = imaplib.IMAP4_SSL('imap.gmail.com', 993)
        obj.login("${secret.gmail.user}", "${secret.gmail.password}")
        obj.select()
        l = len(obj.search(None, 'unseen')[1][0].split())
        print("%{F${theme.bg} }%{A:${pkgs.chromium}/bin/chromium https\://inbox.google.com:}%{T3}📧%{T-} "+str(l)+"%{A-}")
        print("${theme.red}" if l != 0 else "${theme.green}")
        ''; 
        executable = true;};
    now = pkgs.writeTextFile { name="now.sh"; text=''
        #!${pkgs.bash}/bin/bash
        echo -n "`date +'%%{F${theme.bg}} %%{T3}⌚%%{T-} %H:%M %%{T3}📆%%{T-} %A, %d'`"
        echo -n "%{F${theme.red}}" 
        echo "`${pkgs.gcalcli}/bin/gcalcli --nocolor agenda 'now' 'now+1s' | head -2 | tail -1 | awk '{$1=""; $2=""; $3=""; $4=""; print}' | tr -s ' '`"
        echo "${theme.fg}"'';
        executable = true;};
    next = pkgs.writeTextFile {name="next.sh"; text=''
        #!${pkgs.bash}/bin/bash
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
    ''; executable = true;};

    text_and_color_for_powerline = (arr: ''
            ${builtins.concatStringsSep "\n" (builtins.genList (i: ''
            readarray -t arr${builtins.toString i} <<< "`${builtins.elemAt arr i}`"
            text[${builtins.toString i}]=''${arr${builtins.toString i}[0]}
            color[${builtins.toString i}]=''${arr${builtins.toString i}[1]}
            '') (builtins.length arr))}
            color[${builtins.toString (builtins.length arr)}]="${theme.bg}"
    '');

    left_side = (arr: pkgs.writeTextFile {
        name = "polybar-left-side.sh";
        text = ''
            #!${pkgs.bash}/bin/bash
            ${text_and_color_for_powerline arr}
            for index in `seq 0 ${builtins.toString(builtins.length arr)}`
            do
                cur_color=''${color[index]}
                cur_text=''${text[index]}
                next_color=''${color[`expr $index + 1`]}
                echo -n "%{B$cur_color}$cur_text%{B$next_color}%{F$cur_color}%{T4} %{T-}"
            done'';
        executable = true;
    });

    network = pkgs.writeTextFile {
        name = "network.sh";
        text = ''
            #!${pkgs.bash}/bin/bash
            WIFI="`nmcli d w | grep "\*" | awk '{print "%{F${theme.bg}}%{T5}"$8"%{T-}" $2}'`"
            if [[ `wc -l <<< "$WIFI"` -eq 0 ]]
            then
                echo "W down"
                echo "${theme.red}"
            else
                echo "$WIFI"
                echo "${theme.green}"
            fi
        '';
        executable = true;
    };

    battery = pkgs.writeTextFile {
        name = "battery.sh";
        text = ''
            #!${pkgs.bash}/bin/bash
            BATTERY="`${pkgs.acpi}/bin/acpi -b`"
            STATUS=`awk '{print $3}' <<< "$BATTERY" | tr -d ','`
            CHARGE=`awk '{print $4}' <<< "$BATTERY" | tr -d ',%'`
            TIME=`awk '{print $5}' <<< "$BATTERY"`
            echo -n "%{F${theme.bg}}"
            case "$STATUS" in
                Full) echo "%{T3}⚡ %{T-}FULL"; echo "${theme.blue}";;
                Charging) echo "%{T3}⚡ %{T-}$CHARGE% ($TIME)"; echo "${theme.green}";;
                Discharging)
                    echo "%{T3}🔋%{T-} $CHARGE% ($TIME)";
                    if [[ $CHARGE -gt 10 ]]
                    then
                        echo "${theme.fg}"
                    else
                        echo "${theme.red}"
                    fi
                ;;
            esac
        '';
        executable = true;
    };
    status = pkgs.writeTextFile {
        name = "status.sh";
        text = ''
            echo -n "%{F${theme.bg}}"
            echo -n "`whoami`@`hostname` "
            echo -n "`top -b -n2 -p 1 | fgrep "Cpu(s)" | tail -1 | awk -F'id,' -v prefix="$prefix" '{ split($1, vs, ","); v=vs[length(vs)]; sub("%", "", v); printf "%s%.1f%%\n", prefix, 100 - v }'` "
            echo -n $(${pkgs.bc}/bin/bc -l <<< "scale=2; `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq`/1000000")
            echo -n "GHz "
            echo -n "$((`cat /sys/class/thermal/thermal_zone*/temp | sort | tail -1`/1000))° "
            echo "`free | tail -2 | head -1 | awk '{print "scale=3; "$7"/1000000"}' | ${pkgs.bc}/bin/bc -l` GB"
            echo "${theme.fg}"
        '';
        executable = true;
    };
    right_side = (arr: pkgs.writeTextFile {
        name = "polybar-right-side.sh";
        text = ''
            #!${pkgs.bash}/bin/bash
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
            done'';
        executable = true;
    });
}
