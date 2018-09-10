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
        DATE="`echo $AGENDA_NEXT | awk '{print $1 " " $2}'`"
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
        echo ": `echo "$AGENDA_NEXT" | awk '{print $3; print $4; print $5}'`"
        echo $color
    ''; executable = true;};
    left_side = pkgs.writeTextFile {
        name = "polybar-left-side.sh";
        text = ''
            #!${pkgs.bash}/bin/bash
            readarray -t weather <<< "`${weather}`"
            readarray -t now <<< "`${now}`"
            readarray -t next <<< "`${next}`"
            readarray -t email <<< "`${email}`"
            text=( "''${weather[0]}" "''${now[0]}" "''${next[0]}" "''${email[0]}" )
            color=( "''${weather[1]}" "''${now[1]}" "''${next[1]}" "''${email[1]}" )
            text[4]=""
            color[4]="${theme.bg}"
            for index in `seq 0 3`
            do
                cur_color=''${color[index]}
                cur_text=''${text[index]}
                next_color=''${color[`expr $index + 1`]}
                echo -n "%{B$cur_color}$cur_text%{B$next_color}%{F$cur_color}%{T4} %{T-}"
            done'';
        executable = true;
    };
}
