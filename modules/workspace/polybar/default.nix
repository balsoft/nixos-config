{pkgs, config, lib, ...}:
with lib;
with config.deviceSpecific;
let 
    thm = config.themes.colors;
    theme = thm;
    secret = config.secrets;
    device = config.device;
    text_and_color_for_powerline = (arr: ''
      ${builtins.concatStringsSep "\n" (builtins.genList (i: ''
      readarray -t arr${builtins.toString i} < "/tmp/bar/${(builtins.elemAt arr i).name}"
      text[${builtins.toString i}]=''${arr${builtins.toString i}[0]}
      color[${builtins.toString i}]=''${arr${builtins.toString i}[1]}
      '') (builtins.length arr))}
      color[${builtins.toString (builtins.length arr)}]="${theme.bg}"
      text[${builtins.toString (builtins.length arr)}]=""
      color[${builtins.toString (builtins.length arr + 1)}]="${theme.bg}"
      text[${builtins.toString (builtins.length arr + 1)}]=""
      '');
    start_scripts = (arr: ''mkdir -p /tmp/bar; { ${builtins.concatStringsSep "\n" (map (x: "touch /tmp/bar/${x.name}; ${x} &") arr)} }'');

  left_side = (arr: pkgs.writeTextFile {
    name = "polybar-left-side";
    text = ''
      #!${pkgs.bash}/bin/bash
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
        ${pkgs.inotifyTools}/bin/inotifywait /tmp/bar -e moved_to -qq
      done'';
    executable = true;
  });
    right_side = (arr: pkgs.writeTextFile {
    name = "polybar-right-side";
    text = ''
      #!${pkgs.bash}/bin/bash
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
        ${pkgs.inotifyTools}/bin/inotifywait /tmp/bar -e moved_to -qq
      done'';
    executable = true;
  });
    wrapScriptToLoop = interval: script:
  pkgs.writeTextFile {
    name = "${script.name}";
    executable = true;
    text = ''
            cd /tmp/bar
            while true
            do
                touch ${script.name} ${script.name}.new
                ${script} 2>/dev/null > ${script.name}.new
                if [[ -e ${script.name}.new ]] && [[ `wc -l < ${script.name}.new` -gt 1 ]]
                then
                    mv ${script.name}.new ${script.name}
                fi
                sleep ${toString interval}
            done
        '';
 };

 weather = { color_good ? theme.green, color_rain ? theme.orange, color_cold ? theme.blue, city, city-id, owm-key ? secret.owm-key, terminal ? "${pkgs.konsole}/bin/konsole --noclose --fullscreen -e", interval ? 60 }: 
    wrapScriptToLoop interval (pkgs.writeTextFile { 
      name = "weather"; 
      text = ''
            #!${pkgs.bash}/bin/bash
            ping -c 1 api.openweathermap.org &> /dev/null || exit 1 
            get_icon() {
              case $1 in 
                01*) icon=;;
                02*) icon=;;
                03*) icon=;;
                04*) icon=;;
                09*) icon=;;
                10*) icon=;;
                11*) icon=;;
                13*) icon=;;
                50*) icon=;;
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
                  color=${color_rain} ,
                fi
                echo $color
            }
            weather=$(${pkgs.curl}/bin/curl -sf "http://api.openweathermap.org/data/2.5/weather?APPID=${owm-key}&id=${city-id}&units=metric")
            if [ ! -z "$weather" ]; then
              weather_temp=$(echo "$weather" | ${pkgs.jq}/bin/jq ".main.temp" | cut -d "." -f 1)
              weather_icon=$(echo "$weather" | ${pkgs.jq}/bin/jq -r ".weather[0].icon")
        
              echo "%{F${theme.fg} A:${terminal} "curl wttr.in/${city}":}%{T2}$(get_icon "$weather_icon")%{T-}" "$weather_temp°%{A-}"
              echo $(get_color $weather_temp $weather_icon)
            fi''; 
    executable = true;
  });
  email = { color_unread ? theme.orange, color_nounread ? theme.alt, user, password, interval ? 10}: 
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
        print("%{F${theme.fg} }%{A:i3-msg workspace :}%{T6}%{T-} "+str(l)+"%{A-}")
        print("${color_unread}" if l != 0 else "${color_nounread}")
    except:
        pass'';
    executable = true;
  });
  time = {interval ? 10}:
  wrapScriptToLoop interval (pkgs.writeTextFile { 
    name = "time";
    text = ''
    #!${pkgs.bash}/bin/bash 
    echo "`date +'%%{F${theme.fg}}%%{T6}%%{T-} %H:%M %%{T6}%%{T-} %A, %d'`"
    echo "${theme.alt}"
    '';
    executable = true;
  });
  now = {interval ? 60}: 
  wrapScriptToLoop interval (pkgs.writeTextFile { 
    name = "now";
    text = ''
    #!${pkgs.bash}/bin/bash
    ping -c 1 calendar.google.com &> /dev/null || exit 1 
    echo -n "%{F${theme.fg}}"
    echo $(PYTHONIOENCODING=utf8 ${pkgs.gcalcli}/bin/gcalcli --nocolor agenda 'now' 'now+1s' --tsv | head -1 | awk '{$1=""; $2=""; $3=""; $4=""; print}' | tr -s ' ')
    echo "${theme.alt}"
    '';
    executable = true;
  });
  next = {interval ? 60}: 
  wrapScriptToLoop interval (pkgs.writeTextFile {
    name = "next";
    text = ''
      #!${pkgs.bash}/bin/bash
      ping -c 1 calendar.google.com &> /dev/null || exit 1 
      AGENDA_NEXT="`PYTHONIOENCODING=utf8 ${pkgs.gcalcli}/bin/gcalcli --nocolor search "*" 'now' 'now+6d' --nostarted --tsv | head -1`"
      DATE="`awk '{print $1 " " $2}' <<< "$AGENDA_NEXT"`"
      echo -n "%{F${theme.fg}}%{T6}%{T-} "
      if [[ `date -d "$DATE" +'%u'` -eq `date +'%u'` ]]
      then
        echo -n `date -d "$DATE" +'%H:%M'`
      else
        echo -n `date -d "$DATE" +'%H:%M %A'`
      fi
      if [[ $((`date -d "$DATE" +%s`-`date +%s`)) -lt 1800 ]]
      then
        color=${theme.orange}
      else
        color=${theme.blue}
      fi
      echo ": `awk '{$1=""; $2=""; $3=""; $4=""; print}' <<< "$AGENDA_NEXT" | tr -s " " | tr -s " "`"
      echo $color
    ''; 
    executable = true;
  });


  network = {color_down ? theme.orange, color_up ? theme.blue, interval ? 5}: 
  wrapScriptToLoop interval (pkgs.writeTextFile {
    name = "network";
    text = ''
      #!${pkgs.bash}/bin/bash
      WIFI="`${pkgs.iw}/bin/iw wlan0 info | grep ssid | cut -f2 -d' '`"
      if [[ `wc -c <<< "$WIFI"` -lt 2 ]]
      then
        echo "%{F${theme.fg}}%{A:${pkgs.wpa_supplicant_gui}/bin/wpa_gui:}%{T6}%{T-} %{A-}"
        echo "${color_down}"
      else
        echo "%{F${theme.fg}}%{A:${pkgs.wpa_supplicant_gui}/bin/wpa_gui:}%{T6}%{T-} $WIFI %{A-}"
        echo "${color_up}"
      fi
    '';
    executable = true;
  });

  battery = {color_charging ? theme.green, color_discharging ? theme.alt, color_full ? theme.blue, color_low ? theme.orange, low_threshold ? 10, interval ? 5}: 
  wrapScriptToLoop interval (pkgs.writeTextFile {
    name = "battery";
    text = ''
      #!${pkgs.bash}/bin/bash
      BATTERY="`${pkgs.acpi}/bin/acpi -b | grep --invert-match unavailable | head -1`"
      STATUS=`awk -F'[,:] ' '{print $2}' <<< "$BATTERY"`
      CHARGE=`awk -F'[,%] ' '{print $2}' <<< "$BATTERY" | tr -d "%"`
      TIME=`awk -F', ' '{print $3}' <<< "$BATTERY" | cut -d " " -f 1`
      case "$STATUS" in
        Full) ;& "Not charging") text="%{T6}%{T-} FULL"; color="${color_full}";;
        Charging) text="%{T3}%{T-} $CHARGE% ($TIME)"; color="${color_charging}";;
        Discharging)
          if [[ $CHARGE -gt ${builtins.toString low_threshold} ]]
          then
            text="%{T6}%{T-} $CHARGE% ($TIME)";  
            color="${color_discharging}"
          else
            text="%{T6}%{T-} $CHARGE% ($TIME)";  
            color="${color_low}"
          fi
        ;;
      esac
      echo "%{F${theme.fg}}%{A:${pkgs.gnome3.gnome-power-manager}/bin/gnome-power-statistics &:}$text%{A-}"
      echo $color
    '';
    executable = true;
  });
  sound = {}: pkgs.writeTextFile rec {
    name = "sound";
    text = ''
      cd /tmp/bar
      stdbuf -o0 pactl subscribe | stdbuf -o0 grep "sink" | stdbuf -o0 grep --invert "input" > /tmp/${name}_events &
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
            color=${theme.alt}
          else
            if [[ $volume -lt 66 ]]
            then
              icon=""
              color=${theme.alt}
            else
              icon=""
              color=${theme.orange}
            fi
          fi
        fi
        echo "%{A:${pkgs.lxqt.pavucontrol-qt}/bin/pavucontrol-qt:}%{T6}%{F${theme.fg}}$icon%{T-}$volume$end%{A}" > ${name}.new
        echo $color >> ${name}.new
        mv ${name}.new ${name}
        ${pkgs.inotifyTools}/bin/inotifywait /tmp/${name}_events -qq
      done'';
    executable = true;
  };
  status = { interval ? 5 }: 
  wrapScriptToLoop interval (pkgs.writeTextFile {
    name = "status";
    text = ''
      echo -n "%{F${theme.fg}}"
      echo -n "%{T6}%{T-} "
      echo -n "`top -b -n2 -p 1 | fgrep "Cpu(s)" | tail -1 | awk -F'id,' -v prefix="$prefix" '{ split($1, vs, ","); v=vs[length(vs)]; sub("%", "", v); printf "%s%.1f%%\n", prefix, 100 - v }'` "
      echo -n $(${pkgs.bc}/bin/bc -l <<< "scale=2; `cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq|sort|head -1`/1000000")
      echo -n "GHz "
      echo -n "$((`cat /sys/class/thermal/thermal_zone*/temp | sort | tail -1`/1000))° "
      echo "`free | tail -2 | head -1 | awk '{print "scale=3; "$7"/1000000"}' | ${pkgs.bc}/bin/bc -l`GB"
      echo "${theme.alt}"
    '';
    executable = true;
  });
  brightness = { device }: pkgs.writeTextFile rec {
    name = "brightness";
    text = ''
      cd /tmp/bar
      stdbuf -o0 ${pkgs.acpid}/bin/acpi_listen | stdbuf -o0 grep -e "video/" > ${name}-events &
      while true
      do
        LIGHT=`${pkgs.light}/bin/light | cut -f 1 -d '.'`
        echo -n "%{F${theme.fg}}" > ${name}.new
        color=${theme.alt}
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
        ${if device == "ASUS-Laptop" then ''
        if [[ `cat /sys/devices/platform/asus-nb-wmi/als_enable` -eq 1 ]] 
        then
          icon=""
          color="${theme.blue}"
        fi
        '' else ""}
        echo "%{A1:light -S 0:}%{A4:light -U 5:}%{A5:light -A 5:}$icon $LIGHT%{A}%{A}%{A}" >> ${name}.new
        echo $color >> ${name}.new
        mv ${name}.new ${name}
        ${pkgs.inotifyTools}/bin/inotifywait ${name}-events ${if device == "ASUS-Laptop" then "/sys/devices/platform/asus-nb-wmi/als_enable" else ""} -qq
        sleep 0.2
      done
    '';
    executable = true;
  };
  polybar_left = [ 
    (weather { city-id = "513378"; city = "Ozery"; }) 
    (time {})
    (now {})
    (next {}) 
    (email { user = secret.gmail.user; password = secret.gmail.password; }) 
  ];

  polybar_right = [
    (status {})
  ] ++ (lib.optionals isLaptop [
    (brightness { inherit device; })
  ]) ++ (lib.optionals (device != "Prestigio-Laptop") [
    (sound {})
  ]) ++ (if (isLaptop && device != "Prestigio-Laptop") then [
    (battery {})
  ] else []) ++ [
    (network {})
  ];

in
{
  home-manager.users.balsoft = rec {
    xsession.windowManager.i3.config.startup = lib.mkBefore
    [
      { command = "kill -9 $(pgrep polybar)"; always = true; }
      {
        command = ''exec ${
          pkgs.writeTextFile {
            name = "start_scripts";
            text = start_scripts (polybar_left ++ polybar_right);
            executable = true;
          }
        }'';
      }
    ];
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      i3Support = true;
      alsaSupport = false;
      nlSupport = false;
    };
    config = {
      "bar/top" = {
        font-0 = "Roboto Mono for Powerline:size=" + (if smallScreen then "8;1" else "11;2");
        font-3 = "Roboto Mono for Powerline:size=" + (if smallScreen then "18;4" else "24;5");
        font-1 = "Noto Sans Symbols2:size=15;4";
        font-2 = "Noto Emoji:size=" + (if smallScreen then "8;1" else "11;2");
        font-4 = "Unifont:size=" + (if smallScreen then "8;1" else "11;2");
        font-5 = "Material Icons:size=" + (if smallScreen then "10;2" else "16;4");
        width = "100%";
        height = if smallScreen then "19px" else "25px";
        radius = 0;
        background = thm.bg;
        foreground = thm.fg;
        modules-left = "left_side";
        modules-center = "i3";
        modules-right = "right_side";
        tray-position = "none";
        monitor = "\${env:MONITOR:}";
      };
      "module/i3" = {
        type = "internal/i3";
        label-focused-foreground = thm.blue;
        label-urgent-foreground = thm.orange;
        pin-workspaces = true;
      };

      "module/left_side" = {
        type = "custom/script";
        exec = toString (left_side (polybar_left));
        tail = true;
      };

      "module/right_side" = {
        type = "custom/script";
        exec = toString (right_side (polybar_right));
        tail = true;
      };
    };
    script = "";
  };
  programs.autorandr.enable = true;
  programs.autorandr.hooks =
  {
    preswitch.polybar = "kill -9 $(pgrep polybar)";
    postswitch = {
      polybar = "for i in $(polybar -m | cut -d ':' -f 1); do MONITOR=$i polybar top & sleep 0.5; done";
    };
  };
    
};
}
