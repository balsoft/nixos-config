{pkgs, lib, ...}:
let
thm = {
	bg = "#31363b";
	fg = "#efefef";
	bd = "#3caae4";
};
genIni = lib.generators.toINI {
  mkKeyValue = key: value:
    let
      value' =
        if builtins.isBool value then (if value then "true" else "false")
        else if (builtins.isString value && key != "include-file") then value
        else builtins.toString value;
    in
      "${key}=${value'}";
};
in
rec {
	programs.home-manager = {
		enable = true;
		path = https://github.com/rycee/home-manager/archive/master.tar.gz;
	};
	
	programs.zsh = {
		enable = true;
		enableAutosuggestions = true;
		enableCompletion = programs.zsh.enableAutosuggestions;
		oh-my-zsh = {
			enable = true;
			theme = "agnoster";
			plugins = [
				"git"
#				"compleat"
				"dirhistory"
			];
		};
		initExtra = ''
cmdignore=(htop tmux top vim)
function active_window_id () {
	if [[ -n $DISPLAY ]] ; then
	    ${pkgs.xorg.xprop}/bin/xprop -root _NET_ACTIVE_WINDOW | awk '{print $5}'
	    return
	fi
	echo nowindowid
}

# end and compare timer, notify-send if needed
function notifyosd-precmd() {
	retval=$?
	if [ ! -z "$cmd" ]; then
	    cmd_end=`date +%s`
	    ((cmd_time=$cmd_end - $cmd_start))
	fi
	if [ $retval -eq 0 ]; then
	    cmdstat="✓"
	else
	    cmdstat="✘"
	    fi
	if [ ! -z "$cmd" -a ! $term_window = $(active_window_id) ]; then
		${pkgs.libnotify}/bin/notify-send -i utilities-terminal -u low "$cmdstat $cmd" "in `date -u -d @$cmd_time +'%T'`"
	fi
	unset cmd
}

# make sure this plays nicely with any existing precmd
precmd_functions+=( notifyosd-precmd )

# get command name and start the timer
function notifyosd-preexec() {
    cmd=$1
    term_window=$(active_window_id)
    cmd_start=`date +%s`
}

# make sure this plays nicely with any existing preexec
preexec_functions+=( notifyosd-preexec )
XDG_DATA_DIRS=$XDG_DATA_DIRS:$GSETTINGS_SCHEMAS_PATH
		'';
	};

	gtk = {
		enable = true;
		iconTheme = 
		{
			name = "breeze-dark";
			package = pkgs.breeze-icons;
		};
		theme = 
		{
			name = "Breeze-Dark";
			package = pkgs.breeze-gtk;
		};
		
	};

	xsession.windowManager.i3 = {
		enable = true;
		config = {
			bars = [];
			fonts = [ "RobotoMono 9" ];
			colors = rec{
				background = thm.bg;
				unfocused = {
					text = "#555555";
					border = thm.bg;
					background = thm.bg;
					childBorder = thm.bg;
					indicator = thm.fg;
				};
				focused = unfocused // {
                    text = thm.fg;
				};
			};
			modifier = "Mod4";
			window = {
				border = 0;
			};
			startup = [
				{ command = "${pkgs.albert}/bin/albert"; always = true; }
				{ command = "${pkgs.tdesktop}/bin/telegram-desktop"; workspace = "9";}
				{ command = "${pkgs.kmix}/bin/kmix"; }
				{ command = "${pkgs.plasma-workspace}/bin/klipper"; }
				{ command = "${pkgs.xorg.setxkbmap}/bin/setxkbmap -layout '${home.keyboard.layout}' -options '${builtins.concatStringsSep "," home.keyboard.options}'"; }
				{ command = "polybar -r top"; }
				{ command = "dunst"; }
			];
			keybindings = let modifier = xsession.windowManager.i3.config.modifier;
			in ({
				"${modifier}+q" = "kill";
				"${modifier}+Return" = "exec ${pkgs.kdeApplications.konsole}/bin/konsole";
				"${modifier}+t" = "exec ${pkgs.tdesktop}/bin/telegram-desktop";
				"${modifier}+l" = "layout toggle";
				"${modifier}+Left" = "focus left";
				"${modifier}+Right" = "focus right";
				"${modifier}+Up" = "focus up";
				"${modifier}+Down" = "focus down";
				"${modifier}+Shift+Up" = "move up";
				"${modifier}+Shift+Down" = "move down";
				"${modifier}+Shift+Right" = "move right";
				"${modifier}+Shift+Left" = "move left";
				"${modifier}+f" = "fullscreen toggle";
				"${modifier}+r" = "mode resize";
				"${modifier}+b" = "exec ${pkgs.falkon}/bin/falkon";
			} // builtins.listToAttrs (
				builtins.genList (x: {name = "${modifier}+${toString x}"; value = "workspace ${toString x}";}) 10
			) // builtins.listToAttrs (
				builtins.genList (x: {name = "${modifier}+Shift+${toString x}"; value = "move container to workspace ${toString x}";}) 10
			));
		};
	};

	services.kdeconnect = {
		enable = true;
		indicator = true;
	};
	
	services.polybar = {
		enable = true;
		package = pkgs.polybar.override {
			i3Support = true;
			iwSupport = true;
			
		};
		config = {
			"bar/top" = {
				#font-0 = "PowerlineSymbols:size=15;3";
				font-0 = "Roboto Mono:size=11;1";
				font-1 = "Noto Sans:size=11;1";
				width = "100%";
				height = "3%";
				radius = 0;
				background = thm.bg;
				foreground = thm.fg;
				modules-left = "i3";
				modules-center = "date";
				modules-right = "cpu pipe ram pipe battery";
				tray-position = "right";
			};
			
			"module/date" = {
				type = "internal/date";
				internal = 5;
				date = "%d.%m.%y";
				time = "%H:%M";
				label = "%time%  %date%";
			};

			"module/i3" = {
				type = "internal/i3";
				label-focused-foreground = thm.bd;		
			};

			"module/pipe" = {
				type = "custom/text";
				content = " | ";		
			};

			"module/battery" = {
				type = "internal/battery";
				label-charging = "CHR: %percentage%% (%time%)";
				label-discharging = "BAT: %percentage%% (%time%)";
				label-full = "FULL";
			};

			"module/cpu" = {
				type = "internal/cpu";
				label = " CPU: %percentage%%";
			};

			"module/ram" = {
				type = "internal/memory";
				label = "RAM: %percentage_used%%";
			};
		};
		script = "";
	};

	services.dunst = {
		enable = true;
		iconTheme = {
			name = "breeze-dark";
			package = pkgs.breeze-icons;
		};
		settings = {
			global = {
				geometry = "300x5-30+50";
				transparency = 10;
				frame_color = thm.bd;
				font = "Roboto Mono 13";
				padding = 15;
				horizontal_padding = 17;
			};
			
			urgency_low = {
				background = thm.bg;
				foreground = thm.fg;
				timeout = 5;
			};
			
			urgency_normal = {
				background = "#2b3034";
				foreground = thm.fg;
				timeout = 10;
			};
			
			urgency_critical = {
				background = thm.fg;
				foreground = thm.bg;
				timeout = 15;
			};
		};
	};
	
	home.packages = with pkgs; [
		# Internet
		wget
		curl
		chromium
		# IDE
		vscode
		geany
		kdevelop
		jetbrains.pycharm-community
		kate
		# Messaging
		tdesktop
		telepathy_haze
		telepathy_idle
		libnotify
		quaternion
		# Audio/Video
		vlc
		kdenlive
		google-play-music-desktop-player
		# Tools
		zip
		unrar
		mc
		nox
		pinta
		wine
		kolourpaint
		krita
		ktorrent
		wireshark
		#wpsoffice
		micro
		cmake
		gnumake
		gcc
		gdb
		python3
		qalculate-gtk
		#typora
		libreoffice-fresh
		qt5ct
		breeze-qt5
		units
		goldendict
	];
	home.keyboard = {
		options = ["grp:caps_toggle" "grp_led:caps"];
		layout = "us,ru";
	};
	home.sessionVariables = {
		EDITOR = "micro";
		QT_QPA_PLATFORMTHEME = "qt5ct";
		GTK_THEME = "Breeze-Dark";
	};
	xdg = {
		enable = true;
		configFile = { 
			"libinput-gestures.conf".text = ''
				gesture swipe down 4 xdotool key "Alt+quoteleft"
				gesture swipe up 4 xdotool key "Alt+asciitilde"
				gesture pinch in 2 xdotool key "Ctrl+F8"
				gesture pinch out 2 xdotool key "Ctrl+F8"
				gesture swipe right 3 xdotool key "Ctrl+Tab"
				gesture swipe left 3 xdotool key "Ctrl+Shift+Tab"
				gesture swipe up 3 xdotool key "Pause"
				gesture swipe down 3 xdotool key "Pause"
			'';
			"albert/albert.conf".text = genIni {
				General = {
					frontendId = "org.albert.frontend.boxmodel.qml";
					hotkey = "Meta+Space";
					showTray = false;
					terminal = "${pkgs.konsole}/bin/konsole -e";
				};
				"org.albert.extension.applications".enabled = true;
				"org.albert.extension.calculator".enabled = true;
				"org.albert.extension.files" = {
					enabled = true;
					filters = "application/*, image/*";
					fuzzy = true;		
				};
				"org.albert.extension.python" = {
					enabled = true;
					enabled_modules = "Python, Wikipedia, GoogleTranslate, Kill, Locate, Units, Currency, GoldenDict";
				};
				"org.albert.extension.ssh".enabled = true;
				"org.albert.extension.system" = {
						enabled = true;
						logout = "i3-msg exit";
						reboot = "reboot";
						shutdown = "shutdown now";		
				};
				"org.albert.extension.terminal".enabled = true;
				"org.albert.extension.websearch".enabled = true;
				"org.albert.frontend.boxmodel.qml" = {
					enabled = true;
					alwaysOnTop=true;
					clearOnHide=false;
					hideOnClose=false;
					hideOnFocusLoss=true;
					showCentered=true;
					stylePath="${pkgs.albert}/share/albert/org.albert.frontend.boxmodel.qml/styles/BoxModel/MainComponent.qml";
					windowPosition="@Point(299 13)";
				};
			};
			"albert/org.albert.frontend.boxmodel.qml/style_properties.ini".text = genIni {
				BoxModel = {
					animation_duration=0;
					background_color="\"@Variant(\\0\\0\\0\\x43\\x1\\xff\\xff\\x31\\x31\\x36\\x36;;\\0\\0)\"";
					border_color="\"@Variant(\\0\\0\\0\\x43\\x1\\xff\\xff==\\xae\\xae\\xe9\\xe9\\0\\0)\"";
					border_size=1;
					icon_size=10;
					input_fontsize=10;
					item_description_fontsize=8;
					item_title_fontsize=9;
					max_items=10;
					padding=6;
					radius=2;
					settingsbutton_size=10;
					spacing=5;
					window_width=300;
				};
			};
		};
	};
	news.display = "silent";
	programs.command-not-found.enable = true;
}
