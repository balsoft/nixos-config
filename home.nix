{pkgs, lib, ...}:
let
thm = {
	bg = "#31363b";
	fg = "#efefef";
	blue = "#3caae4";
	green = "#11d116";
	red = "#f67400";
};
term = "${pkgs.kdeApplications.konsole}/bin/konsole";

secret = import ./secret.nix;

scripts = import ./scripts {inherit pkgs; inherit secret; theme=thm;};
genIni = lib.generators.toINI {
  mkKeyValue = key: value:
    let
      mvalue =
        if builtins.isBool value then (if value then "true" else "false")
        else if (builtins.isString value && key != "include-file") then value
        else builtins.toString value;
    in
      "${key}=${mvalue}";
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
		initExtra = scripts.zshrc;
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
		config = rec {
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
				focusedInactive = unfocused;
				urgent = unfocused // {
                    text = thm.fg;
                    border = thm.red;
                    childBorder = thm.red;
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
				{ command = "pkill polybar; polybar top"; always = true; }
				{ command = "dunst"; }
			];
			keybindings =
			({
				"${modifier}+q" = "kill";
				"${modifier}+Return" = "exec ${term}";
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
				"${modifier}+c" = "exec ${pkgs.chromium}/bin/chromium";
			} // builtins.listToAttrs (
				builtins.genList (x: {name = "${modifier}+${toString x}"; value = "workspace ${toString x}";}) 10
			) // builtins.listToAttrs (
				builtins.genList (x: {name = "${modifier}+Shift+${toString x}"; value = "move container to workspace ${toString x}";}) 10
			));
			keycodebindings = {
				"232" = "exec echo $((`cat /sys/class/backlight/*/brightness`-10)) > /sys/class/backlight/*/brightness";
				"233" = "exec echo $((`cat /sys/class/backlight/*/brightness`-10)) > /sys/class/backlight/*/brightness";
				"107" = "exec scrot -e 'mv $f ~/Pictures && notify-send \"Screenshot saved as ~/Pictures/$f\"'";
			};
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
				font-0 = "Roboto Mono for Powerline:size=11;2";
				font-3 = "Roboto Mono for Powerline:size=17;4";
				font-1 = "Noto Sans Symbols2:size=15;4";
				font-2 = "Noto Emoji:size=11;2";
				width = "100%";
				height = "25px";
				radius = 0;
				background = thm.bg;
				foreground = thm.fg;
				modules-left = "left_side";
				modules-center = "i3";
				modules-right = "info cpu freq temp ram pipe battery pipe network";
				tray-position = "none";
			};
			"module/i3" = {
				type = "internal/i3";
				label-focused-foreground = thm.blue;
				label-urgent-foreground = thm.red;
			};

			"module/left_side" = {
				type = "custom/script";
				exec = scripts.polybar.left_side;
				interval = 30;
			};

			"module/pipe" = {
				type = "custom/text";
				content = " | ";
			};
			"module/plend" = {
                type = "custom/text";
                content = "%{B-}%{T4}%{T-} %{F-}";
			};
			
			"module/temp" = {
				type = "internal/temperature";
				warn-temperature = 70;
				label-warn-foreground = thm.red;
			};
			
			"module/battery" = {
				type = "internal/battery";
				format-charging-background = thm.bg;
				format-charging-foreground = thm.green;
				label-charging = "%{T3}⚡ %{T-}%percentage%% (%time%)";
				label-discharging = "%{T3}🔋%{T-} %percentage%% (%time%)";
				format-full-foreground = thm.blue;
				label-full = "%{T3}⚡ %{T-}FULL";
			};

			"module/cpu" = {
				type = "internal/cpu";
				label = " %percentage%%";
			};

			"module/freq" = {
                type = "custom/script";
                exec = "echo `cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq`/1000000 | ${pkgs.bc}/bin/bc -l";
                label = " %output:0:4%GHz ";
			};
			
			"module/ram" = {
				type = "internal/memory";
				label = " %gb_free%%{A}";
			};

			"module/network" = {
				type = "internal/network";
				interface = "wlan0";
				label-connected = "%{T3}📶%{T-} %essid%";
				format-connected-foreground = thm.green;
				format-disconnected-foreground = thm.red;
			};
			
			"module/diskicon" = {
                type = "custom/text";
                content = "%{T3}💾 %{T-}";
			};
			
			"module/info" = {
                type = "custom/script";
                exec = "echo `whoami`@`hostname`";
                label = "%{A1:${pkgs.ksysguard}/bin/ksysguard:}%{T3}💻%{T-} %output%";
			};
			
			"module/disk" = {
                type = "internal/fs";
                mount-0 = "/";
                mount-1 = "/home";
                label-mounted = "%mountpoint%: %free%";
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
				geometry = "500x5-30+50";
				transparency = 10;
				frame_color = thm.blue;
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

	programs.autorandr = {
		enable = true;
		hooks = {
			postswitch = {
				"notify-i3" = "${pkgs.i3}/bin/i3-msg restart";
			};
		};
		profiles = {
			"dacha" = {
				fingerprint = {
					eDP = "00ffffffffffff0030e4f60400000000001a01049522137803a1c59459578f27205054000000010101010101010101010101010101012e3680a070381f403020350058c210000019222480a070381f403020350058c210000019000000fd00283c43430e010a20202020202000000002000c47ff0a3c6e1c151f6e0000000052";
					HDMI-A-0 = "00ffffffffffff0006b3cc24010101011a1a010380351e78ea0565a756529c270f5054afcf80714f8180818fb30081409500a9408bc0023a801871382d40582c45000f282100001e000000fd00304b1e5311000a202020202020000000fc00565a3234390a20202020202020000000ff0047364c4d52533034383636390a018902031df14a900403011412051f1013230907078301000065030c001000023a801871382d40582c45000f282100001e011d8018711c1620582c25000f282100009e011d007251d01e206e2855000f282100001e8c0ad08a20e02d10103e96000f282100001800000000000000000000000000000000000000000000000000004a";
				};
				config = {
					eDP = {
                        enable = true;
                        primary = true;
                        position = "0x0";
                        mode = "1920x1080";
                        rate = "60.00";
					};
					HDMI-A-0 = {
						enable = true;
						position = "1920x0";
						mode = "1920x1080";
						rate = "60.00";
					};
				};
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
		wine
		kolourpaint
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
		qt5ct
		breeze-qt5
		units
		goldendict
		ksysguard
		scrot
		xclip
		abiword
		gnumeric
		kile
		texlive.combined.scheme-full
		gcalcli
	];

	programs.git = {
		enable = true;
		userEmail = "balsoft@yandex.ru";
		userName = "Александр Бантьев";

	};

	home.keyboard = {
		options = ["grp:caps_toggle,grp_led:caps"];
		layout = "us,ru";
	};
	home.sessionVariables = {
		EDITOR = "micro";
		QT_QPA_PLATFORMTHEME = "qt5ct";
		GTK_THEME = "Breeze-Dark";
		LESS = "-asrRix8";
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
					icon_size=15;
					input_fontsize=15;
					item_description_fontsize=12;
					item_title_fontsize=14;
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
	
	accounts = {
        email.accounts.gmail = {
            address = "${secret.gmail.user}@gmail.com";
            flavor = "gmail.com";
            passwordCommand = "echo '${secret.gmail.password}'";
            userName = secret.gmail.user;
			realName = "Александр Бантьев";
            primary = true;
			mbsync = {
				enable = true;
				create = "maildir";
			};
			msmtp.enable = true;
			notmuch.enable = true;
        };
	};

	programs.mbsync.enable = true;
	programs.msmtp.enable = true;
	programs.notmuch.enable = true;
	news.display = "silent";
	programs.command-not-found.enable = true;
}
