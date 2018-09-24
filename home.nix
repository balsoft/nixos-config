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

customPackages = import ./packages {inherit pkgs;};

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
		shellAliases = {
			"p" = "nix-shell -p $1 --run zsh";
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
			assigns = {
				"C" = [{ class = "Chromium"; }];
				"T" = [{ class = "^Telegram"; }];	
			};
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
				{ command = "${pkgs.tdesktop}/bin/telegram-desktop"; }
				{ command = "${pkgs.chromium}/bin/chromium"; }
				{ command = term; workspace = "0"; }
				{ command = "${pkgs.kdeconnect}/lib/libexec/kdeconnectd -platform offscreen"; }
				{ command = "pkill polybar; polybar top"; always = true; }
				{ command = "${pkgs.kmix}/bin/kmix"; }
				{ command = "${customPackages.mconnect}/bin/mconnect"; }
				{ command = "${pkgs.polkit-kde-agent}/lib/libexec/polkit-kde-authentication-agent-1"; }
				{ command = "dunst"; }
			];
			keybindings =
			({
				"${modifier}+q" = "kill";
				"${modifier}+Return" = "exec ${term}";
				"${modifier}+t" = "workspace T";
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
				"${modifier}+d" = "exec ${pkgs.dolphin}/bin/dolphin";
				"${modifier}+Escape" = "exec ${pkgs.ksysguard}/bin/ksysguard";
				"${modifier}+Print" = "exec scrot -e 'mv $f ~/Pictures && notify-send \"Screenshot saved as ~/Pictures/$f\"'";
				"${modifier}+Shift+Print" = "exec scrot -e 'xclip -in $f && notify-send \"Screenshot copied to clipboard\"'";
				"--release ${modifier}+Control+Print" = "exec scrot -s -e 'mv $f ~/Pictures && notify-send \"Screenshot saved as ~/Pictures/$f\"'";
				"--release ${modifier}+Control+Shift+Print" = "exec scrot -s -e 'xclip -in $f && notify-send \"Screenshot copied to clipboard\"'";
				"${modifier}+c" = "workspace C";
			} // builtins.listToAttrs (
				builtins.genList (x: {name = "${modifier}+${toString x}"; value = "workspace ${toString x}";}) 10
			) // builtins.listToAttrs (
				builtins.genList (x: {name = "${modifier}+Shift+${toString x}"; value = "move container to workspace ${toString x}";}) 10
			));
			keycodebindings = {
				"232" = "exec echo $((`cat /sys/class/backlight/*/brightness`-10)) > /sys/class/backlight/*/brightness";
				"233" = "exec echo $((`cat /sys/class/backlight/*/brightness`-10)) > /sys/class/backlight/*/brightness";
			};
		};
		extraConfig = "hide_edge_borders smart";
	};

	
	services.polybar = {
		enable = true;
		package = pkgs.polybar.override {
			i3Support = true;
		};
		config = {
			"bar/top" = {
				font-0 = "Roboto Mono for Powerline:size=11;2";
				font-3 = "Roboto Mono for Powerline:size=24;5";
				font-1 = "Noto Sans Symbols2:size=15;4";
				font-2 = "Noto Emoji:size=11;2";
				font-4 = "Unifont:size-11;2";
				width = "100%";
				height = "25px";
				radius = 0;
				background = thm.bg;
				foreground = thm.fg;
				modules-left = "left_side";
				modules-center = "i3";
				modules-right = "right_side";
				tray-position = "none";
			};
			"module/i3" = {
				type = "internal/i3";
				label-focused-foreground = thm.blue;
				label-urgent-foreground = thm.red;
			};

			"module/left_side" = {
				type = "custom/script";
				exec = (scripts.polybar.left_side (with scripts.polybar; [ 
					(weather { city-id = "513378"; city = "Ozery"; }) 
					(time {})
					(now {})
					(next {}) 
					(email { user = secret.gmail.user; password = secret.gmail.password; }) 
				]));
				tail = true;
			};

			"module/right_side" = {
				type = "custom/script";
				exec = (scripts.polybar.right_side (with scripts.polybar; [
					(status {})
					(battery {})
					(network {})
				]));
				tail = true;
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
				word_wrap = true;
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

	services.udiskie.enable = true;
	
	home.packages = 
	(with pkgs; [
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
		frei0r
		ffmpeg-full
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
		libqalculate
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
		texlive.combined.scheme-basic
		gcalcli
		kdeconnect
	]) 
	++ 
	(with customPackages; [
		mconnect
	]);

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
		SSH_ASKPASS = "${pkgs.ksshaskpass}/bin/ksshaskpass";
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
				"org.albert.extension.files" = {
					enabled = true;
					filters = "application/*, image/*";	
				};
				"org.albert.extension.python" = {
					enabled = true;
					enabled_modules = "Python, Wikipedia, GoogleTranslate, Kill, qalc";
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
					icon_size=17;
					input_fontsize=17;
					item_description_fontsize=12;
					item_title_fontsize=14;
					max_items=10;
					padding=6;
					radius=2;
					settingsbutton_size=10;
					spacing=5;
					window_width=700;
				};
			};
			"kdeglobals".text = ''
				[$Version]
				update_info=fonts_global.upd:Fonts_Global,fonts_global_toolbar.upd:Fonts_Global_Toolbar
				
				[ColorEffects:Disabled]
				ChangeSelectionColor=
				Color=56,56,56
				ColorAmount=0
				ColorEffect=0
				ContrastAmount=0.65
				ContrastEffect=1
				Enable=
				IntensityAmount=0.1
				IntensityEffect=2
				
				[ColorEffects:Inactive]
				ChangeSelectionColor=true
				Color=112,111,110
				ColorAmount=0.025
				ColorEffect=2
				ContrastAmount=0.1
				ContrastEffect=2
				Enable=false
				IntensityAmount=0
				IntensityEffect=0
				
				[Colors:Button]
				BackgroundAlternate=77,77,77
				BackgroundNormal=49,54,59
				DecorationFocus=61,174,233
				DecorationHover=61,174,233
				ForegroundActive=61,174,233
				ForegroundInactive=189,195,199
				ForegroundLink=41,128,185
				ForegroundNegative=218,68,83
				ForegroundNeutral=246,116,0
				ForegroundNormal=239,240,241
				ForegroundPositive=39,174,96
				ForegroundVisited=127,140,141
				
				[Colors:Complementary]
				BackgroundAlternate=59,64,69
				BackgroundNormal=49,54,59
				DecorationFocus=30,146,255
				DecorationHover=61,174,230
				ForegroundActive=246,116,0
				ForegroundInactive=175,176,179
				ForegroundLink=61,174,230
				ForegroundNegative=237,21,21
				ForegroundNeutral=201,206,59
				ForegroundNormal=239,240,241
				ForegroundPositive=17,209,22
				ForegroundVisited=61,174,230
				
				[Colors:Selection]
				BackgroundAlternate=29,153,243
				BackgroundNormal=61,174,233
				DecorationFocus=61,174,233
				DecorationHover=61,174,233
				ForegroundActive=252,252,252
				ForegroundInactive=239,240,241
				ForegroundLink=253,188,75
				ForegroundNegative=218,68,83
				ForegroundNeutral=246,116,0
				ForegroundNormal=239,240,241
				ForegroundPositive=39,174,96
				ForegroundVisited=189,195,199
				
				[Colors:Tooltip]
				BackgroundAlternate=77,77,77
				BackgroundNormal=49,54,59
				DecorationFocus=61,174,233
				DecorationHover=61,174,233
				ForegroundActive=61,174,233
				ForegroundInactive=189,195,199
				ForegroundLink=41,128,185
				ForegroundNegative=218,68,83
				ForegroundNeutral=246,116,0
				ForegroundNormal=239,240,241
				ForegroundPositive=39,174,96
				ForegroundVisited=127,140,141
				
				[Colors:View]
				BackgroundAlternate=49,54,59
				BackgroundNormal=35,38,41
				DecorationFocus=61,174,233
				DecorationHover=61,174,233
				ForegroundActive=61,174,233
				ForegroundInactive=189,195,199
				ForegroundLink=41,128,185
				ForegroundNegative=218,68,83
				ForegroundNeutral=246,116,0
				ForegroundNormal=239,240,241
				ForegroundPositive=39,174,96
				ForegroundVisited=127,140,141
				
				[Colors:Window]
				BackgroundAlternate=77,77,77
				BackgroundNormal=49,54,59
				DecorationFocus=61,174,233
				DecorationHover=61,174,233
				ForegroundActive=61,174,233
				ForegroundInactive=189,195,199
				ForegroundLink=41,128,185
				ForegroundNegative=218,68,83
				ForegroundNeutral=246,116,0
				ForegroundNormal=239,240,241
				ForegroundPositive=39,174,96
				ForegroundVisited=127,140,141
				
				[DesktopIcons]
				ActiveColor=169,156,255
				ActiveColor2=0,0,0
				ActiveEffect=togamma
				ActiveSemiTransparent=false
				ActiveValue=0.699999988079071
				Animated=true
				DefaultColor=144,128,248
				DefaultColor2=0,0,0
				DefaultEffect=none
				DefaultSemiTransparent=false
				DefaultValue=1
				DisabledColor=34,202,0
				DisabledColor2=0,0,0
				DisabledEffect=togray
				DisabledSemiTransparent=true
				DisabledValue=1
				Size=48
				
				[DialogIcons]
				ActiveColor=169,156,255
				ActiveColor2=0,0,0
				ActiveEffect=none
				ActiveSemiTransparent=false
				ActiveValue=1
				Animated=false
				DefaultColor=144,128,248
				DefaultColor2=0,0,0
				DefaultEffect=none
				DefaultSemiTransparent=false
				DefaultValue=1
				DisabledColor=34,202,0
				DisabledColor2=0,0,0
				DisabledEffect=togray
				DisabledSemiTransparent=true
				DisabledValue=1
				Size=32
				
				[General]
				BrowserApplication[$e]=chromium-browser.desktop
				ColorScheme=Breeze Dark
				Name=Breeze Dark
				fixed=Monospace,10,-1,5,50,0,0,0,0,0
				font=Roboto,10,-1,5,50,0,0,0,0,0
				menuFont=Roboto,10,-1,5,50,0,0,0,0,0
				shadeSortColumn=true
				smallestReadableFont=Roboto,8,-1,5,57,0,0,0,0,0,Medium
				toolBarFont=Roboto,10,-1,5,50,0,0,0,0,0
				
				[Icons]
				Theme=breeze-dark
				
				[KDE]
				DoubleClickInterval=400
				LookAndFeelPackage=org.kde.breezedark.desktop
				ShowDeleteCommand=false
				SingleClick=true
				StartDragDist=4
				StartDragTime=500
				WheelScrollLines=3
				contrast=4
				widgetStyle=Breeze
				
				[KFileDialog Settings]
				Automatically select filename extension=true
				Breadcrumb Navigation=false
				Decoration position=0
				LocationCombo Completionmode=5
				PathCombo Completionmode=5
				Previews=false
				Show Bookmarks=false
				Show Full Path=false
				Show Preview=false
				Show Speedbar=true
				Show hidden files=false
				Sort by=Name
				Sort directories first=true
				Sort reversed=false
				Speedbar Width=141
				View Style=Simple
				listViewIconSize=0
				
				[KShortcutsDialog Settings]
				Dialog Size=600,480
				
				[MainToolbarIcons]
				ActiveColor=169,156,255
				ActiveColor2=0,0,0
				ActiveEffect=none
				ActiveSemiTransparent=false
				ActiveValue=1
				Animated=false
				DefaultColor=144,128,248
				DefaultColor2=0,0,0
				DefaultEffect=none
				DefaultSemiTransparent=false
				DefaultValue=1
				DisabledColor=34,202,0
				DisabledColor2=0,0,0
				DisabledEffect=togray
				DisabledSemiTransparent=true
				DisabledValue=1
				Size=22
				
				[PanelIcons]
				ActiveColor=169,156,255
				ActiveColor2=0,0,0
				ActiveEffect=togamma
				ActiveSemiTransparent=false
				ActiveValue=0.699999988079071
				Animated=false
				DefaultColor=144,128,248
				DefaultColor2=0,0,0
				DefaultEffect=none
				DefaultSemiTransparent=false
				DefaultValue=1
				DisabledColor=34,202,0
				DisabledColor2=0,0,0
				DisabledEffect=togray
				DisabledSemiTransparent=true
				DisabledValue=1
				Size=48
				
				[PreviewSettings]
				MaximumRemoteSize=0
				
				[SmallIcons]
				ActiveColor=169,156,255
				ActiveColor2=0,0,0
				ActiveEffect=none
				ActiveSemiTransparent=false
				ActiveValue=1
				Animated=false
				DefaultColor=144,128,248
				DefaultColor2=0,0,0
				DefaultEffect=none
				DefaultSemiTransparent=false
				DefaultValue=1
				DisabledColor=34,202,0
				DisabledColor2=0,0,0
				DisabledEffect=togray
				DisabledSemiTransparent=true
				DisabledValue=1
				Size=16
				
				[ToolbarIcons]
				ActiveColor=169,156,255
				ActiveColor2=0,0,0
				ActiveEffect=none
				ActiveSemiTransparent=false
				ActiveValue=1
				Animated=false
				DefaultColor=144,128,248
				DefaultColor2=0,0,0
				DefaultEffect=none
				DefaultSemiTransparent=false
				DefaultValue=1
				DisabledColor=34,202,0
				DisabledColor2=0,0,0
				DisabledEffect=togray
				DisabledSemiTransparent=true
				DisabledValue=1
				Size=22
				
			'';
			"mconnect/mconnect.conf".text = genIni {
				"main" = {
					devices = "lge;huawei";
				};
				lge = {
					name = "lge";
					type = "phone";
					allowed = 1;
				};
				huawei = {
					name = "huawei";
					type = "phone";
					allowed = 1;
				};
			};
		};
	};

	home.file.".icons/default".source = "${pkgs.breeze-qt5}/share/icons/breeze_cursors";
	
	home.file.".local/share/albert/org.albert.extension.python/modules/qalc.py".text = scripts.albert.qalc;

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
