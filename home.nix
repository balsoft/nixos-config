device: {pkgs, lib, ...}:
with import ./support.nix { inherit lib; };
with import ./common.nix device;
let
	thm = {
		bg = "#31363b";
		fg = "#efefef";
		gray = "#7f8c8d";
		alt = "#48585f";
		dark = "#232629";
		blue = "#3caae4";
		green = "#11d116";
		red = "#da4453";
		orange = "#f67400";
		yellow = "#c9ce3b";
	};

	thmDec = builtins.mapAttrs (name: color: colorHex2Dec color) thm;

	term = "${pkgs.kdeApplications.konsole}/bin/konsole";

	secret = import ./secret.nix;

	scripts = import ./scripts {inherit pkgs; inherit secret; theme=thm; inherit device;};

	customPackages = import ./packages {inherit pkgs;};

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
				"dirhistory"
			];
		};
		shellAliases = {
			"p" = "nix-shell -p $1 --run zsh";
			"b" = "nix-build \"<nixpkgs>\" --no-out-link -A $1";
		};
		initExtra = scripts.zshrc;
	};

	gtk = {
		enable = true;
		iconTheme = {
			name = "Papirus-Dark";
			package = pkgs.papirus-icon-theme;
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
				"" = [{ class = "Chromium"; }];
				"" = [{ class = "^Telegram"; } ];	
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
                    border = thm.orange;
                    childBorder = thm.orange;
				};
				focused = unfocused // {
                    text = thm.fg;
				};
			};
			modifier = "Mod4";
			window = {
				border = 0;
				hideEdgeBorders = "smart";
			};
			startup = [
				{ command = "${pkgs.albert}/bin/albert"; always = true; }
				{ command = "${pkgs.tdesktop}/bin/telegram-desktop"; }
				{ command = "${pkgs.chromium}/bin/chromium"; }
				{ command = "pkill polybar; polybar top"; always = true; }
				{ command = "${customPackages.mconnect}/bin/mconnect"; }
				{ command = "${pkgs.polkit-kde-agent}/lib/libexec/polkit-kde-authentication-agent-1"; }
				{ command = "dunst"; }
				{ command = "${pkgs.autorandr}/bin/autorandr horizontal"; always = true; }
				#{ command = "google-drive-ocamlfuse '/home/balsoft/Google Drive/'"; }
				#{ command = "pkill compton; allow_rgb10_configs=false ${pkgs.compton}/bin/compton --backend glx --vsync opengl-swc"; always = true;}
				{ command = "trojita"; workspace = ""; }
				{ command = "google-drive-ocamlfuse -headless -f '/home/balsoft/Google Drive'"; }
				{ command = "${pkgs.hsetroot}/bin/hsetroot -solid '#31363b'"; always = true; }
			];
			keybindings =
			({
				"${modifier}+q" = "kill";
				"${modifier}+Return" = "exec ${term}";
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
				"${modifier}+Shift+f" = "floating toggle";
				"${modifier}+d" = "exec ${pkgs.dolphin}/bin/dolphin";
				"${modifier}+Escape" = "exec ${pkgs.ksysguard}/bin/ksysguard";
				"${modifier}+Print" = "exec scrot -e 'mv $f ~/Pictures && notify-send \"Screenshot saved as ~/Pictures/$f\"'";
				"${modifier}+Control+Print" = "exec scrot -e 'xclip -selection clipboard -t image/png -i $f && notify-send \"Screenshot copied to clipboard\" && rm $f'";
				"--release ${modifier}+Shift+Print" = "exec scrot -s -e 'mv $f ~/Pictures && notify-send \"Screenshot saved as ~/Pictures/$f\"'";
				"--release ${modifier}+Control+Shift+Print" = "exec scrot -s -e 'xclip -selection clipboard -t image/png -i $f && notify-send \"Screenshot copied to clipboard\" && rm $f'";
				"${modifier}+x" = "move workspace to output right";	
				"${modifier}+c" = "workspace ";
				"${modifier}+Shift+c" = "move container to workspace ";
				"${modifier}+t" = "workspace ";
				"${modifier}+Shift+t" = "move container to workspace ";
			} // builtins.listToAttrs (
				builtins.genList (x: {name = "${modifier}+${toString x}"; value = "workspace ${toString x}";}) 10
			) // builtins.listToAttrs (
				builtins.genList (x: {name = "${modifier}+Shift+${toString x}"; value = "move container to workspace ${toString x}";}) 10
			));
			keycodebindings = {
				"122" = "exec ${pkgs.pamixer}/bin/pamixer -d 5";
				"123" = "exec ${pkgs.pamixer}/bin/pamixer -i 5";
				"121" = "exec ${pkgs.pamixer}/bin/pamixer -t";
			};
		};
	};


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
			};
			"module/i3" = {
				type = "internal/i3";
				label-focused-foreground = thm.blue;
				label-urgent-foreground = thm.orange;
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
					(brightness { inherit device; })
				] ++ (if isLaptop && device != "Prestigio-Laptop" then [
					(sound {})
					(battery {})
				] else []) ++ [
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
			predetect = {
				compton = "pkill compton";		
			};
			postswitch = {
				compton = "allow_rgb10_configs=false ${pkgs.compton}/bin/compton --backend glx --vsync opengl-swc";	
			};
		};
		profiles = if device == "HP-Laptop" then {
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
		} else if device == "ASUS-Laptop" then {
			"dacha" = {
				fingerprint = {
					HDMI2 = "00ffffffffffff0006b3cc24010101011a1a010380351e78ea0565a756529c270f5054afcf80714f8180818000fc00565a3234390a20202020202020000000ff0047364c4d52533034383636390a018902031df14a9004030114000f282100009e011d007251d01e206e2855000f282100001e8c0ad08a20e02d10103e96000f28210000180000000";
					eDP1 = "00ffffffffffff000dae61130000000007180104a51d117802ce85a3574e9d2612505400000001010101010100fe00434d4e0a202020202020202020000000fe004e3133334853452d4541330a2000a1";
				};
				config = {
					eDP1 = {
                        enable = true;
                        primary = true;
                        position = "0x0";
                        mode = "1920x1080";
                        rate = "60.00";
					};
					HDMI2 = {
						enable = true;
						position = "1920x0";
						mode = "1920x1080";
						rate = "60.00";
					};
				};
			};
		} else {};
	};

	services.udiskie.enable = true;
	
	home.packages = 
	(with pkgs; [
		# Internet
		wget
		curl
		chromium
	] ++ (if devMachine then [
		# IDE
		vscode
		geany
		kdevelop
		jetbrains.pycharm-community
		kate
		steam # much dev, very work, wow
	] else [] ) ++ [
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
		lxqt.pavucontrol-qt
		# Tools
		zip
		unrar
		wine
		kolourpaint
		ktorrent
		wireshark
		#wpsoffice
		arduino
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
		adwaita-qt
		gnome3.adwaita-icon-theme
		papirus-icon-theme
		breeze-icons
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
		google-drive-ocamlfuse
		kdeconnect
		trojita
		nix-zsh-completions
		material-icons
		breeze-icons
		papirus-icon-theme
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
					frontendId = "org.albert.frontend.qmlboxmodel";
					hotkey = "Meta+Space";
					showTray = false;
					terminal = "${pkgs.konsole}/bin/konsole -e";
					incrementalSort = true;
				};
				"org.albert.extension.applications".enabled = true;
				"org.albert.extension.files" = {
					enabled = true;
					filters = "application/*, image/*, directory/*, text/*";	
				};
				"org.albert.extension.chromebookmarks".enabled = true;
				"org.albert.extension.mpris".enabled = true;
				"org.albert.extension.python" = {
					enabled = true;
					enabled_modules = "Python, Wikipedia, GoogleTranslate, Kill, qalc";
				};
				"org.albert.extension.ssh".enabled = true;
				"org.albert.extension.system" = {
						enabled = true;
						logout = "i3-msg exit";
						lock = "${pkgs.i3lock-fancy}/bin/i3lock-fancy";
						reboot = "reboot";
						shutdown = "shutdown now";		
				};
				"org.albert.extension.terminal".enabled = true;
				"org.albert.extension.websearch".enabled = true;
				"org.albert.frontend.qmlboxmodel" = {
					enabled = true;
					alwaysOnTop=true;
					clearOnHide=false;
					hideOnClose=false;
					hideOnFocusLoss=true;
					showCentered=true;
					stylePath="${pkgs.albert}/share/albert/org.albert.frontend.qmlboxmodel/styles/BoxModel/MainComponent.qml";
					windowPosition="@Point(299 13)";
				};
			};
			"albert/org.albert.frontend.qmlboxmodel/style_properties.ini".text = genIni {
				BoxModel = {
					animation_duration=0;
					#background_color="\"@Variant(\\0\\0\\0\\x43\\x1\\xff\\xff\\x31\\x31\\x36\\x36;;\\0\\0)\"";
					background_color = thm.bg;
					#border_color="\"@Variant(\\0\\0\\0\\x43\\x1\\xff\\xff==\\xae\\xae\\xe9\\xe9\\0\\0)\"";
					border_color = thm.blue;
					border_size=1;
					icon_size=46;
					input_fontsize=28;
					item_description_fontsize=20;
					item_title_fontsize=24;
					max_items=10;
					padding=6;
					radius=2;
					settingsbutton_size=10;
					spacing=5;
					window_width=1200;
				};
			};
			"kdeglobals".text = genIni {
				"Colors:Button" = {
					BackgroundAlternate = thmDec.dark;
					BackgroundNormal = thmDec.bg;
					DecorationFocus = thmDec.blue;
					DecorationHover = thmDec.blue;
					ForegroundActive = thmDec.blue;
					ForegroundInactive = thmDec.alt;
					ForegroundLink = thmDec.blue;
					ForegroundNegative = thmDec.red;
					ForegroundNeutral = thmDec.orange;
					ForegroundNormal = thmDec.fg;
					ForegroundPositive = thmDec.green;
					ForegroundVisited = thmDec.gray;
				};
				"Colors:Complementary" = {
					BackgroundAlternate=thmDec.dark;
					BackgroundNormal=thmDec.bg;
					DecorationFocus=thmDec.blue;
					DecorationHover=thmDec.blue;
					ForegroundActive=thmDec.orange;
					ForegroundInactive=thmDec.alt;
					ForegroundLink=thmDec.blue;
					ForegroundNegative=thmDec.red;
					ForegroundNeutral=thmDec.yellow;
					ForegroundNormal=thmDec.fg;
					ForegroundPositive=thmDec.green;
					ForegroundVisited=thmDec.blue;
				};
				"Colors:Selection" = {
					BackgroundAlternate=thmDec.blue;
					BackgroundNormal=thmDec.blue;
					DecorationFocus=thmDec.blue;
					DecorationHover=thmDec.blue;
					ForegroundActive=thmDec.fg;
					ForegroundInactive=thmDec.fg;
					ForegroundLink=thmDec.blue;
					ForegroundNegative=thmDec.red;
					ForegroundNeutral=thmDec.orange;
					ForegroundNormal=thmDec.fg;
					ForegroundPositive=thmDec.green;
					ForegroundVisited=thmDec.alt;
				};
				"Colors:Tooltip" = {
					BackgroundAlternate=thmDec.dark;
					BackgroundNormal=thmDec.bg;
					DecorationFocus=thmDec.blue;
					DecorationHover=thmDec.blue;
					ForegroundActive=thmDec.blue;
					ForegroundInactive=thmDec.alt;
					ForegroundLink=thmDec.blue;
					ForegroundNegative=thmDec.red;
					ForegroundNeutral=thmDec.orange;
					ForegroundNormal=thmDec.fg;
					ForegroundPositive=thmDec.green;
					ForegroundVisited=thmDec.gray;
				};
				"Colors:View" = {
					BackgroundAlternate=thmDec.dark;
					BackgroundNormal=thmDec.bg;
					DecorationFocus=thmDec.blue;
					DecorationHover=thmDec.blue;
					ForegroundActive=thmDec.blue;
					ForegroundInactive=thmDec.alt;
					ForegroundLink=thmDec.blue;
					ForegroundNegative=thmDec.red;
					ForegroundNeutral=thmDec.orange;
					ForegroundNormal=thmDec.fg;
					ForegroundPositive=thmDec.green;
					ForegroundVisited=thmDec.gray;
				};
				"Colors:Window" = {
					BackgroundAlternate=thmDec.dark;
					BackgroundNormal=thmDec.bg;
					DecorationFocus=thmDec.blue;
					DecorationHover=thmDec.blue;
					ForegroundActive=thmDec.blue;
					ForegroundInactive=thmDec.alt;
					ForegroundLink=thmDec.blue;
					ForegroundNegative=thmDec.red;
					ForegroundNeutral=thmDec.orange;
					ForegroundNormal=thmDec.fg;
					ForegroundPositive=thmDec.green;
					ForegroundVisited=thmDec.gray;
				};
				General = {
					ColorScheme="Breeze Dark";
					Name="Breeze Dark";
				};
			};
			"qt5ct/qt5ct.conf".text = genIni {
				Appearance = {
					color_scheme_path = "${pkgs.qt5ct}/share/qt5ct/colors/airy.conf";
					custom_palette = false;
					icon_theme = "Papirus-Dark";
					standard_dialogs = "default";
					style = "Breeze";
				};

				Fonts = {
					fixed = "@Variant(\\0\\0\\0@\\0\\0\\0\\x16\\0R\\0o\\0\\x62\\0o\\0t\\0o\\0 \\0M\\0o\\0n\\0o@(\\0\\0\\0\\0\\0\\0\\xff\\xff\\xff\\xff\\x5\\x1\\0\\x32\\x10)"; # Roboto Mono Regular 12
					general= "@Variant(\\0\\0\\0@\\0\\0\\0\\f\\0R\\0o\\0\\x62\\0o\\0t\\0o@(\\0\\0\\0\\0\\0\\0\\xff\\xff\\xff\\xff\\x5\\x1\\0\\x32\\x10)"; # Roboto Regular 12
				};
				Interface = {
					activate_item_on_single_click = 1;
					buttonbox_layout = 0;
					cursor_flash_time = 1000;
					dialog_buttons_have_icons = 1;
					double_click_interval = 400;
					gui_effects = "@Invalid()";
					menus_have_icons = true;
					stylesheets = "@Invalid()";
					toolbutton_style = 4;
					underline_shortcut = 1;
					wheel_scroll_lines = 3;
				};
			};
			"konsolerc.home".text = genIni {
				"Desktop Entry".DefaultProfile = "Default.profile";
				KonsoleWindow.ShowMenuBarByDefault = false;
			};

			"katerc.home".text = genIni {
				"KTextEditor Renderer" = {
					"Animate Bracket Matching" = false;
					"Schema" = "Breeze Dark";
					"Show Indentation Lines" = true;
					"Show Whole Bracket Expression" = false;
					"Word Wrap Marker" = true;
				};
				UiSettings = {
					ColorScheme = "Breeze Dark";
				};
			};

			"kateschemarc".text = genIni {
				"Breeze Dark"."Color Background" = thmDec.bg;
			};

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
			"flaska.net/trojita.conf".text = genIni {
				General = {
					"app.updates.checkEnabled" = false;
					"imap.auth.user" = secret.gmail.user;
					"imap.auth.pass" = secret.gmail.password;
					"imap.host" = "imap.gmail.com";
					"imap.method" = "SSL";
					"imap.needsNetwork" = true;
					"imap.numberRefreshInterval" = 300;
					"imap.port" = 993;
					"imap.proxy.system" = true;
					"imap.starttls" = true;
					"imapIdleRenewal" = 29;
					"msa.method" = "SMTP";
					"msa.smtp.auth" = true;
					"msa.smtp.auth.reuseImapCredentials" = true;
					"msa.smtp.burl" = false;
					"msa.smtp.host" = "smtp.gmail.com";
					"msa.smtp.port" = 587;
					"msa.smtp.starttls" = true;
					"offline.cache" = "days";
					"offline.cache.numDays" = "30";
				};
				autoMarkRead = {
					enabled = true;
					seconds = 0;
				};
				composer = {
					imapSentName = "Sent";
					saveToImapEnabled = false;
				};
				gui = {
					"mainWindow.layout" = "compact";
					preferPlaintextRendering = true;
					showSystray = false;
					startMinimized = false;
				};
				identities = {
					"1\\address" = "${secret.gmail.user}@gmail.com";
					"1\\organisation" = "";
					"1\\realName" = "Alexander Bantyev";
					"1\\signature" = "";
					size = 1;
				};
				interoperability.revealVersions = true;
				plugin = {
					addressbook = "abookaddressbook";
					password = "cleartextpassword";
				};
			};
			"Code/User/settings.json".text = builtins.toJSON { 
				"editor.fontFamily" = "Roboto Mono"; 
				"editor.formatOnPaste" = true; 
				"editor.formatOnSave" = true; 
				"editor.insertSpaces" = false; 
				"files.autoSave" = "onFocusChange"; 
				"git.autofetch" = true;
				"terminal.integrated.cursorStyle" = "line"; 
				"terminal.integrated.shell.linux" = "zsh"; 
				"update.channel" = "none";
				"window.zoomLevel" = 0; 
				"window.menuBarVisibility" = "toggle";
				"workbench.colorTheme" = "Breeze Dark Theme"; 
				"workbench.colorCustomizations" = { 
					"activityBar.background" = thm.bg;
					"activityBar.foreground" = thm.fg;
					"activityBarBadge.background" = "#3daee9";
					"activityBarBadge.foreground" = thm.fg;
					"badge.background" = "#00000030";
					"badge.foreground" = thm.fg;
					"button.background" = thm.bg;
					"debugToolBar.background" = "#31363b";
					"diffEditor.insertedTextBackground" = "#C3E88D15";
					"diffEditor.removedTextBackground" = "#FF537020";
					"dropdown.background" = "#31363b";
					"dropdown.border" = "#FFFFFF10";
					"editor.background" = "#31363b";
					"editor.foreground" = thm.fg;
					"editor.lineHighlightBackground" = "#3daee910";
					"editor.selectionBackground" = "#3daee920";
					"editor.selectionHighlightBackground" = "#FFCC0020";
					"editorBracketMatch.background" = "#31363b";
					"editorBracketMatch.border" = "#FFCC0050";
					"editorCursor.foreground" = thm.fg;
					"editorError.foreground" = thm.fg;
					"editorGroup.border" = "#00000030";
					"editorGroupHeader.tabsBackground" = "#31363b";
					"editorGutter.addedBackground" = "#C3E88D60";
					"editorGutter.deletedBackground" = "#FF537060";
					"editorGutter.modifiedBackground" = "#82AAFF60";
					"editorHoverWidget.background" = "#31363b";
					"editorHoverWidget.border" = "#FFFFFF10";
					"editorIndentGuide.background" = "#48585F80";
					"editorLineNumber.foreground" = thm.alt;
					"editorLink.activeForeground" = "#EEFFFF";
					"editorMarkerNavigation.background" = "#EEFFFF05";
					"editorSuggestWidget.background" = "#31363b";
					"editorSuggestWidget.border" = "#FFFFFF10";
					"editorSuggestWidget.foreground" = thm.fg;
					"editorSuggestWidget.highlightForeground" = "#3daee9";
					"editorSuggestWidget.selectedBackground" = "#00000050";
					"editorWarning.foreground" = thm.fg;
					"editorWhitespace.foreground" = thm.fg;
					"editorWidget.background" = "#31363b";
					"extensionButton.prominentBackground" = "#C3E88D90";
					"extensionButton.prominentHoverBackground" = "#C3E88D";
					focusBorder = "#FFFFFF00";
					"input.background" = "#FFFFFF05";
					"input.border" = "#FFFFFF10";
					"input.foreground" = thm.fg;
					"input.placeholderForeground" = "#EEFFFF60";
					"inputValidation.errorBorder" = "#FF5370";
					"inputValidation.infoBorder" = "#82AAFF";
					"inputValidation.warningBorder" = "#FFCB6B";
					"list.activeSelectionBackground" = "#31363b";
					"list.activeSelectionForeground" = "#3daee9";
					"list.focusBackground" = "#EEFFFF20";
					"list.focusForeground" = "#EEFFFF";
					"list.highlightForeground" = "#3daee9";
					"list.hoverBackground" = "#31363b";
					"list.hoverForeground" = "#FFFFFF";
					"list.inactiveSelectionBackground" = "#31363b";
					"list.inactiveSelectionForeground" = "#3daee9";
					"notification.background" = "#31363b";
					"notification.buttonBackground" = "#EEFFFF50";
					"notification.foreground" = thm.fg;
					"notification.infoBackground" = "#82AAFF";
					"notification.infoForeground" = "#ffffff";
					"notification.warningBackground" = "#FF5370";
					"notification.warningForeground" = "#ffffff";
					"panel.border" = "#31363b";
					"panelTitle.activeForeground" = "#EEFFFF";
					"peekView.border" = "#00000030";
					"peekViewEditor.background" = "#EEFFFF05";
					"peekViewEditorGutter.background" = "#EEFFFF05";
					"peekViewResult.background" = "#EEFFFF05";
					"peekViewTitle.background" = "#EEFFFF05";
					"peekViewTitleDescription.foreground" = thm.fg;
					"pickerGroup.foreground" = thm.fg;
					"progressBar.background" = "#3daee9";
					"scrollbar.shadow" = "#31363b00";
					"scrollbarSlider.activeBackground" = "#3daee9";
					"scrollbarSlider.background" = "#00000050";
					"scrollbarSlider.hoverBackground" = "#00000030";
					"selection.background" = "#EEFFFF";
					"sideBar.background" = "#31363b";
					"sideBar.foreground" = thm.fg;
					"sideBarSectionHeader.background" = "#31363b";
					"sideBarTitle.foreground" = thm.fg;
					"statusBar.background" = "#31363b";
					"statusBar.debuggingBackground" = "#C792EA";
					"statusBar.debuggingForeground" = "#ffffff";
					"statusBar.foreground" = thm.fg;
					"statusBar.noFolderBackground" = "#31363b";
					"tab.activeBorder" = "#3daee9";
					"tab.activeForeground" = "#FFFFFF";
					"tab.border" = "#31363b";
					"tab.inactiveBackground" = "#31363b";
					"tab.inactiveForeground" = "#546E7A";
					"tab.unfocusedActiveBorder" = "#546E7A";
					"tab.unfocusedActiveForeground" = "#EEFFFF";
					"terminal.ansiBlack" = "#546E7A";
					"terminal.ansiBlue" = "#82AAFF";
					"terminal.ansiBrightBlack" = "#546E7A";
					"terminal.ansiBrightBlue" = "#82AAFF";
					"terminal.ansiBrightCyan" = "#89DDFF";
					"terminal.ansiBrightGreen" = "#C3E88D";
					"terminal.ansiBrightMagenta" = "#C792EA";
					"terminal.ansiBrightRed" = "#FF5370";
					"terminal.ansiBrightWhite" = "#ffffff";
					"terminal.ansiBrightYellow" = "#FFCB6B";
					"terminal.ansiCyan" = "#89DDFF";
					"terminal.ansiGreen" = "#C3E88D";
					"terminal.ansiMagenta" = "#C792EA";
					"terminal.ansiRed" = "#FF5370";
					"terminal.ansiWhite" = "#ffffff";
					"terminal.ansiYellow" = "#FFCB6B";
					"textLink.activeForeground" = "#EEFFFF";
					"textLink.foreground" = thm.fg;
					"titleBar.activeBackground" = "#31363b";
					"titleBar.activeForeground" = "#546E7A";
					"titleBar.inactiveBackground" = "#31363b";
					"titleBar.inactiveForeground" = "#546E7A";
					"widget.shadow" = "#00000030";
				};
			};
		};
	};
	xdg.dataFile."albert/org.albert.extension.python/modules/qalc.py".text = scripts.albert.qalc;
	xdg.dataFile."Steam/skins/Metro".source = pkgs.fetchurl {
		url = "http://metroforsteam.com/downloads/4.3.1.zip";
		sha256 = "0e4e8bd6e164c60be7924d18ab29ddf966d31dd0db6a6820c213d25bc1a14bd2";
	};
	xdg.dataFile."konsole/Default.profile".text = genIni {
		Appearance.ColorScheme = "Breeze";
		"Cursor Options".CursorShape = 1;
		General = {
			Command = "zsh";
			Name = "Default";
			Parent = "FALLBACK/";
		};
		Scrolling.HistoryMode = 2;
		"Terminal Features".BlinkingCursorEnabled = true;
	};
	home.file.".icons/default".source = "${pkgs.breeze-qt5}/share/icons/breeze_cursors";

	home.activation = builtins.mapAttrs (name: value: {inherit name; before = []; after = [];} // value) {
		konsole.data = "$DRY_RUN_CMD cp ~/.config/konsolerc.home ~/.config/konsolerc";
		kate.data = "$DRY_RUN_CMD cp ~/.config/katerc.home ~/.config/katerc";
		# FIXME soooo ugly and imperative...
		vscode.data =if devMachine then builtins.concatStringsSep " || echo 'Error'\n" ((map (ext: "$DRY_RUN_CMD code --install-extension ${ext}") [
			"AndrewFridley.Breeze-Dark-Theme"
			"ms-vscode.cpptools"
			"bbenoist.nix"
			"eamodio.gitlens"
			"vsciot-vscode.vscode-arduino"
		]) ++ ["$DRY_RUN_CMD ${pkgs.patchelf}/bin/patchelf --set-interpreter ${pkgs.glibc}/lib/ld-linux-x86-64.so.2 ~/.vscode/extensions/*/bin/* || echo 'error in patching'"]) else "";
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
	programs.ssh = {
		enable = true;
		matchBlocks = map (name: {
			hostname = name;
			identityFile = pkgs.writeTextFile {
				name = "id_rsa";
				text = secret.id_rsa;
			};
		}) (builtins.attrNames myDevices);
	};
}
