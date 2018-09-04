{pkgs, lib, ...}:
let
thm = {
	bg = "#31363b";
	fg = "#efefef";
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
			bars = [
				{
					colors = {
						background = thm.bg;
						statusline = thm.fg;
					};
					position = "top";
				}			
			];
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
			];
			keybindings = let modifier = xsession.windowManager.i3.config.modifier;
			in ({
				"${modifier}+q" = "kill";
				"${modifier}+Pause" = "exec ${pkgs.kdeApplications.konsole}/bin/konsole";
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
	home.file.".profile" = {
		text = builtins.concatStringsSep "\n" (map (x: "export ${x}=${builtins.getAttr x home.sessionVariables}") (builtins.attrNames home.sessionVariables));		
	};
	xdg = {
		enable = true;
		configFile."libinput-gestures.conf".text = ''
gesture swipe down 4 xdotool key "Alt+quoteleft"
gesture swipe up 4 xdotool key "Alt+asciitilde"
gesture pinch in 2 xdotool key "Ctrl+F8"
gesture pinch out 2 xdotool key "Ctrl+F8"
gesture swipe right 3 xdotool key "Ctrl+Tab"
gesture swipe left 3 xdotool key "Ctrl+Shift+Tab"
gesture swipe up 3 xdotool key "Pause"
gesture swipe down 3 xdotool key "Pause"
		'';
	};
	news.display = "silent";
	programs.command-not-found.enable = true;
}
