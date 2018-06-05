
{pkgs, ...}:
{
	programs.home-manager = {
		enable = true;
		path = https://github.com/rycee/home-manager/archive/master.tar.gz;
	};
	
	programs.zsh = {
		enable = true;
		enableAutosuggestions = true;
		enableCompletion = true;
		oh-my-zsh = 
		{
			enable = true;
			theme = "agnoster";
			plugins = 
			[
				"git"
				"compleat"
				"dirhistory"
			];
		};
		initExtra = ''
			#randomquote(){${pkgs.curl}/bin/curl -s $(${pkgs.curl}/bin/curl -s "http://bash.im/forweb/"|grep -o "http://bash.im/quote/[[:digit:]]*") | iconv -f CP1251 -t UTF-8 | ${pkgs.gnugrep}/bin/grep "<div class=\"text\">[^<]*" -a|${pkgs.gnused}/bin/sed "s/<br>/\x0a/g" | ${pkgs.gnused}/bin/sed "s/<br \/>/\x0a/"|${pkgs.gnused}/bin/sed "s/<div class=\x22text\x22>//"|${pkgs.gnused}/bin/sed "s/<\/div>//"|${pkgs.gnused}/bin/sed "s/&quot;/\"/g" |${pkgs.gnused}/bin/sed "s/&gt;/>/g"| ${pkgs.gnused}/bin/sed "s/&lt;/</g"| ${pkgs.gnused}/bin/sed 's/^\s*//g'}
			# commands to ignore
			cmdignore=(htop tmux top vim)
			function active_window_id () {
				if [[ -n $DISPLAY ]] ; then
				    xprop -root _NET_ACTIVE_WINDOW | awk '{print $5}'
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
				    cmdstat="succesfully"
				    sound="/home/balsoft/.local/share/gnome-shell/extensions/timepp@zagortenay333/data/sounds/bell.ogg"
				else
				    cmdstat="with errors"
				    sound="/home/balsoft/.local/share/gnome-shell/extensions/timepp@zagortenay333/data/sounds/goat.ogg"
				fi
				if [ ! -z "$cmd" -a $cmd_time -gt 10 -a ! $term_window = $(active_window_id) ]; then
				    if [ ! -z $SSH_TTY ] ; then
					${pkgs.libnotify}/bin/notify-send -i utilities-terminal -u low "$cmd on `hostname` completed $cmdstat" "\"$cmd\" took $cmd_time seconds"
				    else
					${pkgs.libnotify}/bin/notify-send -i utilities-terminal -u low ""$cmd" completed $cmdstat" "\"$cmd\" took $cmd_time seconds"
				    fi	
				    ${pkgs.gnome3.gsound}/bin/gsound-play -f $sound
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
			export QT_STYLE_OVERRIDE=kvantum
		'';
	};



	gtk = {
		enable = true;
		iconTheme = 
		{
			name = "Papirus-Dark";
			package = pkgs.papirus-icon-theme;
		};
		theme = 
		{
			name = "Adapta-Nokto-Teal";
			package = pkgs.adapta-gtk-theme;
		};
		
	};

	home.packages = with pkgs; [
		# Internet
		wget
		curl
		chromium
		gnome3.epiphany
		# IDE
		vscode
		geany
		kdevelop
		jetbrains.pycharm-community
		# Messaging
		discord
		tdesktop
		telepathy_haze
		telepathy_idle
		libnotify
		gnome3.evolution
		skype
		# Audio/Video
		vlc
		kdenlive
		google-play-music-desktop-player
		# Tools
		zip
		unrar
		mc
		gnome3.gnome-terminal
		tilix
		gnome3.gnome-tweak-tool
		transmission
		qtstyleplugin-kvantum-qt4
		nox
		gnome3.gnome-maps
		gnome3.gnome-clocks
		pinta
		adapta-backgrounds
		gnome3.gnome-weather
		wine
		gnome3.gnome-calendar
	];
	home.keyboard = {
		options = ["grp:caps_toggle" "grp_led:caps"];
	};
	xdg = {
		enable = true;
		configFile."libinput-gestures.conf".text = ''
gesture swipe up 4 _internal ws_up
gesture swipe down 4 _internal ws_down
gesture pinch in 2 dbus-send --session --type=method_call --dest=org.gnome.Shell /org/gnome/Shell org.gnome.Shell.Eval string:Main.overview.toggle();
gesture pinch out 2 dbus-send --session --type=method_call --dest=org.gnome.Shell /org/gnome/Shell org.gnome.Shell.Eval string:Main.overview.toggle();
gesture swipe left 4 dbus-send --session --type=method_call --dest=org.gnome.Shell /org/gnome/Shell org.gnome.Shell.Eval string:Main.wm._switchApp();  
gesture swipe right 4 dbus-send --session --type=method_call --dest=org.gnome.Shell /org/gnome/Shell org.gnome.Shell.Eval string:Main.wm._switchApp();  
		'';
	};
	home.file = {
		"run_gnome_session.nixsh".text = ''
#! /usr/bin/env nix-shell
#! nix-shell -p glib.dev -i bash
XDG_SESSION_TYPE=wayland dbus-run-session ${pkgs.gnome3.gnome-session}/bin/gnome-session
		'';
	};
	
	programs.command-not-found.enable = true;
}
