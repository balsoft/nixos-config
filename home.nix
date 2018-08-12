
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
				    cmdstat="✓"
				else
				    cmdstat="✘"
				    fi
				if [ ! -z "$cmd" -a ! $term_window = $(active_window_id) ]; then
					${pkgs.libnotify}/bin/notify-send -i utilities-terminal -u low "$cmd $cmdstat" "in `date -u -d @$cmd_time +'%T'`"
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
			name = "Breeze-Dark";
			package = pkgs.gnome-breeze;
		};
		
	};

	home.packages = with pkgs; [
		# Internet
		wget
		curl
		chromium
		midori
		# IDE
		vscode
		geany
		kdevelop
		jetbrains.pycharm-community
		# Messaging
		tdesktop
		telepathy_haze
		telepathy_idle
		libnotify
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
		wpsoffice
		
	];
	home.keyboard = {
		options = ["grp:caps_toggle" "grp_led:caps"];
	};
	xdg = {
		enable = true;
		configFile."libinput-gestures.conf".text = ''
gesture swipe up 4 xdotool key "Alt+quoteright"
gesture swipe down 4 xdotool key "Alt+asciitilde"
gesture pinch in 2 xdotool key "Ctrl+F8"
gesture pinch out 2 xdotool key "Ctrl+F8"
gesture swipe left 3 xdotool key "Ctrl+Tab"
gesture swipe right 3 xdotool key "Ctrl+Shift+Tab"
gesture swipe up 3 xdotool key "Pause"
gesture swipe down 3 xdotool key "Pause"
		'';
	};
	programs.command-not-found.enable = true;
}
