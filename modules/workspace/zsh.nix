{ pkgs, config, ... }: {

  environment.pathsToLink = [ "/share/zsh" ];
  environment.sessionVariables.SHELL = "zsh";
  home-manager.users.balsoft.programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    oh-my-zsh = {
      enable = true;
      theme = "agnoster";
      plugins = [ "git" "dirhistory" ];
    };
    shellAliases = {
      "p" = "nix-shell --run zsh -p";
      "b" = ''nix-build "<nixpkgs>" --no-out-link -A'';
      "o" = "xdg-open";
      "ix" = ''curl -F "f:1=<-" ix.io'';
      "clip" = "${pkgs.xclip}/bin/xclip -selection clipboard";
    };
    initExtra = ''
      r(){nix run nixpkgs.$1 -c $@}

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

      source ${
       pkgs.fetchFromGitHub {
         owner = "chisui";
         repo = "zsh-nix-shell";
         rev = "b2609ca787803f523a18bb9f53277d0121e30389";
         sha256 = "01w59zzdj12p4ag9yla9ycxx58pg3rah2hnnf3sw4yk95w3hlzi6";
       }
     }/nix-shell.plugin.zsh
      source ${
       pkgs.fetchurl {
         url =
         "https://gist.githubusercontent.com/chisui/0d12bd51a5fd8e6bb52e6e6a43d31d5e/raw/ea75cad507e2899b9b6d6ce423330641911110d8/agnoster-nix.zsh-theme";
         sha256 = "0fq1jnzdf0956ia0as61p43wmqak6zljhyax1gqnjgb8jimr5kzm";
       }
     }
       

      PS1="$PS1
     $ "
   '';
  };
}
