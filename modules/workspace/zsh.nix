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
    plugins = [
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.fetchFromGitHub {
         owner = "chisui";
         repo = "zsh-nix-shell";
         rev = "b2609ca787803f523a18bb9f53277d0121e30389";
         sha256 = "01w59zzdj12p4ag9yla9ycxx58pg3rah2hnnf3sw4yk95w3hlzi6";
       };
      }
      {
        name = "zsh-autosuggestions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-autosuggestions";
          rev = "v0.4.0";
          sha256 = "0z6i9wjjklb4lvr7zjhbphibsyx51psv50gm07mbb0kj9058j6kc";
        };
      }
      {
        name = "you-should-use";
        src = pkgs.fetchFromGitHub {
          owner = "MichaelAquilina";
          repo = "zsh-you-should-use";
          rev = "2be37f376c13187c445ae9534550a8a5810d4361";
          sha256 = "0yhwn6av4q6hz9s34h4m3vdk64ly6s28xfd8ijgdbzic8qawj5p1";
        };
      }
    ];
    shellAliases = {
      "b" = ''nix-build "<nixpkgs>" --no-out-link -A'';
      "p" = "nix-shell --run zsh -p";
      "o" = "xdg-open";
      "post" = ''curl -F"file=@-" 0x0.st'';
      "clip" = "${pkgs.xclip}/bin/xclip -selection clipboard";
    };
    initExtra = ''
      r(){nix run nixpkgs.$1 -c $@ }
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
       
      PS1="$PS1
     $ "
   '';
  };
}
