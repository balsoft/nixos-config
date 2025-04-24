{ pkgs, config, lib, ... }: {

  environment.pathsToLink = [ "/share/zsh" ];
  environment.sessionVariables.SHELL = "${pkgs.zsh}/bin/zsh";

  # A history file is screwed up otherwise :(
  persist.state.directories = [ "/home/balsoft/.local/share/zsh" ];

  home-manager.users.balsoft.programs.zsh = {
    enable = true;
    # enableAutosuggestions = true;
    enableCompletion = true;
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "dirhistory" ];
    };

    dotDir = ".config/zsh";

    history = rec {
      size = 10000000;
      save = size;
      path = "$HOME/.local/share/zsh/history";
    };
    plugins = [
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
      {
        name = "async";
        file = "async.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "mafredri";
          repo = "zsh-async";
          rev = "3ba6e2d1ea874bfb6badb8522ab86c1ae272923d";
          sha256 = "3hhZXL8/Ml7UlkkHBPpS5NfUGB5BqgO95UvtpptXf8E=";
        };
      }
      {
        name = "powerlevel10k-config";
        src = ./.;
        file = "p10k.zsh";
      }
      {
        name = "zsh-powerlevel10k";
        src = pkgs.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
    ];
    shellAliases = {
      "b" = "nix build";
      "p" = "nix-shell --run zsh -p";
      "s" = "nix shell";
      "e" = "$EDITOR";
      "dog" = "$EDITOR -";
      "d" = "nix develop";
      "r" = "nix run";
      "f" = "nix search";
      "fs" = "nix search self";
      "o" = "xdg-open";
      "post" = ''curl -F"file=@-" https://0x0.st'';
      "cat" = "${pkgs.bat}/bin/bat";
      "ls" = "${pkgs.eza}/bin/eza";
      "gp" = "git push && github_status_reset_bg";
      "gpf!" = "git push --force-with-lease && github_status_reset_bg";
    };
    initExtra = ''
      cmdignore=(htop tmux top vim)

      export GITHUB_TOKEN=$(cat /var/secrets/github_token)

      # end and compare timer, notify-send if needed
      function notifyosd-precmd() {
        retval=$?
        if [ ! -z "$cmd" ]; then
          cmd_end=`date +%s`
          ((cmd_time=$cmd_end - $cmd_start))
        fi
        cleareol="\e[K"
        colorreset="\e[1;0m"
        if [ $retval -eq 0 ]; then
          cmdstat="✓"
          bgcolor="\e[1;30;42m"
          fgcolor="\e[1;32;40m"
        else
          cmdstat="✘"
          bgcolor="\e[1;41m"
          fgcolor="\e[1;31;40m"
          printf "$bgcolor $cmdstat $retval $colorreset\n"
        fi
        if [ ! -z "$cmd" ]; then
          if [[ $cmd_time -gt 3 ]]; then
            ${pkgs.libnotify}/bin/notify-send -a command_complete -i utilities-terminal -u low "$cmdstat $cmd" "in `date -u -d @$cmd_time +'%T'`"
            echo -e '\a'
          fi
        fi
        unset cmd
      }

      # make sure this plays nicely with any existing precmd
      precmd_functions+=( notifyosd-precmd )

      # get command name and start the timer
      function notifyosd-preexec() {
        cmd=$1
        cmd_start=`date +%s`
      }

      bindkey -v
      bindkey -M viins '^H' backward-kill-word

      # make sure this plays nicely with any existing preexec
      preexec_functions+=( notifyosd-preexec )
      XDG_DATA_DIRS=$XDG_DATA_DIRS:$GSETTINGS_SCHEMAS_PATH

      function repl() {
        source="$(nix flake prefetch --json "$1" | ${pkgs.jq}/bin/jq -r .storePath)"
        TEMP="$(mktemp --suffix=.nix)"
        echo "let self = builtins.getFlake \"$source\"; in self // self.legacyPackages.\''${builtins.currentSystem} or { } // self.packages.\''${builtins.currentSystem} or { }" > "$TEMP"
        nix repl "$TEMP"
        rm "$TEMP"
      }


      function ss() { nix shell "self#$1" }
      function es() { nix edit "self#$1" }
      function bs() { nix build "self#$1" }
      function is() { nix search "self#$1" }
      function rs() { repl self }

      zle_highlight=(default:bg=#333333,fg=white)

      PS1="$PS1
      $ "
    '';
  };
}
