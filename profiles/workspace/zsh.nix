{ pkgs, config, lib, ... }: {

  environment.pathsToLink = [ "/share/zsh" ];
  environment.sessionVariables.SHELL = "zsh";

  # A history file is screwed up otherwise :(
  persist.state.directories = [ "/home/balsoft/.local/share/zsh" ];

  home-manager.users.balsoft.programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    oh-my-zsh = {
      enable = true;
      theme = "agnoster";
      plugins = [ "git" "dirhistory" ];
    };

    dotDir = ".config/zsh";

    history = rec {
      size = 1000000;
      save = size;
      path = "$HOME/.local/share/zsh/history";
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
      "b" = "nix build";
      "p" = "nix-shell --run zsh -p";
      "s" = "nix shell";
      "e" = "nix edit";
      "d" = "nix develop";
      "r" = "nix run";
      "f" = "nix search";
      "fs" = "nix search self";
      "o" = "xdg-open";
      "post" = ''curl -F"file=@-" https://0x0.st'';
      "cat" = "${pkgs.bat}/bin/bat";
      "ls" = "${pkgs.exa}/bin/exa";
    };
    initExtra = ''
       cmdignore=(htop tmux top vim)

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
         if [ ! -z "$cmd" ] && [[ $cmd_time -gt 3 ]]; then
           ${pkgs.libnotify}/bin/notify-send -i utilities-terminal -u low "$cmdstat $cmd" "in `date -u -d @$cmd_time +'%T'`"
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

       bindkey -M emacs '^H' backward-kill-word
       bindkey -r '^W'

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

       source ${pkgs.nix-zsh-completions}/share/zsh/plugins/nix/nix-zsh-completions.plugin.zsh
       fpath=(${pkgs.nix-zsh-completions}/share/zsh/site-functions $fpath)
       autoload -U compinit && compinit

       PS1="$PS1
       $ "
    '';
  };
}
