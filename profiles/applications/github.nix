{ pkgs, config, ... }: {
  secrets.github_token = {
    owner = "balsoft";
    services = [ ];
  };

  home-manager.users.balsoft = {
    home.packages = let
      stateless-github-cli = pkgs.writeShellScriptBin "gh" ''
        export GITHUB_TOKEN="''${GITHUB_TOKEN-$(cat ${config.secrets.github_token.decrypted})}"
        exec ${pkgs.github-cli}/bin/gh "$@"
      '';
    in [ stateless-github-cli ];

    xdg.configFile."gh/config.yaml".text = builtins.toJSON {
      git_protocol = "ssh";
      editor = "";
      aliases = {
        pv = "pr view --comments";
      };
    };
  };
}
