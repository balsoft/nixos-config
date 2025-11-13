{
  home-manager.users.balsoft.programs = {
    git = {
      enable = true;
      settings = {
        user.email = "balsoft@balsoft.ru";
        user.name = "Alexander Bantyev";
        pull.rebase = true;
      };
      ignores = [ ".envrc" ".direnv" ".shell.nix" ".balsoft" "*~" ".#*" "#*#" ];
      signing = {
        signByDefault = true;
        key = "687558B21E04FE92B255BED0E081FF12ADCB4AD5";
      };
    };
    lazygit = {
      enable = true;
      settings = { update.method = "never"; };
    };
  };
}
