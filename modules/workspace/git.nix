{
  home-manager.users.balsoft.programs.git = {
    enable = true;
    userEmail = "balsoft@balsoft.ru";
    userName = "Alexander Bantyev";
    extraConfig.pull.rebase = true;
    ignores = [ ".envrc" ".direnv" ".#*" ];
    signing = {
      signByDefault = true;
      key = "687558B21E04FE92B255BED0E081FF12ADCB4AD5";
    };
  };
}
