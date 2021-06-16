{ pkgs, lib, ... }: {

  environment.sessionVariables = {
    XCURSOR_PATH = lib.mkForce "/home/balsoft/.icons";
  };

  home-manager.users.balsoft = {

    xsession.pointerCursor = {
      package = pkgs.breeze-qt5;
      name = "Breeze";
    };

    home.file."/home/balsoft/.icons/default".source =
      "${lib.getBin pkgs.breeze-qt5}/share/icons/breeze_cursors";


    home.file."/home/balsoft/.icons/Breeze".source =
      "${lib.getBin pkgs.breeze-qt5}/share/icons/breeze_cursors";
  };
}
