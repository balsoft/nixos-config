{ pkgs, lib, ... }: {

  environment.sessionVariables = {
    XCURSOR_PATH = lib.mkForce "/home/balsoft/.icons";
  };

  home-manager.users.balsoft = {

    xsession.pointerCursor = {
      package = pkgs.breeze-qt5;
      name = "Breeze";
    };

    home.file.".icons/default".source =
      "${pkgs.breeze-qt5}/share/icons/breeze_cursors";


    home.file.".icons/Breeze".source =
      "${pkgs.breeze-qt5}/share/icons/breeze_cursors";
  };
}
