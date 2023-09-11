{ pkgs, lib, ... }: {

  environment.sessionVariables = {
    XCURSOR_PATH = lib.mkForce "/home/balsoft/.icons";
  };

  home-manager.users.balsoft = {

    home.pointerCursor = {
      package = pkgs.breeze-qt5;
      name = "Breeze";
      size = 16;
    };

    home.file."/home/balsoft/.icons/default".source =
      "${lib.getBin pkgs.breeze-qt5}/share/icons/breeze_cursors";


    # home.file."/home/balsoft/.icons/Breeze".source =
    #   "${lib.getBin pkgs.breeze-qt5}/share/icons/breeze_cursors";
  };
}
