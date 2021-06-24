{ pkgs, lib, config, ... }:
with import ../../support.nix { inherit lib config; }; {
  home-manager.users.balsoft.xdg.configFile."okularpartrc".text = genIni {
    "Dlg Accessibility" = {
      RecolorBackground = thmDec.base00;
      RecolorForeground = thmDec.base05;
    };
    "Document" = {
      ChangeColors = true;
      PaperColor = thmDec.base00;
      RenderMode = "Recolor";
    };
    "Main View" = { ShowLeftPanel = false; };
    PageView = {
      BackgroundColor = thmDec.base00;
      UseCustomBackgroundColor = true;
    };
  };
}
