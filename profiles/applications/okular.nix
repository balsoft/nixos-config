{ pkgs, lib, config, ... }:
with (pkgs.my-lib.thmDec config.themes.colors); {
  home-manager.users.balsoft = {
    home.packages = [
      (if config.deviceSpecific.isPhone then
        pkgs.okularMobile
      else
        pkgs.kdePackages.okular)
    ];
    xdg.configFile."okularpartrc".text = pkgs.my-lib.genIni {
      "Dlg Accessibility" = {
        RecolorBackground = base00;
        RecolorForeground = base05;
      };
      "Document" = {
        ChangeColors = true;
        PaperColor = base00;
        RenderMode = "Recolor";
      };
      "Main View" = { ShowLeftPanel = false; };
      PageView = {
        BackgroundColor = base00;
        UseCustomBackgroundColor = true;
      };
    };
  };
}
