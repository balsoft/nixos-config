{pkgs, config, lib, ...}:
with (import ../../support.nix {inherit lib config;});
{
  home-manager.users.balsoft = {
    xdg.configFile = {
      "katerc.home".text = genIni {
        General = {
          "Show Full Path in Title" = true;
          "Show Menu Bar" = false;
          "Show Status Bar" = true;
          "Show Tab Bar" = true;
        };
        "KTextEditor Renderer" = {
          "Animate Bracket Matching" = false;
          "Schema" = "Breeze Dark";
          "Show Indentation Lines" = true;
          "Show Whole Bracket Expression" = false;
          "Word Wrap Marker" = true;
        };
        UiSettings = {
          ColorScheme = "Nord";
        };
      };

      "kateschemarc".text = genIni {
        "Breeze Dark"."Color Background" = thmDec.bg;
      };
    };
#    home.packages = [ pkgs.kate ];
    home.activation.konsole =
    {
      data = "$DRY_RUN_CMD cp -f ~/.config/katerc.home ~/.config/katerc";
      before = [];
      after = ["linkGeneration"];
    };
  };
}
