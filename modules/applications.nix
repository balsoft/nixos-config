{pkgs, config, lib, ...}:
with import ../support.nix {inherit lib config;};
{
  options.defaultApplications = lib.mkOption
  {
    type = lib.types.attrs;
    description = "Preferred applications";
  };
  config = rec
  {
    defaultApplications =
    {
      term =
      {
        cmd = "${pkgs.kdeApplications.konsole}/bin/konsole";
        desktop = "konsole";
      };
      editor =
      {
        cmd = "${pkgs.emacs}/bin/emacsclient -c -n";
        desktop = "emacs";
      };
      browser =
      {
        cmd = "${pkgs.firefox}/bin/firefox";
        desktop = "firefox";
      };
      fm =
      {
        cmd = "${pkgs.dolphin}/bin/dolphin";
        desktop = "dolphin";
      };
      monitor =
      {
        cmd = "${pkgs.ksysguard}/bin/ksysguard";
        desktop = "ksysguard";
      };
      torrent =
      {
        cmd = "${pkgs.ktorrent}/bin/ktorrent";
        desktop = "ktorrent";
      };
      archive =
      {
        cmd = "${pkgs.ark}/bin/ark";
        desktop = "org.kde.ark";
      };
      mail =
      {
        cmd = "${pkgs.trojita}/bin/trojita";
        desktop = "trojita";
      };
    };
    home-manager.users.balsoft.xdg.configFile."mimeapps.list.home".text = with defaultApplications;
    genIni
    {
    "Default Applications" = builtins.mapAttrs (name: value: "${value.desktop}.desktop") {
          "text/html" = browser;
          "image/*" = {desktop = "org.kde.gwenview";};
          "application/x-bittorrent" = torrent;
          "application/zip" = archive;
          "application/rar" = archive;
          "application/7z" = archive;
          "application/*tar" = archive;
          "application/x-kdenlive" = archive;
          "x-scheme-handler/http" = browser;
          "x-scheme-handler/https" = browser;
          "x-scheme-handler/about" = browser;
          "x-scheme-handler/unknown" = browser;
          "x-scheme-handler/mailto" = mail;
          "application/pdf" = {desktop = "org.kde.okular";};
        };
      };
      
       home-manager.users.balsoft.home.activation.mimeapps =
       {
       before = [];
       after = ["linkGeneration"];
       data = "$DRY_RUN_CMD cp ~/.config/mimeapps.list.home ~/.config/mimeapps.list";
     };
  };
}
