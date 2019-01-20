{pkgs, config, lib, ...}:
{
  options.defaultApplications = lib.mkOption
  {
    type = lib.types.attrs;
    description = "Preferred applications";
  };
  config =
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
    };
  };
}
