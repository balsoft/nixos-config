{ config, pkgs, ... }: {
  environment.systemPackages = [ pkgs.plasma5Packages.angelfish ];
  defaultApplications.browser = {
    cmd = "${pkgs.plasma5Packages.angelfish}/bin/angelfish";
    desktop = "org.kde.angelfish";
  };
  home-manager.users.balsoft = {
    xdg.configFile.angelfishrc.text = pkgs.lib.generators.toGitINI {
      NavigationBar = {
        navBarReload = true;
      };
    };
  };
}
