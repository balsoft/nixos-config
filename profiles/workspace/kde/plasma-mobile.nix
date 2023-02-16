{ config, pkgs, lib, ... }: {

  services.xserver = {
    enable = true;
    desktopManager.plasma5 = {
      mobile.enable = true;
      runUsingSystemd = false;
    };
    displayManager = {
      autoLogin = {
        enable = true;
        user = "balsoft";
      };
      defaultSession = "plasma-mobile";
      lightdm = {
        enable = true;
        extraSeatDefaults = ''
          session-cleanup-script=${pkgs.procps}/bin/pkill -P1 -fx ${pkgs.lightdm}/sbin/lightdm
        '';
      };
    };
    libinput.enable = true;
  };

  powerManagement.enable = true;

  home-manager.users.balsoft = {
    xdg.configFile."autostart/org_kde_powerdevil.desktop".text = ''
      [Desktop Entry]
      DBusActivatable=true
      Exec=${pkgs.powerdevil}/libexec/org_kde_powerdevil
      Name=org_kde_powerdevil
      Type=Application
    '';
    xdg.configFile."plasmarc".text =
      lib.generators.toGitINI { Theme.name = "generated"; };
    xdg.configFile."plasmamobilerc".text = lib.generators.toGitINI {
      General = {
        actionDrawerTopLeftMode = "1";
        actionDrawerTopRightMode = "0";
        vibrationDuration = "100";
        vibrationIntensity = "0.5";
      };
      QuickSettings = {
        disabledQuickSettings = builtins.concatStringsSep ","
          [ "org.kde.plasma.quicksetting.record" ];
        enabledQuickSettings = builtins.concatStringsSep "," [
          "org.kde.plasma.quicksetting.wifi"
          "org.kde.plasma.quicksetting.mobiledata"
          "org.kde.plasma.quicksetting.bluetooth"
          "org.kde.plasma.quicksetting.flashlight"
          "org.kde.plasma.quicksetting.screenrotation"
          "org.kde.plasma.quicksetting.settingsapp"
          "org.kde.plasma.quicksetting.airplanemode"
          "org.kde.plasma.quicksetting.audio"
          "org.kde.plasma.quicksetting.battery"
          "org.kde.plasma.quicksetting.location"
          "org.kde.plasma.quicksetting.nightcolor"
          "org.kde.plasma.quicksetting.screenshot"
          "org.kde.plasma.quicksetting.powermenu"
          "org.kde.plasma.quicksetting.donotdisturb"
          "org.kde.plasma.quicksetting.caffeine"
          "org.kde.plasma.quicksetting.keyboardtoggle"
        ];
      };
    };
  };
}
