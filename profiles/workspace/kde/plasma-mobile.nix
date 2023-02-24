{ config, pkgs, lib, ... }: {

  environment.sessionVariables = {
    _JAVA_AWT_WM_NONREPARENTING = "1";
    XDG_SESSION_TYPE = "wayland";
    QT_QPA_PLATFORM = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
  };

  nixpkgs.overlays = [
    (final: prev: {
      libsForQt5 = prev.libsForQt5 // {
        kwallet = null;
        kwallet-pam = null;
        kwalletmanager = null;
      };
    })
  ];

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

  programs.feedbackd.enable = true;

  services.upower.enable = true;

  services.geoclue2.enable = true;

  home-manager.users.balsoft = {
    home.activation.removeGtkRc = {
      data = "rm -f $HOME/.gtkrc-2.0";
      before = [ "checkLinkTargets" ];
      after = [ ];
    };

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
        vibrationsEnabled = "true";
        navigationPanelEnabled = "false";
        taskSwitcherPreviewsEnabled = "false";
        animationsEnabled = "false";
      };
      QuickSettings = {
        disabledQuickSettings = builtins.concatStringsSep "," [ ];
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
          "org.kde.plasma.quicksetting.record"
        ];
      };
    };
    xdg.configFile."plasmaparc".text =
      lib.generators.toGitINI { General.VolumeStep = 2; };
  };
}
