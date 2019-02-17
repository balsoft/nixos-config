{pkgs, lib, config, ...}:
let cpu = config.deviceSpecific.cpu;
    device = config.device;
    isShared = config.deviceSpecific.isShared;
in
{
  services.xserver = {
        enable = true;
        enableTCP = true;
    libinput = {
      enable = true;
      sendEventsMode = "disabled-on-external-mouse";
      middleEmulation = false;
      naturalScrolling = true;
    };
    videoDrivers = if
                     cpu == "amd"
                   then
                     [ "amdgpu" ]
                   else if
                     device == "Lenovo-Workstation"
                   then
                     [ "radeon" ]
                   else
                     [ "intel" ];
    displayManager.lightdm = {
      enable = true;
      autoLogin.enable = !isShared;
      autoLogin.user = "balsoft";
      greeter.enable = isShared;
      background = "222222";
      greeters.enso = {
        enable = isShared;
        cursorTheme =
        {
          package = pkgs.breeze-qt5;
          name = "Breeze";
        };
        iconTheme =
        {
          package = pkgs.papirus-icon-theme;
          name = "Papirus-Dark";
        };
        theme =
        {
          package = pkgs.generated-gtk-theme;
          name = "Generated";
        };
      };
    };
#   desktopManager.plasma5.enable = true;
    desktopManager.default = "none";
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
    #windowManager.xmonad.enable = true;
    windowManager.default = "i3";
    layout = "us,ru";
    xkbOptions = "grp:caps_toggle,grp_led:caps";
  };
}
