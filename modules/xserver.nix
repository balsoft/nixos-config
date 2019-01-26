{pkgs, lib, config, ...}:
let cpu = config.devices.${config.device}.cpu;
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
      greeters.gtk = {
        enable = isShared;
        iconTheme = {
          package = pkgs.papirus-icon-theme;
          name = "Papirus-Dark";
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