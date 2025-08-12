{ pkgs, config, lib, ... }: {
  environment.sessionVariables = {
    XKB_DEFAULT_LAYOUT = "us,ru";
    XKB_DEFAULT_OPTIONS =
      "grp:lctrl_toggle,grp_led:caps,ctrl:nocaps,compose:ralt";
    LANG = lib.mkForce "C.UTF-8";
    LC_ALL = "C.UTF-8";
    XCOMPOSEFILE = "${config.home-manager.users.balsoft.xdg.configHome}/XCompose";
  };

  i18n.defaultLocale = "C.UTF-8";

  time.timeZone = "Asia/Tbilisi";
  home-manager.users.balsoft = {
    home.file.".XCompose".source = ./compose;
    xdg.configFile."gtk-3.0/Compose".source = ./compose;
    xdg.configFile."XCompose".source = ./compose;
    home.language = let
      C = "C.UTF-8";
      ru = "ru_RU.UTF-8";
    in {
      address = C;
      monetary = C;
      paper = ru;
      time = C;
      base = C;
    };
  };
}
