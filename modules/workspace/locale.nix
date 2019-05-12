{ pkgs, config, lib, ... }: {
  i18n = { defaultLocale = "en_GB.UTF-8"; };

  time.timeZone = "Europe/Moscow"; # Mother Russia
  home-manager.users.balsoft.home.language = let
    base = "en_GB.UTF-8";
    rest = "ru_RU.UTF-8";
  in {
    address = rest;
    monetary = rest;
    paper = rest;
    time = rest;
    base = base;
  };
}
