{ pkgs, config, lib, ... }: {
  console.font = "cyr-sun16";
  console.keyMap = "ruwin_cplk-UTF-8";

  time.timeZone = "Europe/Moscow"; # Mother Russia
  home-manager.users.balsoft.home.language = let
    en = "en_GB.UTF-8";
    ru = "ru_RU.UTF-8";
  in {
    address = ru;
    monetary = ru;
    paper = ru;
    time = en;
    base = en;
  };
}
