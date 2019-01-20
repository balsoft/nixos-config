{pkgs, lib, config, ...}:
{
  home-manager.users.balsoft =
  {
    home.language = let base = "en_GB.UTF-8"; rest = "ru_RU.UTF-8"; in
    {
      address = rest;
      monetary = rest;
      paper = rest;
      time = rest;
      base = base;
    };
    services.udiskie.enable = true;
    programs.git =
    {
      enable = true;
      userEmail = "balsoft@yandex.ru";
      userName = "Александр Бантьев";
    };
    news.display = "silent";
    programs.command-not-found.enable = true;
    home.keyboard =
    {
      options = ["grp:caps_toggle,grp_led:caps"];
      layout = "us,ru";
    };
    programs.ssh =
    {
      enable = true;
      matchBlocks =
      {
        "*" =
        {
          identityFile = toString (pkgs.writeTextFile
          {
            name = "id_rsa";
            text = config.secrets.id_rsa;
          });
          extraOptions.Ciphers = "aes128-gcm@openssh.com";
          compression = false;
        };
      };
    };
  };
}
