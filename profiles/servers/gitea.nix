{ pkgs, config, lib, ... }:
{
  services.gitea = {
    enable = false;
    appName = "code.balsoft.ru";
    settings.server = {
      HTTP_PORT = 6000;
      ROOT_URL = "https://code.balsoft.ru";
      DISABLE_REGISTRATION = true;
      COOKIE_SECURE = true;
      DOMAIN = "code.balsoft.ru";
    };
  };
}
