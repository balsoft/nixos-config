{ pkgs, config, lib, ... }:
{
  services.gitea = {
    enable = true;
    appName = "code.balsoft.ru";
    cookieSecure = true;
    rootUrl = "https://code.balsoft.ru";
    domain = "code.balsoft.ru";
    httpPort = 6000;
    # disableRegistration = true;
  };
}
