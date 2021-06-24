{ config, pkgs, lib, ... }: {
  secrets.email-mastodon = {
    owner = "mastodon:mastodon";
    encrypted = "/home/balsoft/.password-store/email/mastodon@balsoft.ru.gpg";
    services = [ "mastodon-web" ];
  };
  services.mastodon = {
    enable = true;
    configureNginx = true;
    localDomain = "social.balsoft.ru";
    smtp = {
      createLocally = false;
      fromAddress = "mastodon@balsoft.ru";
      user = "mastodon";
      host = "balsoft.ru";
      passwordFile = config.secrets.email-mastodon.decrypted;
    };
  };
}
