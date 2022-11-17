{ config, pkgs, inputs, lib, ... }: {
  environment.systemPackages = [ pkgs.himalaya ];
  home-manager.users.balsoft = {
    xdg.configFile."himalaya/config.toml".text = ''
      downloads-dir="/home/balsoft/Downloads/mail"
      name="Alexander Bantyev"
      signature="Regards,"

      [balsoft]
      default=true
      email="balsoft@balsoft.ru"
      imap-host="balsoft.ru"
      imap-login="balsoft@balsoft.ru"
      imap-passwd-cmd="cat /var/secrets/email"
      imap-port=993
      smtp-host="balsoft.ru"
      smtp-login="balsoft@balsoft.ru"
      smtp-passwd-cmd="cat /var/secrets/email"
      smtp-port=465
    '';
  };
}
