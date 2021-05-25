{ config, pkgs, inputs, lib, ... }: {
  environment.systemPackages = [ inputs.himalaya.defaultPackage.x86_64-linux ];
  home-manager.users.balsoft = {
    xdg.configFile."himalaya/config.toml".text = pkgs.my-lib.genIni {
      default = {
        name = "Alexander Bantyev";
        signature = "Regards,";
        downloads-dir = "'/home/balsoft/Downloads/mail'";
      };
      maiol = {
        default = true;
        email = "balsoft@balsoft.ru";
        imap-host = "balsoft.ru";
        imap-login = "balsoft@balsoft.ru";
        imap-passwd-cmd = "cat ${config.secrets.email.decrypted}";
        imap-port = 993;
        smtp-host = "balsoft.ru";
        smtp-login = "balsoft@balsoft.ru";
        smtp-passwd-cmd = "cat ${config.secrets.email.decrypted}";
        smtp-port = 465;
      };
    };
  };
}
