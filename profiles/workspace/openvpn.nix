{ config, pkgs, ... }: let password-store = config.secretsConfig.password-store; in {
  secrets.tawasal_eu1 = {
    encrypted = "${password-store}/openvpn/tawasal_eu1.gpg";
    services = [ "openvpn-tawasal-eu1.service" ];
  };
  secrets.tawasal_eu2 = {
    encrypted = "${password-store}/openvpn/tawasal_eu2.gpg";
    services = [ "openvpn-tawasal-eu2.service" ];
  };

  services.openvpn.servers = {
    tawasal-eu1.config = "config ${config.secrets.tawasal_eu1.decrypted}";
    tawasal-eu2.config = "config ${config.secrets.tawasal_eu2.decrypted}";
  };
}
