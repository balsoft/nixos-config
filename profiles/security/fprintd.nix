{ config, ... }: {
  services.fprintd = {
    enable = true;
    enablePam = true;
  };

  persist.state.directories = [ "/var/lib/fprint" ];

  security.pam.services.sudo.fprintAuth = true;

  security.pam.services.sudo.rules.auth.u2f.order =
    config.security.pam.services.sudo.rules.auth.fprintd.order + 1;

}
