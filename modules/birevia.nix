{ pkgs, config, ... }: {
  environment.etc."ppp/peers/birevia" = {
    enable = config.device == "AMD-Workstation";
    text = ''
      remotename PPTP
      require-mschap-v2
      require-mppe
      persist
      nodefaultroute
      lcp-echo-failure 4
      lcp-echo-interval 30
      maxfail 0
      mtu 1450
      mru 1450
      pty "pptp ${config.secrets.birevia.ip} --nolaunchpppd"
      name ${config.secrets.birevia.user}
      password ${config.secrets.birevia.password}
      linkname birevia
      lock
      noauth
      refuse-pap
      refuse-eap
      refuse-chap
      refuse-mschap
      nobsdcomp
      nodeflate
    '';
  };

  systemd.services.birevia = {
    enable = config.device == "AMD-Workstation";
    path = with pkgs; [ ppp pptp nettools ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "root";
      Type = "forking";

      ExecStart = pkgs.writeTextFile {
        name = "birevia";
        executable = true;
        text = ''
          #!${pkgs.bash}/bin/bash
          pppd call birevia updetach
          route add -net 172.17.1.0 netmask 255.255.255.0 gw ${config.secrets.birevia.ip}
        '';
      };
    };

  };
  systemd.services.birevia-check = {
    enable = config.device == "AMD-Workstation";
    path = with pkgs; [ ppp systemd ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = "root";
      Restart = "always";
      RestartSec = "60";
      ExecStart = pkgs.writeTextFile {
        name = "birevia";
        executable = true;
        text = ''
          #!${pkgs.bash}/bin/bash

          echo -n "Checking if we are already connected... "
          /run/wrappers/bin/ping 172.17.1.1 -c 1 > /dev/null
          if [ "$?" = 0 ]
          then
            echo "We are, exiting"
            exit 0
          fi
          echo "We are not, connecting..."
          poff birevia
          pkill -9 pppd
          pkill -9 pptp
          systemctl restart birevia.service
        '';
      };
    };
  };
}
