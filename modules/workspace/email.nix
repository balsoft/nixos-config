{ pkgs, config, ... }: {
  home-manager.users.balsoft = {
    accounts.email.accounts."${config.secrets.gmail.user}@gmail.com" = {
      address = "${config.secrets.gmail.user}@gmail.com";
      flavor = "gmail.com";
      passwordCommand = "${pkgs.coreutils}/bin/echo ${config.secrets.gmail.password}";
      realName = "Alexander Bantyev";
      primary = true;
      userName = config.secrets.gmail.user;
      getmail = {
        readAll = false;
        enable = true;
        mailboxes = [ "INBOX" "Junk" "Trash" ];
      };
      msmtp = { enable = true; };
      imapnotify = {
        enable = true;
        boxes = [ "INBOX" ];
        onNotify = "${pkgs.libnotify}/bin/notify-send 'New mail!' '%s'";
      };
      signature = {
        showSignature = "append";
        text = ''
          Александр Бантьев /Alexander Bantyev/ aka balsoft

          Nix, NixOS DevOPS/SRE at serokell.io

          <balsoft75@gmail.com>
          <alexander.bantyev@serokell.io>

          https://matrix.to/#/@balsoft:balsoft.ru
          https://t.me/balsoft
          https://github.com/balsoft
        '';
      };
    };
    services = {
      getmail = {
        enable = true;
        frequency = "*-*-* *:*:00,15,30,45";
      };
      imapnotify.enable = true;
    };
  };
}
