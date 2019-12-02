{config, ...}:
{
  home-manager.users.balsoft.accounts.email.accounts."${config.secrets.gmail.user}@gmail.com" = {
    address = "${config.secrets.gmail.user}@gmail.com";
    flavor = "gmail.com";
    passwordCommand = "echo '${config.secrets.gmail.password}'";
    realName = "Alexander Bantyev";
    primary = true;
    userName = config.secrets.gmail.user;
    getmail = {
      enable = true;
      mailboxes = [ "INBOX" "Sent" ];
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
}
