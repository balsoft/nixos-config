{ pkgs, config, lib, ... }:
with import ../../support.nix { inherit lib config; }; {
  home-manager.users.balsoft.xdg.configFile."flaska.net/trojita.conf".text =
  if !isNull config.secrets.mail then
    genIni {
      General = {
        "app.updates.checkEnabled" = false;
        "imap.auth.user" = "${config.secrets.mail.user}@${config.secrets.mail.host}";
        "imap.auth.pass" = config.secrets.mail.password;
        "imap.host" = config.secrets.mail.host;
        "imap.archiveFolderName" = "Archive";
        "imap.startmode" = "ONLINE";
        "imap.method" = "SSL";
        "imap.needsNetwork" = true;
        "imap.numberRefreshInterval" = 300;
        "imap.port" = 993;
        "imap.proxy.system" = true;
        "imap.starttls" = true;
        "imapIdleRenewal" = 29;
        "msa.method" = "SMTP";
        "msa.smtp.auth" = true;
        "msa.smtp.auth.reuseImapCredentials" = true;
        "msa.smtp.burl" = false;
        "msa.smtp.starttls" = true;
        "msa.smtp.host" = config.secrets.mail.host;
        "msa.smtp.port" = 25;
        "offline.cache" = "days";
        "offline.cache.numDays" = "30";
      };
      autoMarkRead = {
        enabled = true;
        seconds = 0;
      };
      composer = {
        imapSentName = "Sent";
        saveToImapEnabled = true;
      };
      gui = {
        "mainWindow.layout" = "compact";
        preferPlaintextRendering = true;
        showSystray = false;
        startMinimized = false;
      };
      identities = {
        "1\\address" = "${config.secrets.mail.user}@${config.secrets.mail.host}";
        "1\\organisation" = "serokell.io";
        "1\\realName" = "Alexander Bantyev";
        "1\\signature" = ''\x410\x43b\x435\x43a\x441\x430\x43d\x434\x440 \x411\x430\x43d\x442\x44c\x435\x432 /Alexander Bantyev/ aka balsoft\n\nNix DevOPS/SRE at serokell.io\n\n<balsoft@balsoft.ru>\n<alexander.bantyev@serokell.io>\n\nmatrix://@balsoft:balsoft.ru \n(https://matrix.to/#/@balsoft:balsoft.ru)\nhttps://t.me/balsoft\nhttps://github.com/balsoft\n'';
        size = 1;
      };
      interoperability.revealVersions = true;
      plugin = {
        addressbook = "abookaddressbook";
        password = "cleartextpassword";
      };
    }
  else
    "";
}
