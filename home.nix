
device: {pkgs, lib,  ...}:
with import ./support.nix { inherit lib; };
with import ./common.nix device pkgs;
let
  thm = {
    bg = "#2e3440";
    fg = "#d8dee9";
    gray = "#7f8c8d";
    alt = "#4c566a";
    dark = "#3b4252";
    blue = "#5e81ac";
    green = "#a3be8c";
    red = "#bf616a";
    orange = "#d08770";
    yellow = "#ebcb8b";
    purple = "#b48ead";
    cyan = "#88c0d0";
  };
  

  term = "${pkgs.kdeApplications.konsole}/bin/konsole";

  secret = import ./secret.nix;

  scripts = import ./scripts {inherit pkgs; inherit secret; theme = thm; inherit device;};

  themes = import ./themes {inherit thm; inherit pkgs; inherit genIni;};

  customPackages = pkgs.callPackages ./packages {};

  browser = "${pkgs.firefox}/bin/firefox";


in
rec {
  

  

  
  
  home.packages = 
  (with pkgs; [
    # Internet
    wget
    curl
    chromium
  ] ++ (if goodMachine then [
    geany
    kdevelop
    kate
    texlive.combined.scheme-full
    steam
    krita
    kdenlive
    frei0r
    ffmpeg-full
    ghc
  ] else [] ) ++ [
    firefox
    # Messaging
    tdesktop
    telepathy_haze
    telepathy_idle
    libnotify
    # Audio/Video
    vlc
    google-play-music-desktop-player
    lxqt.pavucontrol-qt
    # Tools
    zip
    unrar
    wine
    kolourpaint
    ktorrent
    wireshark
    #wpsoffice
    arduino
    micro
    cmake
    gnumake
    gcc
    gdb
    python3
    qalculate-gtk
    libqalculate
    qt5ct
    breeze-qt5
    adwaita-qt
    gnome3.adwaita-icon-theme
    papirus-icon-theme
    breeze-icons
    units
    goldendict
    ksysguard
    scrot
    xclip
    abiword
    gnumeric
    kile
    gcalcli
    google-drive-ocamlfuse
    kdeconnect
    trojita
    nix-zsh-completions
    material-icons
    papirus-icon-theme
    kde-cli-tools
  ]) 
  ++ 
  (with customPackages; [
    vk
  ]);

  

  
  
  xdg = {
    enable = true;
    configFile = { 
      "kdeglobals".text = themes.kde;
      "konsolerc.home".text = genIni {
        "Desktop Entry".DefaultProfile = "Default.profile";
        KonsoleWindow.ShowMenuBarByDefault = false;
      };

      "katerc.home".text = genIni {
        General = {
          "Show Full Path in Title" = true;
          "Show Menu Bar" = false;
          "Show Status Bar" = true;
          "Show Tab Bar" = true;
        };
        "KTextEditor Renderer" = {
          "Animate Bracket Matching" = false;
          "Schema" = "Breeze Dark";
          "Show Indentation Lines" = true;
          "Show Whole Bracket Expression" = false;
          "Word Wrap Marker" = true;
        };
        UiSettings = {
          ColorScheme = "Nord";
        };
      };

      "kateschemarc".text = themes.kate;

      "flaska.net/trojita.conf".text = genIni {
        General = {
          "app.updates.checkEnabled" = false;
          "imap.auth.user" = secret.gmail.user;
          "imap.auth.pass" = secret.gmail.password;
          "imap.host" = "imap.gmail.com";
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
          "msa.smtp.host" = "smtp.gmail.com";
          "msa.smtp.port" = 587;
          "msa.smtp.starttls" = true;
          "offline.cache" = "days";
          "offline.cache.numDays" = "30";
        };
        autoMarkRead = { 
          enabled = true;
          seconds = 0;
        };
        composer = {
          imapSentName = "Sent";
          saveToImapEnabled = false;
        };
        gui = {
          "mainWindow.layout" = "one-at-time";
          preferPlaintextRendering = true;
          showSystray = false;
          startMinimized = false;
        };
        identities = {
          "1\\address" = "${secret.gmail.user}@gmail.com";
          "1\\organisation" = "";
          "1\\realName" = "Alexander Bantyev";
          "1\\signature" = "";
          size = 1;
        };
        interoperability.revealVersions = true;
        plugin = {
          addressbook = "abookaddressbook";
          password = "cleartextpassword";
        };
      };
      "mimeapps.list.home".text = genIni {
        "Default Applications" = let browser = "firefox.desktop"; in {
          "text/html" = browser;
          "image/*" = "org.kde.gwenview.desktop";
          "application/x-bittorrent" = "org.kde.ktorrent";
          "application/zip" = "org.kde.ark.desktop";
          "application/rar" = "org.kde.ark.desktop";
          "application/7z" = "org.kde.ark.desktop";
          "application/*tar" = "org.kde.ark.desktop";
          "application/x-kdenlive" = "org.kde.kdenlive.desktop";
          "x-scheme-handler/http" = browser;
          "x-scheme-handler/https" = browser;
          "x-scheme-handler/about" = browser;
          "x-scheme-handler/unknown" = browser;
          "x-scheme-handler/mailto" = "trojita.desktop";
          "application/pdf" = "org.kde.okular.desktop";
        };
      };
    };  

  };
  xdg.dataFile = {
    "Steam/skins/Metro".source = pkgs.fetchurl {
      url = "http://metroforsteam.com/downloads/4.3.1.zip";
      sha256 = "0e4e8bd6e164c60be7924d18ab29ddf966d31dd0db6a6820c213d25bc1a14bd2";
    };
    "konsole/Default.profile".text = genIni {
      Appearance.ColorScheme = "generated";
      "Cursor Options".CursorShape = 1;
      General = {
        Command = "zsh";
        Name = "Default";
        Parent = "FALLBACK/";
      };
      Scrolling.HistoryMode = 2;
      "Terminal Features".BlinkingCursorEnabled = true;
    };
    "konsole/generated.colorscheme".text = themes.konsole;

    "user-places.xbel.home".text = ''
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE xbel>
<xbel xmlns:kdepriv="http://www.kde.org/kdepriv" xmlns:bookmark="http://www.freedesktop.org/standards/desktop-bookmarks" xmlns:mime="http://www.freedesktop.org/standards/shared-mime-info">
<bookmark href="file:///home/balsoft">
<title>Home</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="user-home"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1539244233/0</ID>
  <isSystemItem>true</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="file:///home/balsoft/Google Drive">
<title>Google Drive</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="google-drive"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1540478729/2</ID>
  <isSystemItem>false</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="remote:/">
<title>Network</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="network-workgroup"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1539244233/2</ID>
  <isSystemItem>true</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="file:///">
<title>Root</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="folder-red"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1539244233/3</ID>
  <isSystemItem>true</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="trash:/">
<title>Trash</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="user-trash-full"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1539244233/4</ID>
  <isSystemItem>true</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="file:///home/balsoft/Downloads">
<title>Downloads</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="folder-downloads"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1539244233/1</ID>
  <isSystemItem>true</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="file:///home/balsoft/Documents/">
<title>Documents</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="folder-documents-symbolic"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1540414173/0</ID>
  <isSystemItem>false</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<info>
<metadata owner="http://www.kde.org">
<GroupState-Places-IsHidden>false</GroupState-Places-IsHidden>
<GroupState-Remote-IsHidden>false</GroupState-Remote-IsHidden>
<GroupState-Devices-IsHidden>false</GroupState-Devices-IsHidden>
<GroupState-RemovableDevices-IsHidden>false</GroupState-RemovableDevices-IsHidden>
<withBaloo>true</withBaloo>
<GroupState-SearchFor-IsHidden>false</GroupState-SearchFor-IsHidden>
<GroupState-RecentlySaved-IsHidden>false</GroupState-RecentlySaved-IsHidden>
</metadata>
</info>
<bookmark href="timeline:/today">
<title>Today</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="go-jump-today"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1539244233/5</ID>
  <isSystemItem>true</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="timeline:/yesterday">
<title>Yesterday</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="view-calendar-day"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1539244233/6</ID>
  <isSystemItem>true</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="search:/documents">
<title>Documents</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="folder-text"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1539244233/7</ID>
  <isSystemItem>true</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="search:/images">
<title>Images</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="folder-images"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1539244233/8</ID>
  <isSystemItem>true</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="search:/audio">
<title>Audio Files</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="folder-sound"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1539244233/9</ID>
  <isSystemItem>true</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="search:/videos">
<title>Videos</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="folder-videos"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1539244233/10</ID>
  <isSystemItem>true</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="">
<title>Project Folder</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="folder-favorites"/>
</metadata>
<metadata owner="http://www.kde.org">
  <OnlyInApp>kdenlive</OnlyInApp>
</metadata>
</info>
</bookmark>
<separator href="file:///">
<info>
<metadata owner="http://www.kde.org">
  <UDI>/org/freedesktop/UDisks2/block_devices/sda2</UDI>
  <isSystemItem>true</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="drive-harddisk"/>
</metadata>
</info>
<title>Linux filesystem</title>
</separator>
<bookmark href="file:///home/balsoft/Videos/">
<title>Videos</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="folder-videos-symbolic"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1540409511/0</ID>
  <isSystemItem>false</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="file:///home/balsoft/Pictures">
<title>Pictures</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="folder-pictures-symbolic"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1540409539/1</ID>
  <isSystemItem>false</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="file:///home/balsoft/projects">
<title>projects</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="user-bookmarks-symbolic"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1540463794/11</ID>
  <isSystemItem>false</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="timeline:/calendar/">
<title>Calendar</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="view-calendar-timeline"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1540478496/0</ID>
  <isSystemItem>false</isSystemItem>
  <IsHidden>false</IsHidden>
</metadata>
</info>
</bookmark>
<bookmark href="fish://asus-laptop">
<title>ASUS</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="laptop"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1540585036/0</ID>
</metadata>
</info>
</bookmark>
<bookmark href="fish://hp-laptop">
<title>HP</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="laptop"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1540585062/1</ID>
</metadata>
</info>
</bookmark>
<bookmark href="fish://prestigio-laptop">
<title>Prestigio</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="laptop"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1540585084/2</ID>
</metadata>
</info>
</bookmark>
<bookmark href="fish://lenovo-workstation">
<title>Lenovo</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="computer"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1540585120/3</ID>
</metadata>
</info>
</bookmark>
<bookmark href="fish://192.168.1.1">
<title>Router</title>
<info>
<metadata owner="http://freedesktop.org">
  <bookmark:icon name="network-server-symbolic"/>
</metadata>
<metadata owner="http://www.kde.org">
  <ID>1540585202/4</ID>
</metadata>
</info>
</bookmark>
</xbel>'';
  };
  #xdg.dataFile."albert/org.albert.extension.python/modules/qalc.py".text = scripts.albert.qalc;
  #xdg.dataFile."albert/org.albert.extension.python/modules/nix.py".text = scripts.albert.nix;
  #xdg.dataFile."albert/org.albert.extension.python/modules/translate.py".text = scripts.albert.translate;
  home.file.".icons/default".source = "${pkgs.breeze-qt5}/share/icons/breeze_cursors";
 home.file.".mozilla/firefox/profiles.ini".text = genIni {
    General.StartWithLastProfile = 1;
    Profile0 = {
      Name = "default";
      IsRelative = 1;
      Path = "profile.default";
      Default = 1;
    };
  };
  home.file.".mozilla/firefox/profile.default/user.js".text = ''
  pref("extensions.autoDisableScopes", 0);
  pref("browser.search.defaultenginename", "Google");
  pref("browser.search.selectedEngine", "Google");
  pref("browser.uidensity", 1);
  pref("browser.search.openintab", true);
  pref("accessibility.browsewithcaret", true);
  '';
  home.file.".mozilla/firefox/profile.default/chrome/userChrome.css".text = ''
  #TabsToolbar {
    visibility: collapse;
  }
  toolbar#nav-bar, nav-bar-customization-target {
  background: ${thm.bg} !important;
  }
  @-moz-document url("about:newtab") {
  body { background-color: ${thm.bg}  !important;}
  }
  '';
  home.file.".mozilla/firefox/profile.default/extensions/uBlock0@raymondhill.net.xpi".source =
  builtins.fetchurl {
    url = "https://addons.mozilla.org/firefox/downloads/file/1166954/ublock_origin-1.17.4-an+fx.xpi";
    sha256 = "54c9a1380900eb1eba85df3a82393cef321e9c845fda227690d9377ef30e913e";
  };
  home.file.".mozilla/firefox/profile.default/extensions/{c9f848fb-3fb6-4390-9fc1-e4dd4d1c5122}.xpi".source =
  builtins.fetchurl {
    url = "https://addons.mozilla.org/firefox/downloads/file/883289/no_tabs-1.1-an+fx-linux.xpi";
    sha256 = "48e846a60b217c13ee693ac8bfe23a8bdef2ec073f5f713cce0e08814f280354";
  };
  home.file.".mozilla/firefox/profile.default/extensions/keepassxc-browser@keepassxc.org.xpi".source =
  builtins.fetchurl {
    url = "https://addons.mozilla.org/firefox/downloads/file/1205950/keepassxc_browser-1.3.2-fx.xpi";
    sha256 = "8a9c13f36b6ea8c5287ea6f99a8a9dc8c28b615c529e44d630221c03aee26790";
  };
  home.activation = builtins.mapAttrs (name: value: {inherit name; before = []; after = [ "linkGeneration" ];} // value) {
    konsole.data = "$DRY_RUN_CMD cp ~/.config/konsolerc.home ~/.config/konsolerc";
    kate.data = "$DRY_RUN_CMD cp ~/.config/katerc.home ~/.config/katerc";
    user-places.data = "$DRY_RUN_CMD cp ~/.local/share/user-places.xbel.home ~/.local/share/user-places.xbel";
    mimeapps.data= "$DRY_RUN_CMD cp ~/.config/mimeapps.list.home ~/.config/mimeapps.list";
  };

  
    
}
