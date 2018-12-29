
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

  customPackages = import ./packages {inherit pkgs;};

  polybar_left = with scripts.polybar; [ 
    (weather { city-id = "513378"; city = "Ozery"; }) 
    (time {})
    (now {})
    (next {}) 
    (email { user = secret.gmail.user; password = secret.gmail.password; }) 
  ];

  polybar_right = with scripts.polybar; [
    (status {})
  ] ++ (lib.optionals isLaptop [
    (brightness { inherit device; })
  ]) ++ (lib.optionals (device != "Prestigio-Laptop") [
    (sound {})
  ]) ++ (if (isLaptop && device != "Prestigio-Laptop") then [
    (battery {})
  ] else []) ++ [
    (network {})
  ];

in
rec {
  programs.home-manager = {
    enable = true;
    path = https://github.com/rycee/home-manager/archive/master.tar.gz;
  };
  
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = programs.zsh.enableAutosuggestions;
    oh-my-zsh = {
      enable = true;
      theme = "agnoster";
      plugins = [
        "git"
        "dirhistory"
      ];
    };
    shellAliases = {
      "p" = "nix-shell --run zsh -p";
      "r" = "_r(){nix run nixpkgs.$1 -c $@};_r";
      "b" = "nix-build \"<nixpkgs>\" --no-out-link -A";
    };
    initExtra = scripts.zshrc;
  };

  gtk = {
    enable = true;
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
  };
  home.language = let base = "en_GB.UTF-8"; rest = "ru_RU.UTF-8"; in {
    address = rest;
    monetary = rest;
    paper = rest;
    time = rest;
    base = base;
  };

  xsession.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    config = rec {
      assigns = {
        "" = [{ class = "Chromium"; }];
        "" = [{ class = "^Telegram"; } { class = "^VK"; } { class = "^trojita"; } ];
      };
      bars = [];
      fonts = [ "RobotoMono 9" ];
      colors = rec{
        background = thm.bg;
        unfocused = {
          text = thm.fg;
          border = thm.bg;
          background = thm.alt;
          childBorder = thm.alt;
          indicator = thm.fg;
        };
        focusedInactive = unfocused;
        urgent = unfocused // {
          text = thm.fg;
          border = thm.orange;
          childBorder = thm.orange;
        };
        focused = unfocused // {
          childBorder = thm.blue;
          background = thm.bg;
          text = thm.fg;
        };
      };
      gaps = {
        inner = 6;
        smartGaps = true;
      };
      focus.mouseWarping = true;
      modifier = "Mod4";
      window = {
        border = 1;
        titlebar = false;
        hideEdgeBorders = "smart";
        commands = [ 
          {
            command = "focus";
            criteria = { urgent = "latest"; };
          } 
          {
            command = "border pixel 1px";
            criteria = { window_role = "popup"; };
          } 
        ];
      };
      startup = [
        { command = "${pkgs.albert}/bin/albert"; always = true; }
        { command = "${pkgs.tdesktop}/bin/telegram-desktop"; }
        { command = "${pkgs.chromium}/bin/chromium"; }
        { command = "${customPackages.vk}/bin/vk"; }
        { command = "emacs  --daemon"; }
        { command = "${pkgs.kdeconnect}/lib/libexec/kdeconnectd"; }
        { command = "${pkgs.polkit-kde-agent}/lib/libexec/polkit-kde-authentication-agent-1"; }
        { command = "dunst"; }
        { command = ''exec ${pkgs.writeTextFile { name = "start_scripts"; text = scripts.polybar.start_scripts (polybar_left ++ polybar_right); executable = true;}}''; }
        { command = "balooctl start"; }
        { command = "${pkgs.autorandr}/bin/autorandr --force horizontal"; always = true; }
        #{ command = "google-drive-ocamlfuse '/home/balsoft/Google Drive/'"; }
        #{ command = "pkill compton; allow_rgb10_configs = false ${pkgs.compton}/bin/compton --backend glx --vsync opengl-swc"; always = true;}
        { command = "trojita"; } 
        { command = term; workspace = "0"; }
        { command = "google-drive-ocamlfuse -headless -f '/home/balsoft/Google Drive'"; }
        { command = "${pkgs.hsetroot}/bin/hsetroot -solid '${thm.bg}'"; always = true; }
      ];
      keybindings = let moveMouse = ''"sh -c 'eval `${pkgs.xdotool}/bin/xdotool getactivewindow getwindowgeometry --shell`; ${pkgs.xdotool}/bin/xdotool mousemove $((X+WIDTH/2)) $((Y+HEIGHT/2))'"''; in
      ({
        "${modifier}+q" = "kill";
        "${modifier}+Return" = "exec ${term}";
        "${modifier}+e" = "exec ${editor} -c -n -e '(switch-to-buffer nil)'";
        "${modifier}+l" = "layout toggle";
        "${modifier}+Left" = "focus left; exec ${moveMouse}";
        "${modifier}+Right" = "focus right; exec ${moveMouse}";
        "${modifier}+Up" = "focus up; exec ${moveMouse}";
        "${modifier}+Down" = "focus down; exec ${moveMouse}";
        "${modifier}+Shift+Up" = "move up";
        "${modifier}+Shift+Down" = "move down";
        "${modifier}+Shift+Right" = "move right";
        "${modifier}+Shift+Left" = "move left";
        "${modifier}+f" = "fullscreen toggle";
        "${modifier}+r" = "mode resize";
        "${modifier}+Shift+f" = "floating toggle";
        "${modifier}+d" = "exec ${pkgs.dolphin}/bin/dolphin";
        "${modifier}+Escape" = "exec ${pkgs.ksysguard}/bin/ksysguard";
        "${modifier}+Print" = "exec scrot -e 'mv $f ~/Pictures && notify-send \"Screenshot saved as ~/Pictures/$f\"'";
        "${modifier}+Control+Print" = "exec scrot -e 'xclip -selection clipboard -t image/png -i $f && notify-send \"Screenshot copied to clipboard\" && rm $f'";
        "--release ${modifier}+Shift+Print" = "exec scrot -s -e 'mv $f ~/Pictures && notify-send \"Screenshot saved as ~/Pictures/$f\"'";
        "--release ${modifier}+Control+Shift+Print" = "exec scrot -s -e 'xclip -selection clipboard -t image/png -i $f && notify-send \"Screenshot copied to clipboard\" && rm $f'";
        "${modifier}+x" = "move workspace to output right"; 
        "${modifier}+z" = "exec ${pkgs.i3-easyfocus}/bin/i3-easyfocus";
        "${modifier}+c" = "workspace ";
        "${modifier}+Shift+c" = "move container to workspace ";
        "${modifier}+t" = "workspace ";
        "${modifier}+Shift+t" = "move container to workspace ";
        "${modifier}+k" = "exec ${pkgs.xorg.xkill}/bin/xkill";
        "${modifier}+F5" = "restart";
        "${modifier}+Shift+F5" = "exit";
        "${modifier}+h" = "layout splith";
        "${modifier}+v" = "layout splitv";
      } // builtins.listToAttrs (
        builtins.genList (x: {name = "${modifier}+${toString x}"; value = "workspace ${toString x}";}) 10
      ) // builtins.listToAttrs (
        builtins.genList (x: {name = "${modifier}+Shift+${toString x}"; value = "move container to workspace ${toString x}";}) 10
      ));
      keycodebindings = {
        "122" = "exec ${pkgs.pamixer}/bin/pamixer -d 5";
        "123" = "exec ${pkgs.pamixer}/bin/pamixer -i 5";
        "121" = "exec ${pkgs.pamixer}/bin/pamixer -t";
      };
    };
  };


  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      i3Support = true;
      alsaSupport = false;
      nlSupport = false;
    };
    config = {
      "bar/top" = {
        font-0 = "Roboto Mono for Powerline:size=" + (if smallScreen then "8;1" else "11;2");
        font-3 = "Roboto Mono for Powerline:size=" + (if smallScreen then "18;4" else "24;5");
        font-1 = "Noto Sans Symbols2:size=15;4";
        font-2 = "Noto Emoji:size=" + (if smallScreen then "8;1" else "11;2");
        font-4 = "Unifont:size=" + (if smallScreen then "8;1" else "11;2");
        font-5 = "Material Icons:size=" + (if smallScreen then "10;2" else "16;4");
        width = "100%";
        height = if smallScreen then "19px" else "25px";
        radius = 0;
        background = thm.bg;
        foreground = thm.fg;
        modules-left = "left_side";
        modules-center = "i3";
        modules-right = "right_side";
        tray-position = "none";
        monitor = "\${env:MONITOR:}";
      };
      "module/i3" = {
        type = "internal/i3";
        label-focused-foreground = thm.blue;
        label-urgent-foreground = thm.orange;
        pin-workspaces = true;
      };

      "module/left_side" = {
        type = "custom/script";
        exec = toString (scripts.polybar.left_side (polybar_left));
        tail = true;
      };

      "module/right_side" = {
        type = "custom/script";
        exec = toString (scripts.polybar.right_side (polybar_right));
        tail = true;
      };
    };
    script = "";
  };

  services.dunst = {
    enable = true;
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
    settings = {
      global = {
        geometry = "500x5-30+50";
        transparency = 10;
        frame_color = thm.blue;
        font = "Roboto Mono 13";
        padding = 15;
        horizontal_padding = 17;
        word_wrap = true;
      };
      
      urgency_low = {
        background = thm.bg;
        foreground = thm.fg;
        timeout = 5;
      };
      
      urgency_normal = {
        background = thm.alt;
        foreground = thm.fg;
        timeout = 10;
      };
      
      urgency_critical = {
        background = thm.fg;
        foreground = thm.bg;
        timeout = 15;
      };
    };
  };
  programs.autorandr = {
    enable = true;
    hooks = {
      predetect = {
        compton = "pkill compton";
        polybar = "kill -9 $(pgrep polybar); sleep 0.5";
      };
      postswitch = {
        compton = "allow_rgb10_configs=false ${pkgs.compton}/bin/compton --backend glx -i 0 --vsync opengl-swc -C --shadow-exclude '!focused' --shadow-exclude-reg 'x${builtins.elemAt (builtins.split "px" services.polybar.config."bar/top".height) 0}+0+0' &"; 
        polybar = "for i in $(polybar -m | cut -d ':' -f 1); do MONITOR=$i polybar top & sleep 0.5; done";
      };
    };
    profiles = if device == "HP-Laptop" then {
      "dacha" = {
        fingerprint = {
          eDP = "00ffffffffffff0030e4f60400000000001a01049522137803a1c59459578f27205054000000010101010101010101010101010101012e3680a070381f403020350058c210000019222480a070381f403020350058c210000019000000fd00283c43430e010a20202020202000000002000c47ff0a3c6e1c151f6e0000000052";
          HDMI-A-0 = "00ffffffffffff0006b3cc24010101011a1a010380351e78ea0565a756529c270f5054afcf80714f8180818fb30081409500a9408bc0023a801871382d40582c45000f282100001e000000fd00304b1e5311000a202020202020000000fc00565a3234390a20202020202020000000ff0047364c4d52533034383636390a018902031df14a900403011412051f1013230907078301000065030c001000023a801871382d40582c45000f282100001e011d8018711c1620582c25000f282100009e011d007251d01e206e2855000f282100001e8c0ad08a20e02d10103e96000f282100001800000000000000000000000000000000000000000000000000004a";
        };
        config = {
          eDP = {
                        enable = true;
                        primary = true;
                        position = "0x0";
                        mode = "1920x1080";
                        rate = "60.00";
          };
          HDMI-A-0 = {
            enable = true;
            position = "1920x0";
            mode = "1920x1080";
            rate = "60.00";
          };
        };
      };
    } else if device == "ASUS-Laptop" then {
      "dacha" = {
        fingerprint = {
          HDMI2 = "00ffffffffffff0006b3cc24010101011a1a010380351e78ea0565a756529c270f5054afcf80714f8180818000fc00565a3234390a20202020202020000000ff0047364c4d52533034383636390a018902031df14a9004030114000f282100009e011d007251d01e206e2855000f282100001e8c0ad08a20e02d10103e96000f28210000180000000";
          eDP1 = "00ffffffffffff000dae61130000000007180104a51d117802ce85a3574e9d2612505400000001010101010100fe00434d4e0a202020202020202020000000fe004e3133334853452d4541330a2000a1";
        };
        config = {
          eDP1 = {
                        enable = true;
                        primary = true;
                        position = "0x0";
                        mode = "1920x1080";
                        rate = "60.00";
          };
          HDMI2 = {
            enable = true;
            position = "1920x0";
            mode = "1920x1080";
            rate = "60.00";
          };
        };
      };
    } else {};
  };

  services.udiskie.enable = true;
  
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
  ])
  ++
  (with pkgs.kdeApplications; [
    ark dolphin dolphin-plugins dragon eventviews ffmpegthumbs
    filelight gwenview kate kcachegrind kcalc kcolorchooser kdenlive
    kleopatra kolourpaint kompare krdc krfb kruler ktnef kwalletmanager
    marble okteta okular print-manager
  ])
  ++
  (builtins.filter pkgs.stdenv.lib.isDerivation (builtins.attrValues (pkgs.plasma5)));

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = (epkgs: with epkgs; [ use-package nix-mode haskell-mode nixos-options nord-theme wakib-keys magit exec-path-from-shell ivy counsel smex projectile which-key markdown-mode diminish frames-only-mode company rainbow-delimiters diff-hl yasnippet yasnippet-snippets mode-line-bell powerline smart-mode-line-powerline-theme hasklig-mode ]);    
  };

  programs.git = {
    enable = true;
    userEmail = "balsoft@yandex.ru";
    userName = "Александр Бантьев";
  };

  home.keyboard = {
    options = ["grp:caps_toggle,grp_led:caps"];
    layout = "us,ru";
  };
  
  xdg = {
    enable = true;
    configFile = { 
      "libinput-gestures.conf".text = ''
        gesture swipe down 4 xdotool key "Alt+quoteleft"
        gesture swipe up 4 xdotool key "Alt+asciitilde"
        gesture pinch in 2 xdotool key "Ctrl+F8"
        gesture pinch out 2 xdotool key "Ctrl+F8"
        gesture swipe right 3 xdotool key "Ctrl+Tab"
        gesture swipe left 3 xdotool key "Ctrl+Shift+Tab"
        gesture swipe up 3 xdotool key "Pause"
        gesture swipe down 3 xdotool key "Pause"
      '';
      "albert/albert.conf".text = genIni {
        General = {
          frontendId = "org.albert.frontend.qmlboxmodel";
          hotkey = "Meta+Space";
          showTray = false;
          telemetry = true;
          terminal = "${pkgs.konsole}/bin/konsole -e";
          incrementalSort = true;
        };
        "org.albert.extension.applications".enabled = true;
        "org.albert.extension.files" = {
          enabled = true;
          filters = "application/*, image/*, directory/*, text/*";  
        };
        "org.albert.extension.chromebookmarks".enabled = true;
        "org.albert.extension.mpris".enabled = true;
        "org.albert.extension.python" = {
          enabled = true;
          enabled_modules = "Python, Wikipedia, Kill, qalc, nix, translate";
        };
        "org.albert.extension.ssh".enabled = true;
        "org.albert.extension.system" = {
            enabled = true;
            logout = "i3-msg exit";
            lock = "${pkgs.i3lock-fancy}/bin/i3lock-fancy";
            reboot = "reboot";
            shutdown = "shutdown now";    
        };
        "org.albert.extension.terminal".enabled = true;
        "org.albert.extension.websearch".enabled = true;
        "org.albert.frontend.qmlboxmodel" = {
          enabled = true;
          alwaysOnTop = true;
          clearOnHide = false;
          hideOnClose = false;
          hideOnFocusLoss = true;
          showCentered = true;
          stylePath="${pkgs.albert}/share/albert/org.albert.frontend.qmlboxmodel/styles/BoxModel/MainComponent.qml";
          windowPosition="@Point(299 13)";
        };
      };
      "albert/org.albert.frontend.qmlboxmodel/style_properties.ini".text = themes.albert;
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

      "mconnect/mconnect.conf".text = genIni {
        "main" = {
          devices = "lge;huawei";
        };
        lge = {
          name = "lge";
          type = "phone";
          allowed = 1;
        };
        huawei = {
          name = "huawei";
          type = "phone";
          allowed = 1;
        };
      };
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
        "Default Applications" = {
          "text/html" = "chromium-browser.desktop";
          "image/*" = "org.kde.gwenview.desktop";
          "application/x-bittorrent" = "org.kde.ktorrent";
          "application/zip" = "org.kde.ark.desktop";
          "application/rar" = "org.kde.ark.desktop";
          "application/7z" = "org.kde.ark.desktop";
          "application/*tar" = "org.kde.ark.desktop";
          "application/x-kdenlive" = "org.kde.kdenlive.desktop";
          "x-scheme-handler/http" = "chromium-browser.desktop";
          "x-scheme-handler/https" = "chromium-browser.desktop";
          "x-scheme-handler/about" = "chromium-browser.desktop";
          "x-scheme-handler/unknown" = "chromium-browser.desktop";
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
  } // builtins.mapAttrs (name: value: {
    target = "albert/org.albert.extension.python/modules/" + name + ".py";
    text = value;
  }) scripts.albert;


  #xdg.dataFile."albert/org.albert.extension.python/modules/qalc.py".text = scripts.albert.qalc;
  #xdg.dataFile."albert/org.albert.extension.python/modules/nix.py".text = scripts.albert.nix;
  #xdg.dataFile."albert/org.albert.extension.python/modules/translate.py".text = scripts.albert.translate;
  home.file.".icons/default".source = "${pkgs.breeze-qt5}/share/icons/breeze_cursors";
  home.file.".themes/Generated".source = "${themes.gtk}/generated";
  home.file.".emacs.d/init.el".source = ./scripts/init.el;
  home.activation = builtins.mapAttrs (name: value: {inherit name; before = []; after = [ "linkGeneration" ];} // value) {
    konsole.data = "$DRY_RUN_CMD cp ~/.config/konsolerc.home ~/.config/konsolerc";
    kate.data = "$DRY_RUN_CMD cp ~/.config/katerc.home ~/.config/katerc";
    user-places.data = "$DRY_RUN_CMD cp ~/.local/share/user-places.xbel.home ~/.local/share/user-places.xbel";
    mimeapps .data= "$DRY_RUN_CMD cp ~/.config/mimeapps.list.home ~/.config/mimeapps.list";
    # FIXME soooo ugly and imperative...
  };

  news.display = "silent";
  programs.command-not-found.enable = true;
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "*" = {
        identityFile = toString (pkgs.writeTextFile {
          name = "id_rsa";
          text = secret.id_rsa;
        });
        extraOptions.Ciphers = "aes128-gcm@openssh.com";
        compression = false;
      };
    };
  };
}
