# This is balsoft's configuration file.
#
# https://github.com/balsoft/nixos-config
#
# This is main nixos configuration
# To use this configuration:
#   1. Add your own secret.nix to this folder
#   2. Replace /etc/nixos/configuration.nix with the following:
#      import /path/to/this/nixos-config "Vendor-Type"
#   3. Log in to application and services where neccesary


device: # This is the device we're on now
{ config, pkgs, lib, ... }: 
with import ./common.nix device pkgs; # Common stuff that is shared between home.nix and this
{
  # ========================== HARDWARE =====================================
  imports = [
    /etc/nixos/hardware-configuration.nix
    "${builtins.fetchGit { url="https://github.com/rycee/home-manager"; ref="master"; }}/nixos"
    ./modules
  ];

  hardware.cpu.${cpu}.updateMicrocode = true; # Update microcode
  
  hardware.enableRedistributableFirmware = true; # For some unfree drivers

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true; # For steam

  hardware.sane.enable = true;
  # =========================================================================
  
  
  
  # ============================ BOOT =======================================
  boot = {
    loader = {
      grub.enable = true;
      grub.version = 2;
      grub.useOSProber = true;
      timeout = 1;
    } // (if device == "Lenovo-Workstation" then { # Non-UEFI config
      grub.device = "/dev/sda";
    } else { # UEFI config
      grub.efiSupport = true;
      grub.device = "nodev";
      grub.efiInstallAsRemovable = true; # NVRAM is unreliable
    });
    consoleLogLevel = 3;
    # There is a bug in newer versions of kernel that breaks wireless
    # TODO Figure out what to do here
    kernelPackages = if device == "ASUS-Laptop" then pkgs.linuxPackages else pkgs.linuxPackages;
    kernelParams = [ 
      "quiet" 
      "scsi_mod.use_blk_mq=1" 
      "modeset" 
      "nofb" 
      "rd.systemd.show_status=auto" 
      "rd.udev.log_priority=3" 
      "pti=off" 
      "spectre_v2=off"
    ] ++ (if device == "Prestigio-Laptop" then [
      "intel_idle.max_cstate=1" # Otherwise it hangs
    ] else []);
    kernel.sysctl = {
      "vm.swappiness" = 0;
    };
    extraModprobeConfig = "options iwlwifi swcrypto=1 power_save=0 power_level=5 11n_disable=8 bt_coex_active=1"; # Attempt to fix broken wireless
    blacklistedKernelModules = if device == "Prestigio-Laptop" then [ "axp288_charger" "axp288_fuel_gauge" "axp288_adc" ] else [  ]; # Battery driver sometimes breaks this cheap laptop
  };

  hardware.bluetooth.enable = true;
  services.logind.extraConfig = "HandlePowerKey=suspend";
  # =========================================================================
  
  
  # ====================== TIME & LOCALE ====================================
  time.timeZone = "Europe/Moscow"; # Mother Russia
  # =========================================================================
  
  
  
  # ====================== NETWORKING =======================================
  networking = {
    networkmanager.enable = false;
    wireless = {
      enable = true;
      driver = "wext";
      networks.Keenetic.pskRaw = "4d03ac6e3d2a2b891d83dcceca6f531abd0fec421ad4460878f5f3bc4c76562e";
      networks.BantyevIE.pskRaw = "e3a09c5136e71fa7022a407ffcd0a75acdb4cb733a123bb179dcb5c4762ffe18";
      interfaces = [ "wlan0" ];
      userControlled.enable = true;
    };
    firewall.enable = false;
    usePredictableInterfaceNames = false;
    hostName = device;
  };
  systemd.services.dhcpcd.serviceConfig.Type = lib.mkForce "simple"; # TODO Make a PR with this change; forking is not acceptable for dhcpcd.
  # =========================================================================

  
  
  # ===================== GRAPHICS & FONTS ==================================
  services.xserver = {
        enable = true;
        enableTCP = true;
    libinput = {
      enable = true;
      sendEventsMode = "disabled-on-external-mouse";
      middleEmulation = false;
      naturalScrolling = true;
    };
    videoDrivers = if
                     cpu == "amd"
                   then
                     [ "amdgpu" ]
                   else if
                     device == "Lenovo-Workstation"
                   then
                     [ "radeon" ]
                   else
                     [ "intel" ];
    displayManager.lightdm = {
      enable = true;
      autoLogin.enable = !isShared;
      autoLogin.user = "balsoft";
      greeter.enable = isShared;
      greeters.gtk = {
        enable = isShared;
        iconTheme = {
          package = pkgs.papirus-icon-theme;
          name = "Papirus-Dark";
        };
      };
    };
#   desktopManager.plasma5.enable = true;
    desktopManager.default = "none";
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
    #windowManager.xmonad.enable = true;
    windowManager.default = "i3";
    layout = "us,ru";
    xkbOptions = "grp:caps_toggle,grp_led:caps";
  };
  programs.dconf.enable = true;
  fonts = {
    fonts = with pkgs; [
      terminus_font
      opensans-ttf
      roboto
      roboto-mono
      roboto-slab
      powerline-fonts
      noto-fonts
      noto-fonts-emoji
      #fira-code-symbols
      hasklig
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [ "Roboto Mono 13" ];
        sansSerif = [ "Roboto 13" ];
        serif = [ "Roboto Slab 13" ];
      };
    };
    enableDefaultFonts = true;
  };

  services.actkbd = {
    enable = isLaptop;
    bindings = map (x: x // {
      events = [ "key" ];
      attributes = [ "exec" ];
    }) ((if device == "ASUS-Laptop" then [
      {
        keys = [ 229 ];
        command = "expr -1 + `cat '/sys/class/leds/asus::kbd_backlight/brightness'` > '/sys/class/leds/asus::kbd_backlight/brightness'";
      }
      {
        keys = [ 230 ];
        command = "expr 1 + `cat '/sys/class/leds/asus::kbd_backlight/brightness'` > '/sys/class/leds/asus::kbd_backlight/brightness'";
      }
      {
        keys = [560];
        command = (toString (pkgs.writeTextFile {
          name = "als-script";
          text = ''
            if [[ `cat /sys/devices/platform/asus-nb-wmi/als_enable` -eq 1 ]]
            then
              echo "0" > /sys/devices/platform/asus-nb-wmi/als_enable
              ${pkgs.light}/bin/light -O
            else
              echo "1" > /sys/devices/platform/asus-nb-wmi/als_enable
              ${pkgs.light}/bin/light -I
              while true
              do
                [[ `cat /sys/devices/platform/asus-nb-wmi/als_enable` -eq 0 ]] && exit 1;
                brightness=$(((brightness+`cat '/sys/devices/LNXSYSTM:00/LNXSYBUS:00/ACPI0008:00/iio:device0/in_illuminance_input'`)/2))
                ${pkgs.light}/bin/light -S $((2 + $brightness * 2))
                echo $(((100 - $brightness)/80)) > '/sys/class/leds/asus::kbd_backlight/brightness'
                sleep 1
              done &
            fi'';
          executable = true;
        }));
      }
    ] else []) ++ [
      {
        keys = [ 225 ];
        command = "${pkgs.light}/bin/light -A 10";
      }
      {
        keys = [ 224 ];
        command = "${pkgs.light}/bin/light -U 10";
      }
      {
        keys = [ 431 ];
        command = (toString (pkgs.writeTextFile {
          name = "dark-script";
          text = ''
            if [[ `${pkgs.light}/bin/light` -eq 0 ]]
            then
              ${pkgs.light}/bin/light -I
            else
              ${pkgs.light}/bin/light -O
              ${pkgs.light}/bin/light -S 0
            fi'';
          executable = true;
        }));
      }
    ]);
  };
  services.acpid.enable = true;
  # =========================================================================

  
  
  # ====================== SOUND ============================================
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;  
    support32Bit = true;
  };
  # =========================================================================
  
  
  
  # ====================== PROGRAMS & SERVICES ==============================
  #environment.systemPackages = (builtins.filter pkgs.stdenv.lib.isDerivation (builtins.attrValues (pkgs.plasma5 // pkgs.plasma-workspace)));
  environment.sessionVariables = {
    EDITOR = editor;
    VISUAL = editor;
    SHELL = "zsh";
    QT_QPA_PLATFORMTHEME = "kde";
    QT_SCALE_FACTOR = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    GTK_THEME = "Generated";
    LESS = "-asrRix8";
    DE = "kde";
    #XDG_CURRENT_DESKTOP = "kde";
    DESKTOP_SESSION = "kde";
    QT_XFT = "true";
    QT_SELECT = "5";
    XDG_CURRENT_DESKTOP="KDE";
    KDE_SESSION_VERSION="5";
  };

  environment.systemPackages = (with pkgs.kdeApplications; [
    ark dolphin dolphin-plugins dragon eventviews ffmpegthumbs
    filelight gwenview kate kcachegrind kcalc kcolorchooser kdenlive
    kleopatra kolourpaint kompare krdc krfb kruler ktnef kwalletmanager
    marble okteta okular print-manager kio-extras
  ])
  ++
  (builtins.filter pkgs.stdenv.lib.isDerivation (builtins.attrValues (pkgs.plasma5)))
  ++ (with pkgs; [
    kded
    kdeFrameworks.kio
    plasma-integration
    kinit
    plasma5.xdg-desktop-portal-kde
  ]);

  programs.ssh.askPassword = "${pkgs.plasma5.ksshaskpass}/bin/ksshaskpass";

  environment.pathsToLink = [ "/share/zsh" ];

  virtualisation.virtualbox.host = {
    enable = isHost;
    enableHardening = false;
  };
  
  #virtualisation.libvirtd.enable = true; 
  
  system.autoUpgrade = {
    dates = "19:00";
    enable = true;  
  };

  nixpkgs.config.packageOverrides = old: {
    nur = pkgs.callPackage (import (builtins.fetchGit {
      url = "https://github.com/nix-community/NUR";
    })) {};
    movit = old.movit.overrideAttrs (oldAttrs: { # Currently, movit fails
      doCheck = false;
      GTEST_DIR = "${old.gtest.src}/googletest";
    });
    plasma5 = old.plasma5 // {xdg-desktop-portal-kde = old.plasma5.xdg-desktop-portal-kde.overrideAttrs (oldAttrs: {
      buildInputs = oldAttrs.buildInputs ++  [ old.cups ];
    });
      tdesktop = old.tdesktop.overrideAttrs (oldAttrs: {
        patches = [
        (builtins.fetchurl { url = "https://raw.githubusercontent.com/msva/mva-overlay/master/net-im/telegram-desktop/files/patches/1.5.6/conditional/wide-baloons/0001_baloons-follows-text-width-on-adaptive-layout.patch"; sha256 = "95800293734d894c65059421d7947b3666e3cbe73ce0bd80d357b2c9ebf5b2e5"; })
        ] ++ oldAttrs.patches;
        });};
    } // (if device == "Prestigio-Laptop" then {
    grub2 = old.pkgsi686Linux.grub2;
  } else {});
  nixpkgs.config.android_sdk.accept_license = true;
  services.synergy = if device == "Lenovo-Workstation" then {
    server.enable = true;
    server.configFile = pkgs.writeTextFile {
      name = "synergy.conf";
      text = ''
section: screens
	Lenovo-Workstation:
	ASUS-Laptop:
	HP-Laptop:
end
section: links
	Lenovo-Workstation:
		right = HP-Laptop
		left    = ASUS-Laptop

	ASUS-Laptop:
		right    = Lenovo-Workstation
	HP-Laptop:
		left = Lenovo-Workstation
end
section: options
    keystroke(super+alt+left) = switchInDirection(left)
    keystroke(super+alt+right) = switchInDirection(right)
end
      '';
    };
  } else {
    client.enable = true;
    client.serverAddress = "Lenovo-Workstation";
  };

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  programs.light.enable = isLaptop;

  services.earlyoom = {
    enable = !isSSD;
    freeMemThreshold = 5;
    freeSwapThreshold = 100;
  };

  services.printing = {
    enable = device == "Lenovo-Workstation";
    drivers = [ pkgs.gutenprint ];
  };
  

  services.tor = {
    enable = true;
    client.enable = true;
    client.privoxy.enable = true;
    torsocks.enable = true;
  };
  #services.teamviewer.enable = true;

  programs.wireshark.enable = true;

  services.avahi.enable = true;
  programs.adb.enable = true;

  systemd.services.systemd-udev-settle.enable = false;

  services.upower.enable = true;
  
  services.udev.extraRules = if isLaptop then ''
    ACTION=="add|change", KERNEL=="sd*[!0-9]|sr*", ATTR{queue/scheduler}="bfq"
    ACTION=="change", SUBSYSTEM=="power_supply", ATTR{online}=="0", RUN+="${pkgs.systemd}/bin/systemctl start battery"
      ACTION=="change", SUBSYSTEM=="power_supply", ATTR{online}=="1", RUN+="${pkgs.systemd}/bin/systemctl start ac"
      ACTION=="add|change", SUBSYSTEM=="backlight", MODE:="0777"
  '' + (if device == "ASUS-Laptop" then ''
    ACTION=="add|change", SUBSYSTEM=="net", KERNEL=="wlan*" RUN+="${pkgs.iw}/bin/iw dev %k set power_save off"
  '' else "") else "";
  
  systemd.services.battery = {
        enable = isLaptop;
        description = "Executes commands needed on battery power";
        script = ''
            ${pkgs.linuxPackages_latest.cpupower}/bin/cpupower frequency-set -g powersave
            ${pkgs.hdparm}/bin/hdparm -B 1 /dev/sda
      echo "500" > /sys/class/backlight/*/brightness
        '';
  };
  systemd.services.ac = {
        enable = isLaptop;
        description = "Executes commands needed on ac power";
        script = ''
            ${pkgs.linuxPackages_latest.cpupower}/bin/cpupower frequency-set -g performance
            ${pkgs.hdparm}/bin/hdparm -B 255 /dev/sda
      echo "900" > /sys/class/backlight/*/brightness
        '';
  };
  systemd.services.leds_setup = {
      enable = (device == "ASUS-Laptop");
      description = "Set up leds triggers";
      wantedBy = ["multi-user.target"];
      script = ''
          echo "phy0rx" > /sys/class/leds/asus-wireless\:\:airplane/trigger
      '';
  };
  #services.illum.enable = isLaptop;
  hardware.sensor.iio.enable = (device == "HP-Laptop");
  i18n = {
    defaultLocale = "en_GB.UTF-8";
  };
  # =========================================================================
  
  
  
  # ======================= USERS & SECURITY ================================
  security.apparmor.enable = true;
  nixpkgs.config.allowUnfree = true;
  users.mutableUsers = false;
  users.users.balsoft = {
    isNormalUser = true;
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDd2OdcSHUsgezuV+cpFqk9+Svtup6PxIolv1zokVZdqvS8qxLsA/rwYmQgTnuq4/zK/GIxcUCH4OxYlW6Or4M4G7qrDKcLAUrRPWkectqEooWRflZXkfHduMJhzeOAsBdMfYZQ9024GwKr/4yriw2BGa8GbbAnQxiSeTipzvXHoXuRME+/2GsMFAfHFvxzXRG7dNOiLtLaXEjUPUTcw/fffKy55kHtWxMkEvvcdyR53/24fmO3kLVpEuoI+Mp1XFtX3DvRM9ulgfwZUn8/CLhwSLwWX4Xf9iuzVi5vJOJtMOktQj/MwGk4tY/NPe+sIk+nAUKSdVf0y9k9JrJT98S/ comment" ];
    extraGroups = ["sudo" "wheel" "networkmanager" "disk" "sound" "pulse" "adbusers" "input" "libvirtd" "vboxusers" "wireshark"];
    description = "Александр Бантьев";
    uid = 1000;
    password = "";
  };
  users.users.svetlana-banteva = {
    isNormalUser = true;
    extraGroups = ["pulse" "input"];
    description = "Светлана Бантьева";
    password = "";    
  };
  users.users.bigsoft = {
    isNormalUser = true;
    extraGroups = [ "pulse" "input" "vboxusers" "networkmanager" ];
    description = "Игорь Бантьев";
    password = "";    
  };
  security.sudo = {
    enable = true;
    extraConfig = ''
balsoft ALL = (root) NOPASSWD: /run/current-system/sw/bin/nixos-rebuild switch
    '';
  };
  nix.requireSignedBinaryCaches = false;  

  home-manager.users.bigsoft = if device == "ASUS-Laptop" then {
    xsession = {
      enable = true;
      windowManager.command = ''
        MACHINE="Windows"
        VBoxManage startvm $MACHINE
        until $(VBoxManage showvminfo --machinereadable $MACHINE | grep -q ^VMState=.poweroff.)
        do
        sleep 1
        done
      '';
    }; 
  } else {

  };

  home-manager.users.balsoft = import ./home.nix device { inherit pkgs; inherit lib; };
  # =========================================================================
  
  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.03";
}
