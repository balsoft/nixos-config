# Edit this configuration file to define what should be installed on
# your system.	Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

device: 
{ config, pkgs, lib, ... }: 
let 
	isLaptop = (!isNull(builtins.match ".*Laptop" device));
	isShared = (device == "Prestigio-Laptop");
	cpu = if device == "HP-Laptop" then "amd" else "intel";
in
{
	# ========================== HARDWARE =====================================
	imports = [
		/etc/nixos/hardware-configuration.nix
#		"${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
		"${builtins.fetchGit { url="https://github.com/rycee/home-manager"; ref="master"; }}/nixos"
	];

	hardware.cpu.${cpu}.updateMicrocode = true;
	
	hardware.opengl.enable = true;
	hardware.opengl.driSupport = false;
	hardware.opengl.driSupport32Bit = false;
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
			#grub.canTouchEfiVariables = false;
			grub.efiInstallAsRemovable = true;
		});
		consoleLogLevel = 3;
		kernelPackages = pkgs.linuxPackages_latest;
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
			"intel_idle.max_cstate=1"
		] else []);
		kernel.sysctl = {
			"vm.swappiness" = 0;
		};
		blacklistedKernelModules = if device == "Prestigio-Laptop" then [ "axp288_charger" "axp288_fuel_gauge" "axp288_adc" ] else [];
	};

	hardware.bluetooth.enable = true;	
	services.logind.extraConfig = "HandlePowerKey=suspend";
	# =========================================================================
	
	
	# ====================== TIME & LOCALE ====================================
	# Set your time zone.
	time.timeZone = "Europe/Moscow";
	# =========================================================================
	
	
	
	# ====================== NETWORKING =======================================
	networking = {
		networkmanager.enable = true;
		firewall.enable = false;
		usePredictableInterfaceNames = false;
		hostName = device;
	};
	# =========================================================================

	
	
	# ===================== GRAPHICS & FONTS ==================================
	services.xserver = {
        enable = true;
		libinput = {
			enable = true;
			sendEventsMode = "disabled-on-external-mouse";
			middleEmulation = false;
			naturalScrolling = true;
		};
		desktopManager.wallpaper.combineScreens = false;
		desktopManager.wallpaper.mode = "fill";
		displayManager.lightdm = {
			enable = true;
			autoLogin.enable = !isShared;
			autoLogin.user = "balsoft";
			greeter.enable = isShared;
		};
#		desktopManager.plasma5.enable = true;
		desktopManager.default = "none";
		windowManager.i3.enable = true;
		windowManager.default = "i3";
		layout = "us,ru";
		xkbOptions = "grp:caps_toggle,grp_led:caps";
	};
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
		];
		enableDefaultFonts = true;
	};

	services.actkbd = {
		enable = isLaptop;
		bindings = map (x: x // {
			events = [ "key" ];
			attributes = [ "exec" ];
		}) (if device == "ASUS-Laptop" then [
			{
				keys = [ 229 ];
				command = "expr -1 + `cat '/sys/class/leds/asus::kbd_backlight/brightness'` > '/sys/class/leds/asus::kbd_backlight/brightness'";
			}
			{
				keys = [ 230 ];
				command = "expr 1 + `cat '/sys/class/leds/asus::kbd_backlight/brightness'` > '/sys/class/leds/asus::kbd_backlight/brightness'";
			}
			{
				keys = [25 125];
				command = ''
					${pkgs.xorg.xrandr}/bin/xrandr --output HDMI2 --off
					${pkgs.xorg.xrandr}/bin/xrandr --output HDMI2 --auto
					${pkgs.xorg.xrandr}/bin/xrandr --output eDP1 --preferred --primary --left-of o --output HDMI2 --nograb --noprimary --auto
					/run/current-system/sw/bin/pkill compton
				'';
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
				command = "${pkgs.light}/bin/light -S 0";
			}
		];
	};
	# =========================================================================

	
	
	# ====================== SOUND ============================================
	sound.enable = true;
	hardware.pulseaudio = {
		enable = true;
		package = pkgs.pulseaudioFull;		
	};
	# =========================================================================
	
	
	
	# ====================== PROGRAMS & SERVICES ==============================
	environment.systemPackages = (builtins.filter pkgs.stdenv.lib.isDerivation (builtins.attrValues pkgs.kdeApplications));
	environment.sessionVariables = {
            EDITOR = "micro";
            QT_QPA_PLATFORMTHEME = "qt5ct";
            GTK_THEME = "Breeze-Dark";
    };

#	virtualisation.virtualbox.host.enable = true;
	virtualisation.libvirtd.enable = true;	
	
	system.autoUpgrade = {
		dates = "19:00";
		enable = true; 	
	};

    nixpkgs.config.packageOverrides = pkgs: {
	    nur = pkgs.callPackage (import (builtins.fetchGit {
            url = "https://github.com/nix-community/NUR";
	    })) {};
    } // (if device == "Prestigio-Laptop" then {
		grub2 = (import <nixpkgs> {system = "i686-linux";}).grub2;
	} else {});
	
	services.openssh.enable = true;
	
	programs.light.enable = isLaptop;

	services.earlyoom = {
		enable = true;
		freeMemThreshold = 5;
		freeSwapThreshold = 100;
	};

	services.printing = {
		enable = true;
		drivers = [ pkgs.gutenprint ];
	};
	
	services.dbus.packages = [
#		pkgs.gconf
	];

	services.tor = {
		enable = true;
		client.enable = true;
		client.privoxy.enable = true;
		torsocks.enable = true;
	};
	#services.teamviewer.enable = true;


	services.avahi.enable = true;
	programs.adb.enable = true;

	systemd.services.systemd-udev-settle.enable = false;

	services.upower.enable = true;
	
	services.udev.extraRules = ''
		ACTION=="add|change", KERNEL=="sd*[!0-9]|sr*", ATTR{queue/scheduler}="bfq"
		ACTION=="change", SUBSYSTEM=="power_supply", ATTR{online}=="0", RUN+="${pkgs.systemd}/bin/systemctl start battery"
	    ACTION=="change", SUBSYSTEM=="power_supply", ATTR{online}=="1", RUN+="${pkgs.systemd}/bin/systemctl start ac"
	    ACTION=="add|change", SUBSYSTEM=="backlight", MODE:="0777"
	'';
	
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
		extraGroups = ["sudo" "wheel" "networkmanager" "disk" "sound" "pulse" "adbusers" "input" "libvirtd"];
		description = "Александр Бантьев";
		uid = 1000;
	} // (if isShared then {
		hashedPassword = "YotlMqtSycvPk";
	} else {
		password = "";
	});
	users.users.svetlana-banteva = {
		isNormalUser = true;
		extraGroups = ["pulse" "input"];
		description = "Светлана Бантьева";
		password = "";		
	};
	security.sudo = {
		enable = true;
	};

	home-manager.users.balsoft = import ./home.nix device { inherit pkgs; inherit lib; };
	# =========================================================================
	
	# The NixOS release to be compatible with for stateful data such as databases.
	system.stateVersion = "18.03";
}
