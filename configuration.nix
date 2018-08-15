# Edit this configuration file to define what should be installed on
# your system.	Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }: 
{
	# ========================== HARDWARE =====================================
	imports = [
		/etc/nixos/hardware-configuration.nix
	];
	
	hardware.cpu.intel.updateMicrocode = true;
	hardware.opengl.enable = true;
	hardware.opengl.driSupport32Bit = true;
	# =========================================================================
	
	
	
	# ============================ BOOT =======================================
	boot = {
		loader = {
			grub = {
				enable = true;
				version = 2;
				efiSupport = true;
				useOSProber = true;
				device = "nodev";
			};
			efi = {
				canTouchEfiVariables = true;
				efiSysMountPoint = "/boot";
			};
		};
		consoleLogLevel = 3;
		kernelPackages = pkgs.linuxPackages_latest;
		kernelParams = [ 
            "quiet" 
            "scsi_mod.use_blk_mq=1" 
            "modeset" 
            "nofb" 
            "rd.systemd.show_status=auto" 
            "rd.udev.log_priority=3" 
        ];
		kernel.sysctl = {
			"kernel.printk" = "3 3 3 3";
			"vm.swappiness" = 0;
		};
	};
	# =========================================================================
	
	
	# ====================== TIME & LOCALE ====================================
	# Set your time zone.
	time.timeZone = "Europe/Moscow";
	# =========================================================================
	
	
	
	# ====================== NETWORKING =======================================
	networking = {
		networkmanager.enable = true;
		firewall.enable = false;
	};
	# =========================================================================

	
	
	# ===================== GRAPHICS & FONTS ==================================
	services.xserver = {
        enable = true;
		libinput = {
			enable = true;
			sendEventsMode = "disabled-on-external-mouse";
			middleEmulation = false;
		};
		desktopManager.wallpaper.combineScreens = false;
		desktopManager.wallpaper.mode = "fill";
		displayManager.sddm = {
			enable = true;
			autoLogin.enable = true;
			autoLogin.user = "balsoft";
		};
		desktopManager.plasma5.enable = true;
		displayManager.slim.enable = false;
	};
	fonts = {
		fonts = with pkgs; 
		[
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
	# =========================================================================

	
	
	# ====================== SOUND ============================================
	hardware.pulseaudio = {
		enable = true;
		package = pkgs.pulseaudioFull;
		support32Bit = true;
	};
	# =========================================================================
	
	
	
	# ====================== PROGRAMS & SERVICES ==============================
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
	  };
	
	services.openssh.enable = true;

#	programs.flatpak.enable = true;

	services.printing = {
		enable = true;
		drivers = [ pkgs.gutenprint ];
	};
	
	
	services.dbus.packages = [
		pkgs.gnome3.gconf
	];

	services.tor = {
		enable = true;
		client.enable = true;
		client.privoxy.enable = true;
		torsocks.enable = true;
	};
	services.teamviewer.enable = true;

	services.avahi.enable = true;
	programs.adb.enable = true;

	systemd.services.gdrive = {
		enable = true;
		requires = ["network-online.target"];
		description = ''
			Google Drive user service
		'';
		wantedBy = ["multi-user.target"];
		serviceConfig = {
			User = "balsoft";
			ExecStart = ''
				${pkgs.google-drive-ocamlfuse}/bin/google-drive-ocamlfuse -debug "/home/balsoft/Google Drive/"
			'';
		};
	};
	systemd.services.systemd-udev-settle.enable = false;
	
	services.udev.extraRules = ''
		ACTION=="add|change", KERNEL=="sd*[!0-9]|sr*", ATTR{queue/scheduler}="bfq"
		ACTION=="change", SUBSYSTEM=="power_supply", ATTR{online}=="0", RUN+="${pkgs.systemd}/bin/systemctl start battery"
	    ACTION=="change", SUBSYSTEM=="power_supply", ATTR{online}=="1", RUN+="${pkgs.systemd}/bin/systemctl start ac"
	'';
	
	systemd.services.leds_setup = {
		enable = true;
		description = "Set up leds triggers";
		wantedBy = ["multi-user.target"];
		script = ''
			echo "phy0rx" > /sys/class/leds/asus-wireless\:\:airplane/trigger
		'';
	};
	systemd.services.battery = {
        enable = true;
        description = "Executes commands needed on battery power";
        script = ''
            ${pkgs.linuxPackages_latest.cpupower}/bin/cpupower frequency-set -g powersave
            ${pkgs.hdparm}/bin/hdparm -B 1 /dev/sda
        '';
	};
	systemd.services.ac = {
        enable = true;
        description = "Executes commands needed on ac power";
        script = ''
            ${pkgs.linuxPackages_latest.cpupower}/bin/cpupower frequency-set -g performance
            ${pkgs.hdparm}/bin/hdparm -B 255 /dev/sda
        '';
	};
	services.packagekit.enable = true;
	hardware.sensor.iio.enable = true;
	i18n = {
		defaultLocale = "en_GB.UTF-8";
	};
	# =========================================================================
	
	
	
	# ======================= USERS & SECURITY ================================
	security.apparmor.enable = true;
	nixpkgs.config.allowUnfree = true;
	users.mutableUsers = false;
	users.extraUsers.balsoft = {
		isNormalUser = true;
		extraGroups = ["sudo" "wheel" "networkmanager" "disk" "sound" "pulse" "adbusers" "input" "libvirtd"];
		description = "Александр Бантьев";
		uid = 1000;
		password = "";
	};
	# =========================================================================
	
	# The NixOS release to be compatible with for stateful data such as databases.
	system.stateVersion = "18.03";
}
