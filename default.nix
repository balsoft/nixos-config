# Edit this configuration file to define what should be installed on
# your system.	Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

device: 
{ config, pkgs, lib, ... }: 
with import ./common.nix device;
{
	# ========================== HARDWARE =====================================
	imports = [
		/etc/nixos/hardware-configuration.nix
#		"${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
		"${builtins.fetchGit { url="https://github.com/rycee/home-manager"; ref="master"; }}/nixos"
	];

	hardware.cpu.${cpu}.updateMicrocode = true;
	
	hardware.opengl.enable = true;
	hardware.opengl.driSupport = true;
	hardware.opengl.driSupport32Bit = true;
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
		blacklistedKernelModules = if device == "Prestigio-Laptop" then [ "axp288_charger" "axp288_fuel_gauge" "axp288_adc" ] else [ "pcspkr" ];
		extraModprobeConfig = if device == "ASUS-Laptop" then ''
		options iwlwifi swcrypto=0 11n_disable=1'' else "";
	};

	hardware.bluetooth.enable = false;	
	hardware.bluetooth.powerOnBoot = false;
	services.logind.extraConfig = "HandlePowerKey=suspend";
	# =========================================================================
	
	
	# ====================== TIME & LOCALE ====================================
	# Set your time zone.
	time.timeZone = "Europe/Moscow";
	# =========================================================================
	
	
	
	# ====================== NETWORKING =======================================
	networking = {
		#networkmanager.enable = true;
		wireless = {
			enable = true;
			networks.Keenetic.pskRaw = "4d03ac6e3d2a2b891d83dcceca6f531abd0fec421ad4460878f5f3bc4c76562e";
			userControlled.enable = true;
			#iwd.enable = true;
		};
		firewall.enable = false;
		usePredictableInterfaceNames = false;
		hostName = device;
	};
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
		desktopManager.wallpaper.combineScreens = false;
		desktopManager.wallpaper.mode = "fill";
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
				theme = {
					package = pkgs.breeze-gtk;
					name = "Breeze-Dark";
				};
			};
		};
#		desktopManager.plasma5.enable = true;
		desktopManager.default = "none";
		windowManager.i3.enable = true;
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
		];
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
	environment.systemPackages = (builtins.filter pkgs.stdenv.lib.isDerivation (builtins.attrValues pkgs.kdeApplications));
	environment.sessionVariables = {
		EDITOR = "micro";
		QT_QPA_PLATFORMTHEME = "qt5ct";
		QT_SCALE_FACTOR = "1";
		QT_AUTO_SCREEN_SCALE_FACTOR = "0";
		GTK_THEME = "Breeze-Dark";
		LESS = "-asrRix8";
		DE = "kde";
		XDG_CURRENT_DESKTOP = "kde";
		DESKTOP_SESSION = "kde";
		QT_XFT = "true";
		QT_SELECT = "5";
	};

	programs.ssh.askPassword = "${pkgs.ksshaskpass}/bin/ksshaskpass";

	virtualisation.virtualbox.host = {
		enable = isHost;
		enableHardening = false;
	};
	
	#virtualisation.libvirtd.enable = true;	
	
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
	
	services.openssh = {
		enable = true;
		passwordAuthentication = false;
		authorizedKeysFiles = pkgs.writeTextFile {
			name = "id_rsa.pub";
			text = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDd2OdcSHUsgezuV+cpFqk9+Svtup6PxIolv1zokVZdqvS8qxLsA/rwYmQgTnuq4/zK/GIxcUCH4OxYlW6Or4M4G7qrDKcLAUrRPWkectqEooWRflZXkfHduMJhzeOAsBdMfYZQ9024GwKr/4yriw2BGa8GbbAnQxiSeTipzvXHoXuRME+/2GsMFAfHFvxzXRG7dNOiLtLaXEjUPUTcw/fffKy55kHtWxMkEvvcdyR53/24fmO3kLVpEuoI+Mp1XFtX3DvRM9ulgfwZUn8/CLhwSLwWX4Xf9iuzVi5vJOJtMOktQj/MwGk4tY/NPe+sIk+nAUKSdVf0y9k9JrJT98S/";
		};
	};
	programs.light.enable = isLaptop;

	services.earlyoom = {
		enable = !isSSD;
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
	
	services.udev.extraRules = if isLaptop then ''
		ACTION=="add|change", KERNEL=="sd*[!0-9]|sr*", ATTR{queue/scheduler}="bfq"
		ACTION=="change", SUBSYSTEM=="power_supply", ATTR{online}=="0", RUN+="${pkgs.systemd}/bin/systemctl start battery"
	    ACTION=="change", SUBSYSTEM=="power_supply", ATTR{online}=="1", RUN+="${pkgs.systemd}/bin/systemctl start ac"
	    ACTION=="add|change", SUBSYSTEM=="backlight", MODE:="0777"
	'' + (if device == "ASUS-Laptop" then ''
		ACTION=="add|change", SUBSYSTEM=="net", KERNEL=="wlan*" RUN+="${pkgs.iw}/bin/iw dev %k set power_save off"
	'' else "") else "";
	
	systemd.services.battery = {
        enable = isLaptop && device != "ASUS-Laptop";
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
		extraGroups = ["sudo" "wheel" "networkmanager" "disk" "sound" "pulse" "adbusers" "input" "libvirtd" "vboxusers"];
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
	};
	nix.requireSignedBinaryCaches = false;

	home-manager.users.bigsoft = {
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
	};

	home-manager.users.balsoft = import ./home.nix device { inherit pkgs; inherit lib; };
	# =========================================================================
	
	# The NixOS release to be compatible with for stateful data such as databases.
	system.stateVersion = "18.03";
}
