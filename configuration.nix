# Edit this configuration file to define what should be installed on
# your system.	Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
	# ========================== HARDWARE =====================================
	imports = [
		./hardware-configuration.nix
        "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
	];
	
	hardware.cpu.amd.updateMicrocode = true;
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
			systemd-boot.enable = true;
			efi = {
				canTouchEfiVariables = true;
				efiSysMountPoint = "/boot";
			};
		};
		consoleLogLevel = 0;
		kernelPackages = pkgs.linuxPackages_latest;
		kernelParams = ["scsi_mod.use_blk_mq=1"];
		extraModprobeConfig = ''
options iwlwifi bt_coex_active=0
		'';
	};
	# =========================================================================
	
	
	
	# ======================== FILESYSTEMS ====================================
	fileSystems = {
		"/".device = "/dev/sda5";
		"/home/".device = "/dev/sda4";
		"/boot".device = "/dev/sda1";
		"/run/media/ubuntu".device = "/dev/sda2";
	};
	
	services.udev.extraRules = ''
		ACTION=="add|change", KERNEL=="sd*[!0-9]|sr*", ATTR{queue/scheduler}="bfq"
	'';

	swapDevices = [
		{
		    device = "/dev/sda3"; 
		}
	];
	
	security.pam.enableEcryptfs = true; # For /home
	# =========================================================================
	
	
	
	# ====================== TIME & LOCALE ====================================
	# Set your time zone.
	time.timeZone = "Europe/Moscow";
	# =========================================================================
	
	
	
	# ====================== NETWORKING =======================================
	networking = {
		networkmanager.enable = true;
		hostName = "HP-Laptop";
		firewall.enable = false;
	};
	# =========================================================================

	
	
	# ===================== GRAPHICS & FONTS ==================================
	services.xserver = {
        enable = true;
		videoDrivers = [ "amdgpu" ];
		libinput = {
			enable = true;
			sendEventsMode = "disabled-on-external-mouse";
			middleEmulation = false;
		};
		#desktopManager.plasma5.enable = true;
		desktopManager.gnome3.enable = true;
		desktopManager.mate.enable = true;
		#displayManager.lightdm.enable = true;
		displayManager.slim = {
			enable = false;
#			#autoLogin = true;
#			defaultUser = "balsoft";
		};
		displayManager.job.execCmd = "";
	};
	programs.bash.shellInit = ''
		if [[ `tty` = /dev/tty2 ]]
		then
			while true
			do
				echo "============================== STARTING PLASMA 5 ================================="
				dbus-launch startplasmacompositor
				echo "================================ RESTARTING ======================================"
			done

		fi
		if [[ `tty` = /dev/tty1 ]]
		then
			while true
			do	
				echo "=========================== STARTING GNOME SHELL ================================="
				/home/balsoft/run_gnome_session.nixsh
				echo "=========================== EXITING GNOME SHELL =================================="
			done
		fi
	'';
	fonts = {
		fonts = with pkgs; 
		[
			terminus_font
			opensans-ttf
			roboto
			roboto-mono
			roboto-slab
			powerline-fonts
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
	
	hardware.bluetooth = {
		enable = true;
	};
	# =========================================================================
	
	
	
	# ====================== PROGRAMS & SERVICES ==============================
	virtualisation.virtualbox.host.enable = true;
	
	system.autoUpgrade = {
		dates = "19:00";
		enable = true; 	
	};
	
	services.openssh.enable = true;
	services.postgresql = {
		enable = true;
		authentication = lib.mkForce ''
		    # Generated file; do not edit!
		    # TYPE  DATABASE        USER            ADDRESS                 METHOD
		    local   all             all                                     trust
		    host    all             all             127.0.0.1/32            trust
		    host    all             all             ::1/128                 trust
		'';
	};
	services.printing = {
		enable = true;
		drivers = [ pkgs.gutenprint ];
	};
	
	
	services.dbus.packages = [
		pkgs.gnome3.gconf
	];

	services.gnome3 = {
		chrome-gnome-shell.enable = true;
		gpaste.enable = true;
		gvfs.enable = true;
		gnome-online-accounts.enable = true;
	};

	services.tor = {
		enable = true;
		client.enable = true;
		client.privoxy.enable = true;
		torsocks.enable = true;
	};

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
	systemd.services.fix_touchpad = {
        description = "Fix touchpad bug";
        after = ["suspend.target"];
        wantedBy = ["suspend.target"];
        script = ''
			echo "Fixing touchpad on `date`"
            /run/current-system/sw/bin/modprobe -r psmouse
            /run/current-system/sw/bin/modprobe psmouse
			echo "Done fixing touchpad"
        '';
	};
	environment.variables = {
		QT_STYLE_OVERRIDE = "kvantum";
	};
	services.packagekit.enable = true;
	hardware.sensor.iio.enable = true;
	i18n = {
		defaultLocale = "en_GB.UTF-8";
	};
	# =========================================================================
	
	
	
	# ======================= USERS & SECURITY ================================
	security.apparmor.enable = true;
	# Define a user account. Don't forget to set a password with ‘passwd’.
	nixpkgs.config.allowUnfree = true;
	users.mutableUsers = false;
	users.extraUsers.balsoft = {
		isNormalUser = true;
		extraGroups = ["sudo" "wheel" "networkmanager" "disk" "sound" "pulse" "adbusers" "input"];
		description = "Александр Бантьев";
		uid = 1000;
		password = "";
	};
	# =========================================================================
	
	# The NixOS release to be compatible with for stateful data such as databases.
	system.stateVersion = "18.03";
}
