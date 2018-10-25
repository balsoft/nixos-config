device: rec {
	devMachine = device != "Prestigio-Laptop";

#	vsCodeExt = { publisher, name, version, sha256?"" }: pkgs.fetchzip {
#		url = "https://${publisher}.gallery.vsassets.io/_apis/public/gallery/publisher/${publisher}/extension/${name}/${version}/assetbyname/Microsoft.VisualStudio.Services.VSIXPackage";
#		inherit sha256;
#	};

	myDevices = {
		ASUS-Laptop = {
			cpu = {
				vendor = "intel";
				clock = 2300;
				cores = 2;
			};
			drive = {
				type = "ssd";
				speed = 500;
				size = 250;
			};
			ram = 12;
		};
		HP-Laptop = {
			cpu = {
				vendor = "amd";
				clock = 3500;
				cores = 6;
			};
			drive = {
				type = "ssd";
				speed = 500;
				size = 500;
			};
			ram = 8;
		};
		Lenovo-Workstation = {
			cpu = {
				vendor = "intel";
				clock = 2500;
				cores = 2;
			};
			drive = {
				type = "ssd";
				speed = 250;
				size = 120;
			};
			ram = 8;
		};
		Prestigio-Laptop = {
			cpu = {
				vendor = "intel";
				clock = 1400;
				cores = 2;
			};
			drive = {
				type = "flash";
				speed = 100;
				size = 32;
			};
			ram = 2;
		};
	};

	isLaptop = (!isNull(builtins.match ".*Laptop" device));
	smallScreen = (device == "Prestigio-Laptop");
	isShared = (device == "Prestigio-Laptop" || device == "ASUS-Laptop");
	cpu = myDevices.${device}.cpu.vendor;
	isSSD = myDevices.${device}.drive.type == "ssd";
	isHost = isSSD;
}