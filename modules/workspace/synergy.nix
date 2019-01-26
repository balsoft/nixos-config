{pkgs, config, lib, ...}:
{
  services.synergy = if config.device == "Lenovo-Workstation" then {
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
}
