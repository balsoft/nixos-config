{ pkgs, config, lib, ... }: {
  services.synergy = if config.device == "AMD-Workstation" then {
    server.enable = true;
    server.configFile = pkgs.writeTextFile {
      name = "synergy.conf";
      text = ''
        section: screens
          AMD-Workstation
        	ASUS-Laptop:
        	HP-Laptop:
        end
        section: links
        	ASUS-Laptop:
        		right = AMD-Workstation
        	HP-Laptop:
        		left = AMD-Workstation
        end
        section: options
            keystroke(super+alt+left) = switchInDirection(left)
            keystroke(super+alt+right) = switchInDirection(right)
        end
      '';
    };
  } else {
    client.enable = true;
    client.serverAddress = "AMD-Workstation";
  };
}
