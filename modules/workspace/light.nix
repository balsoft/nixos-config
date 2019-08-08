{ config, pkgs, lib, ... }: {
  programs.light.enable = config.deviceSpecific.isLaptop;
  services.actkbd = {
    enable = config.deviceSpecific.isLaptop;
    bindings = map (x:
    x // {
      events = ["key"];
      attributes = ["exec"];
    }) ((if config.device == "ASUS-Laptop" then [
      {
        keys = [229];
        command =
          "expr -1 + `cat '/sys/class/leds/asus::kbd_backlight/brightness'` > '/sys/class/leds/asus::kbd_backlight/brightness'";
      }
      {
        keys = [230];
        command =
          "expr 1 + `cat '/sys/class/leds/asus::kbd_backlight/brightness'` > '/sys/class/leds/asus::kbd_backlight/brightness'";
      }
      {
        keys = [560];
        command = toString (pkgs.stdenv.mkDerivation {
          name = "als-script";
          src = ./als-script.hs;
          buildInputs = [pkgs.ghc];
          buildPhase = "ghc $src -o $out";
          unpackPhase = "true";
          installPhase = "true";
        });
      }
    ] else
      []) ++ [
      {
        keys = [225];
        command = "${pkgs.light}/bin/light -A 10";
      }
      {
        keys = [224];
        command = "${pkgs.light}/bin/light -U 10";
      }
      {
        keys = [431];
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
}
