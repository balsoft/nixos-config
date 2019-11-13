{ config, pkgs, lib, ... }: {
  programs.light.enable = config.deviceSpecific.isLaptop;
  services.actkbd = {
    enable = config.deviceSpecific.isLaptop;
    bindings = map (x:
      x // {
        events = [ "key" ];
        attributes = [ "exec" ];
      }) ((if config.device == "ThinkPad-Laptop" then
        let kbd_backlight = "/sys/class/leds/tpacpi::kbd_backlight/brightness";
        in [{
          keys = [ 374 ];
          command =
            "echo $(((1 + `cat '${kbd_backlight}'`) % 3)) > '${kbd_backlight}' ";
        }]
      else
        [ ]) ++ [
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
}
