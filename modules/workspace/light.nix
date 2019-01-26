{config, pkgs, lib, ...}:
{
  programs.light.enable = config.deviceSpecific.isLaptop;
  services.actkbd = {
    enable = config.deviceSpecific.isLaptop;
    bindings = map (x: x // {
      events = [ "key" ];
      attributes = [ "exec" ];
    }) ((if config.device == "ASUS-Laptop" then [
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
}
