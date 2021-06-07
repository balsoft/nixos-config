{ pkgs, config, ... }:
{
  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint ];
  };

  hardware.sane.enable = true;
  services.saned.enable = true;

  environment.systemPackages = [ pkgs.gnome3.simple-scan ];
}
