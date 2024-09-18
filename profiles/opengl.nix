{ pkgs, ... }: {
  hardware.graphics.enable = true;
  # hardware.opengl.driSupport = true;
  hardware.graphics.enable32Bit = true; # For steam
  hardware.graphics.package = pkgs.mesa.drivers;
}
