{ pkgs, config, inputs, ... }:
let
  brother_printer = pkgs.linkFarm "Brother_HL-3170CDW_series" [{
    name = "share/cups/model/hl3170cdw.ppd";
    path = "${inputs.brother-hl-3170cdw-driver}/Brother_HL-3170CDW_series.ppd";
  }];
in {
  # services.printing = {
  #   enable = true;
  #   drivers = [
  #     brother_printer
  #   ];
  # };

  # hardware.printers = {
  #   ensureDefaultPrinter = "Brother_HL-3170CDW_series";
  #   ensurePrinters = [{
  #     name = "Brother_HL-3170CDW_series";
  #     deviceUri = "usb://Brother/HL-3170CDW%20series?serial=E71798K6J706416";
  #     model = "hl3170cdw.ppd";
  #   }];
  # };

  # programs.system-config-printer.enable = true;

  hardware.sane.extraBackends = [ pkgs.epkowa pkgs.utsushi ];
  services.udev.packages = [ pkgs.epkowa pkgs.utsushi ];
  hardware.sane.enable = true;
  services.saned.enable = true;

  environment.systemPackages = [ pkgs.simple-scan ];
}
