{
  services.upower.enable = true;

  services.logind.lidSwitchExternalPower = "ignore";

  services.logind.extraConfig = "HandlePowerKey=suspend";
}
