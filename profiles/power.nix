{
  services.upower.enable = true;

  services.logind.settings.Login.HandleLidSwitchExternalPower = "ignore";

  services.logind.settings.Login.HandlePowerKey = "suspend";
}
