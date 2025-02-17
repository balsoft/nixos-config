{ pkgs, config, lib, ... }: {
  services.pulseaudio.enable = lib.mkForce false;

  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };
}
