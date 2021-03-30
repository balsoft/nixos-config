{ pkgs, config, lib, inputs, ... }:

with rec { inherit (config) device deviceSpecific; };
with deviceSpecific; {
  hardware.enableRedistributableFirmware = true; # For some unfree drivers

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true; # For steam
  hardware.opengl.package = pkgs.mesa_drivers;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluezFull;

  hardware.sane.enable = true;

  services.saned.enable = true;

  services.logind.lidSwitchExternalPower = "ignore";

  services.logind.extraConfig = "HandlePowerKey=suspend";

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.callPackage ({ ... }:
      let orig = pkgs.pulseaudioFull;
      in pkgs.stdenv.mkDerivation rec {
        pname = "pulseaudio";
        version = "1.1.1"; # For compatibility
        outputs = [ "out" "dev" ];
        src = inputs.pulseaudio;
        nativeBuildInputs = with pkgs; [
          pkg-config
          makeWrapper
          perlPackages.perl
          perlPackages.XMLParser

          meson
          ninja
        ];
        inherit (orig)
          propagatedBuildInputs configureFlags installFlags postInstall
          preFixup;
        buildInputs = orig.buildInputs
          ++ (with pkgs; [ libexecinfo tdb orc check ]);
        preConfigure = ''
          echo "$version" > .tarball-version
        '';
        mesonFlags = [
          "-Dgtk=disabled"
          "-Delogind=disabled"
          "-Dvalgrind=disabled"
          "-Dtcpwrap=disabled"
          "-Dbluez5-gstreamer=disabled"
          "-Dgstreamer=disabled"
          "-Dsystemduserunitdir=lib/systemd/user"
          "-Dudevrulesdir=lib/udev/rules.d"
        ];
      }) { };
    support32Bit = true;
    extraConfig = ''
      load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
      load-module module-bluetooth-policy auto_switch=2
    '';
  };
}
