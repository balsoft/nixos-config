p: c:
with p;
builtins.mapAttrs (name: value:
stdenv.mkDerivation {
  inherit name;
  src = value;
  unpackPhase = "true";
  buildInputs = [ghc];
  buildPhase = "ghc -o $out $src";
  installPhase = "true";
}) {
  free = ./free.hs;
  temperature = ./temperature.hs;
  network = ./network.hs;
} // builtins.mapAttrs (name: value:
writeTextFile {
  inherit name;
  text = callPackage value {
    iconfont = "Material Design Icons";
    config = c;
  };
  executable = true;
  checkPhase =
  "${bash}/bin/bash -n $src || ${python3}/bin/python3 -m compileall $src";
}) {
  battery = ./battery.nix;
  brightness = ./brightness.nix;
  calendar = ./calendar.nix;
  email = ./email.nix;
  connections = ./connections.nix;
  weather = ./weather.nix;
  sound = ./sound.nix;
  music = ./music.nix;
  hydra-status = ./hydra-status.nix;
  #temperature = ./temperature.nix;
  #free = ./free.nix;
}
