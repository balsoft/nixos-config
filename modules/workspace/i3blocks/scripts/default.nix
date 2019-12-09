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
  emacs = ./emacs.nix;
  connections = ./connections.nix;
  weather = ./weather.nix;
  sound = ./sound.nix;
  music = ./music.nix;
  youtrack-wage = ./youtrack-wage.nix;
  cpu = {...}: ''top -b -n1 -p 1 | fgrep "Cpu(s)" | tail -1 | awk -F'id,' -v prefix="$prefix" '{ split($1, vs, ","); v=vs[length(vs)]; sub("%", "", v); printf "%s%.1f%%\n", prefix, 100 - v }' '';
  freq = {...}: ''echo $(${pkgs.bc}/bin/bc -l <<< "scale=2; `cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq|sort|tail -1`/1000000") GHz'';
  df = {...}: ''echo '<span font="Material Icons 11"></span>' `df / | tail -1 | grep -o '..%'`'';
  date = {...}: "${pkgs.coreutils}/bin/date +'<span font=\"Material Icons 11\"></span> %a %y-%m-%d'";
  time = {...}: "${pkgs.coreutils}/bin/date +'<span font=\"Material Icons 11\"></span> %T'";
  #temperature = ./temperature.nix;
  #free = ./free.nix;
}
