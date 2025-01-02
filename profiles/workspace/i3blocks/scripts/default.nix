p: c:
with p;
let
  iconfont = "Material Icons";
  buildHaskellScript = name: script:
  stdenv.mkDerivation {
    inherit name;
    src = script;
    unpackPhase = "true";
    buildInputs = [ghc];
    buildPhase = "ghc -o $out $src";
    installPhase = "true";
    ICONFONT = iconfont;
  };
  writeScript = name: script:
  writeTextFile {
    inherit name;
    text = callPackage script {
      inherit iconfont;
      config = c;
    };
    executable = true;
    checkPhase =
      "${bash}/bin/bash -n $src || ${python3}/bin/python3 -m compileall $src";
  };
in
builtins.mapAttrs buildHaskellScript {
  free = ./free.hs;
  temperature = ./temperature.hs;
  network = ./network.hs;
} // builtins.mapAttrs writeScript {
  battery = ./battery.nix;
  brightness = ./brightness.nix;
  email = ./email.nix;
  # emacs = ./emacs.nix;
  bluetooth = ./bluetooth.nix;
  connections = ./connections.nix;
  weather = ./weather.nix;
  sound = ./sound.nix;
  music = ./music.nix;
  cpu = {...}: ''${procps}/bin/top -b -n1 -p 1 | ${gnugrep}/bin/fgrep "Cpu(s)" | ${coreutils}/bin/tail -1 | ${gawk}/bin/awk -F'id,' -v prefix="$prefix" '{ split($1, vs, ","); v=vs[length(vs)]; sub("%", "", v); printf "󰍛%s%.1f%%\n", prefix, 100 - v }' '';
  freq = {...}: ''echo $(${pkgs.bc}/bin/bc -l <<< "scale=2; `${coreutils}/bin/cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq|${coreutils}/bin/sort|${coreutils}/bin/tail -1`/1000000") GHz'';
  df = {...}: ''echo '<span font="${iconfont}"></span>' `${coreutils}/bin/df / | ${coreutils}/bin/tail -1 | ${gnugrep}/bin/grep -o '..%'`'';
  date = {...}: "${pkgs.coreutils}/bin/date +'<span font=\"${iconfont}\"></span> %a %y-%m-%d'";
  time = {...}: "${pkgs.coreutils}/bin/date +'<span font=\"${iconfont}\"></span> %T'";
  nixos = {...}: "echo -n '<span font=\"Material Icons 13\"></span>'; ${coreutils}/bin/cat /run/current-system/nixos-version | ${coreutils-full}/bin/cut -d. -f3";
  #temperature = ./temperature.nix;
  #free = ./free.nix;
}
