p: c: with p;
builtins.mapAttrs (name: value: writeTextFile { inherit name; text = callPackage value { iconfont = "Material Icons 10"; config = c; }; executable = true; }) 
{
  battery = ./battery.nix;
}
