p: c: with p;
builtins.mapAttrs 
(
name:
value: 
  writeTextFile 
  { 
    inherit name; 
    text = callPackage 
           value 
           { 
             iconfont = "Material Icons 11"; 
             config = c;
           };
    executable = true; 
    checkPhase = "${bash}/bin/bash -n $src || ${python3}/bin/python3 -m compileall $src"; 
  }
) 
{
  battery = ./battery.nix;
  calendar = ./calendar.nix;
  email = ./email.nix;
  wireless = ./wireless.nix;
  sound = ./sound.nix;
  temperature = ./temperature.nix;
  free = ./free.nix;
}
