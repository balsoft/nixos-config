let
  findModules = dir:
    builtins.concatLists (builtins.attrValues (builtins.mapAttrs (name: type:
      if type == "regular" then
        (if name == "default.nix" then
          [ ]
        else [{
          name = builtins.elemAt (builtins.match "(.*)\\.nix" name) 0;
          value = dir + "/${name}";
        }])
      else if (builtins.readDir (dir + "/${name}")) ? "default.nix" then [{
        inherit name;
        value = dir + "/${name}";
      }] else
        findModules (dir + "/${name}")) (builtins.readDir dir)));

in builtins.listToAttrs (findModules ./.)
